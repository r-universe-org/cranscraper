#' Update maintainer DB
#'
#' Updates maintainer.csv file
#'
#' @export
update_maintainers_csv <- function(){
  token <- ghapps::gh_app_token('r-universe') #enterprise app has 15k API limit
  limits <- gh::gh_rate_limit(.token = token)
  message(sprintf("Rate limit: remaining %d (of %d)", limits$remaining, limits$limit))
  cran <- cran_registry()
  archive <- archived_registry()
  packages <- data.frame(maintainer = c(cran$Maintainer, archive$Maintainer), package = c(cran$Package, archive$Package))
  packages <- packages[sample(seq_len(nrow(packages))),] #shuffle
  packages$email <- tolower(sub("^.*<(.*)>$", "\\1", packages$maintainer))
  db <- packages[!duplicated(packages$email), c("package", "email")]
  db$hash <- openssl::sha1(db$email)
  old <- read.csv("maintainers.csv")
  db <- left_join(db, old, by = 'hash')
  pool <- curl::new_pool(multiplex = FALSE, host_con = 2)
  total <- length(db$login)
  done <- 0
  lapply(seq_along(db$login), function(i){
    pkg <- db$package[i]
    email <- db$email[i]
    url <- sprintf('https://api.github.com/repos/cran/%s/commits/HEAD', pkg)
    req <- curl::new_handle(url = url, httpheader = paste("Authorization: token", token))
    curl::multi_add(req, done = function(res){
      done <<- done + 1
      if(done %% 100 == 0){
        message(sprintf("Done: %d of %d", done, total))
      }
      if(res$status_code == 200){
        json <- jsonlite::parse_json(rawToChar(res$content))
        commit_email <- tolower(json$commit$author$email)
        if(identical(commit_email, email)){
          old_login <- db$login[i]
          if(length(json$author$login)){
            db$login[i] <<- json$author$login
          } else if(!is.na(old_login)){
            message(sprintf("Testing if %s still exists", old_login))
            usertest <- curl::curl_fetch_memory(paste0('https://github.com/', old_login))
            if(usertest$status_code == 404){
              db$login[i] <<- NA_character_ #force remove if user no longer exists
            }
          }
        } else {
          message(sprintf("Metacran email mismatch for %s: %s / %s", pkg, commit_email, email))
        }
      } else {
        message(sprintf('HTTP %d (%s)', res$status_code, res$url))
      }
      if(res$status_code == 403){
        message("Pausing for a few seconds...")
        Sys.sleep(10)
      }
    }, fail = message, pool = pool)
  })
  curl::multi_run(pool = pool)
  db <- db[!is.na(db$login),]
  db <- db[order(paste(tolower(db$login), db$hash), method = 'radix'), c("hash", "login")]
  write.csv(db, 'maintainers.csv', row.names = FALSE, quote = FALSE)

  # Commit update
  gert::git_add('maintainers.csv')
  if(nrow(gert::git_status(staged = TRUE)) == 0){
    message("No changes in 'maintainers.csv'")
  } else {
    msg <- paste("Maintainer.csv update at:", Sys.time())
    gert::git_commit(msg, author = "r-universe[bot] <74155986+r-universe[bot]@users.noreply.github.com>")
    gert::git_push(verbose = TRUE)
  }
}
