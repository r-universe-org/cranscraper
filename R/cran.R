#' CRAN tools
#'
#' Scraping git repositories for CRAN packages
#'
#' @export
#' @rdname cran
cran_registry <- function(){
  tmp <- tempfile()
  on.exit(unlink(tmp))
  curl::curl_download('https://cloud.r-project.org/web/packages/packages.rds', destfile = tmp)
  packages <- as.data.frame(readRDS(tmp), stringsAsFactors = FALSE)
  # CRAN ships two versions of recommended packages: the 'base' and 'cran' version
  packages <- packages[!duplicated(packages$Package),]
  packages$Git <- find_git_url(packages)
  return(packages)
}

find_git_url <- function(packages){
  input <- paste(packages$BugReports, packages$URL)
  input <- normalize_github_urls(input)
  input <- paste(input, replace_rforge_urls(input)) #Prefer GitHub URL over r-forge guess
  output <- rep(NA_character_, length(input))
  pattern <- 'https?://(github.com|gitlab.com|bitbucket.org)/[A-Za-z0-9_-]+/[A-Za-z0-9_.-]+'
  m <- regexpr(pattern, input, ignore.case = TRUE)
  rows <- !is.na(m) & m > -1
  urls <- regmatches(input, m)
  output[rows] <- sub("\\.git$", "", sub("^http://", "https://", tolower(urls)))
  return(output)
}

first_maintainer <- function(x){
  vapply(x, function(x){
    ps <- utils::as.person(x)
    ifelse(length(ps) == 1, x, as.character(ps[1]))
  }, character(1))
}

#' @export
#' @rdname cran
#' @param full_reset do not keep data from current `crantogit.csv` in case
#' of errors.
cran_registry_with_status <- function(full_reset = FALSE){
  cran <- cran_registry()
  bioc <- bioc_registry()
  packages <- data.frame(
    Package = c(cran$Package, bioc$Package),
    Maintainer = first_maintainer(c(cran$Maintainer, bioc$Maintainer)),
    Git = c(cran$Git, bioc$Git),
    Registry = rep(c(NA, 'bioc'), c(length(cran$Package), length(bioc$Package))),
    stringsAsFactors = FALSE
  )
  packages$Git[grepl("https://github.com/cran/", packages$Git, fixed = TRUE)] <- NA # No mirror urls
  packages <- packages[!is.na(packages$Git),]

  # Setup scraper outputs
  if(isTRUE(full_reset)){
    foundvec <- rep(FALSE, nrow(packages))
    subdirvec <- rep(NA_character_, nrow(packages))
    realurlvec <- packages$Git
  } else {
    current <- utils::read.csv('crantogit.csv', na.strings = "")
    pos <- match(packages$Package, current$package)
    foundvec <- !is.na(pos)
    subdirvec <- current$subdir[pos]
    realurlvec <- ifelse(foundvec, current$url[pos], packages$Git)
  }
  pool <- curl::new_pool()
  lapply(sample(seq_along(packages$Git)), function(i){
    k <- i
    pkg <- as.list(packages[k,])
    package <- pkg$Package
    desc_url <- paste0(pkg$Git, '/raw/HEAD/DESCRIPTION')
    curl::curl_fetch_multi(desc_url, handle = myhandle(), done = function(res){
      if(res$status == 200 && test_package_match(res$content, package)){
        foundvec[k] <<- TRUE
        realurlvec[k] <<- get_real_url(pkg$Git, res$url)
      } else {
        # If 404, the package seems removed
        # In case of other network errors, just do nothing (keeps the current values)
        if(isTRUE(foundvec[k]) && res$status == 404 && is.na(subdirvec[k])){
          message("REMOVING package: ", package)
          foundvec[k] <<- FALSE
          realurlvec[k] <<- pkg$Git
        }
        message("HTTP or description error: ", package, " from ", pkg$Git,  ": ", res$status)
        alt_subdirs <- sprintf(c("pkg", "r", "%s", "pkg/%s"), package)
        if(package == 'duckdb') alt_subdirs <- 'tools/rpkg'
        lapply(alt_subdirs, function(alt_dir){
          alt_url <- sprintf('%s/raw/HEAD/%s/DESCRIPTION', pkg$Git, alt_dir)
          curl::curl_fetch_multi(alt_url, handle = myhandle(), done = function(res2){
            if(res2$status == 200 && test_package_match(res2$content, package)){
              message("Found subdir for: ", package, " in ", alt_dir)
              foundvec[k] <<- TRUE
              subdirvec[k] <<- alt_dir
              realurlvec[k] <<- get_real_url(pkg$Git, res2$url)
            } else if(isTRUE(foundvec[k]) && res2$status == 404 && identical(subdirvec[k], alt_dir)){
              message("REMOVING package: ", package, " with subdir: ", subdirvec[k])
              foundvec[k] <<- FALSE #package no longer there?
              realurlvec[k] <<- pkg$Git
              subdirvec[k] <<- NA_character_
            }
          }, pool = pool)
        })
      }
    }, fail = function(msg){
      message("Failure for ", package, ": ", msg)
    }, pool = pool)
  })
  curl::multi_run(pool = pool)
  packages$found <- foundvec
  packages$subdir <- subdirvec
  packages$Git <- realurlvec
  return(packages)
}

test_package_match <- function(buf, package){
  tryCatch({
    realname <- parse_description_package(buf)
    out <- identical(realname, package)
    if(!out){
      message(sprintf("Package name from DESCRIPTION '%s' does not match package '%s'", realname, package))
    }
    out
  }, error = function(e){
    message(sprintf("Failed to parse DESCRIPTION for package '%s' %s", package, e$message))
    FALSE
  })
}

parse_description_package <- function(buf){
  con <- rawConnection(buf)
  on.exit(close(con))
  desc <- read.dcf(con)
  if('Config/runiverse/noindex' %in% colnames(desc)){
    stop("Package has Config/runiverse/noindex field")
  }
  trimws(unname(desc[,'Package']))
}

# This is to detect redirects for moved GitHub repositories
get_real_url <- function(git_url, description_url){
  # Workaround for bitbucket redirecting to a login page
  if(grepl("atlassian.com", description_url))
    return(git_url)
  domain <- sub("(.*://[^/]+)/.*", '\\1', git_url)
  new_repo <- sub(".*://[^/]+/([^/]+/[^/]+).*", '\\1', description_url)
  updated_git_url <- tolower(paste0(domain, '/', new_repo))
  if(updated_git_url != tolower(git_url)){
    message(sprintf("Repo moved from %s to %s", git_url, updated_git_url))
  }
  return(updated_git_url)
}

#' @export
#' @rdname cran
cran_registry_update_json <- function(){
  registry <- cran_registry_with_status()
  df <- data.frame(
    package = registry$Package,
    maintainer = registry$Maintainer,
    url = registry$Git,
    subdir = registry$subdir,
    available = registry$found,
    registry = registry$Registry,
    owner = slugify_owner(registry$Git),
    stringsAsFactors = FALSE)

  # Santiy check and save new CSV
  csvdata <- df[df$available, c('package', 'url', 'subdir', 'registry')]
  if(file.exists('crantogit.csv')){
    oldcount <- nrow(read.csv('crantogit.csv'))
    newcount <- nrow(csvdata)
    added <- newcount - oldcount
    message(sprintf("Updating crantogit.csv: %d rows were %s!", abs(added), ifelse(added < 0, "DELETED", "ADDED")))
    if(added < -100){
      stop("This seems wrong. Aborting.")
    }
  }
  utils::write.csv(csvdata, file = 'crantogit.csv', quote = FALSE, row.names = FALSE, na = "")
  gert::git_add('crantogit.csv')

  # Split by owner
  paths <- vapply(split(df, df$owner), function(userdata){
    path <- paste0(userdata$owner[1], '.json')
    userdata$owner <- NULL
    if(identical(path, 'bioconductor.json')){
      userdata <- set_bioc_branch(userdata)
    }
    jsonlite::write_json(userdata, path = path, pretty = TRUE)
    return(path)
  }, character(1))

  # A dummy packages.json that is used as fallback for empty universes
  jsonlite::write_json(list(), path = 'packages.json', pretty = TRUE)
  paths <- c(paths, 'packages.json')

  # Delete file that no longer exist
  oldfiles <- list.files(pattern="\\.json$")
  removed <- oldfiles[!(oldfiles %in% paths)]
  unlink(removed)
  if(length(removed) > 100){
    stop("More than 100 universes were removed. This doesn't seem right.")
  }
  gert::git_add(c(removed, paths))
  if(nrow(gert::git_status(staged = TRUE)) == 0){
    message("No changes in registry")
  } else {
    msg <- paste("Registry update at:", Sys.time())
    gert::git_commit(msg, author = "r-universe[bot] <74155986+r-universe[bot]@users.noreply.github.com>")
    gert::git_push(verbose = TRUE)
  }
}

read_description <- function(desc_url){
  con <- curl::curl(desc_url)
  on.exit(close(con))
  read.dcf(con)
}

slugify_owner <- function(url){
  owner <- sub('.*://([a-z]+).*/([^/]*)/.*', '\\1-\\2', url)
  sub('github-', '', owner)
}

replace_rforge_urls <- function(input){
  input <- gsub("www.rforge.net", "github.com/s-u", input, fixed = TRUE)
  input <- gsub('r-forge\\.r-project\\.org/projects/', 'github.com/r-forge/', input, ignore.case = TRUE)
  input <- gsub("https?://lists\\.r-forge\\.r-project\\.org", "", input, ignore.case = TRUE) # Remove URLS to mailing list
  gsub("https?://([A-Za-z0-9_.-]+)\\.r-forge\\.r-project\\.org", 'https://github.com/r-forge/\\1', input, ignore.case = TRUE)
}

normalize_github_urls <- function(input){
  input <- gsub('//www.github.com', '//github.com', input, fixed = TRUE)
  gsub("https?://([A-Za-z0-9-]+)\\.github\\.io/([A-Za-z0-9_.-]+)", 'https://github.com/\\1/\\2', input)
}

myhandle <- function(){
  curl::new_handle(useragent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36')
}
