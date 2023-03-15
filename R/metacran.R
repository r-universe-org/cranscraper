#' Scrape Metacran versions
#'
#' Helper function to check the state of metacran and compare to CRAN.
#'
#' @export
check_metacran <- function(){
  # Current CRAN state
  db <- available.packages(repos = 'https://cran.r-project.org')
  db <- as.data.frame(db[,c("Package", "Version")])
  db <- db[order(db$Package, method = 'radix'),]
  db$Metacran = NA_character_

  # Scrape versions from Metacran
  pool <- curl::new_pool(multiplex = FALSE)
  completed <- 0
  fetch_version <- function(i){
    url <- sprintf('https://raw.githubusercontent.com/cran/%s/HEAD/DESCRIPTION', db$Package[i])
    curl::curl_fetch_multi(url, done = function(res){
      if(res$status_code == 200){
        db$Metacran[i] <<- parse_description_version(res$content)
        completed <<- completed + 1
        if(completed %% 100 == 0)
          message(sprintf("Completed %d of %d", completed, nrow(db)))
      } else if(res$status_code %in% c(403, 429)){
        message("Possibly hitting GH limit, waiting for a few seconds before retrying...")
        Sys.sleep(30)
        fetch_version(i)
      } else {
        message(sprintf("HTTP %d: %s", res$status_code, res$url))
      }
    }, fail = stop, pool = pool)
  }
  lapply(seq_len(nrow(db)), fetch_version)
  curl::multi_run(pool = pool)
  db
}

# Get 'Version' field from DESCRIPTION
parse_description_version <- function(buf){
  con <- rawConnection(buf)
  on.exit(close(con))
  desc <- read.dcf(con)
  trimws(unname(desc[,'Version']))
}
