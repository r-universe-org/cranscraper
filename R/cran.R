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
  input <- paste(packages$BugReports, packages$URL)
  pattern <- 'https?://(github.com|gitlab.com|bitbucket.org)/[A-Za-z0-9_-]+/[A-Za-z0-9_.-]+'
  m <- regexpr(pattern, input, ignore.case = TRUE)
  rows <- !is.na(m) & m > -1
  urls <- regmatches(input, m)
  packages[rows,'Git'] <- sub("\\.git$", "", sub("^http://", "https://", tolower(urls)))
  return(packages)
}

#' @export
#' @rdname cran
cran_registry_with_status <- function(){
  packages <- cran_registry()
  packages <- packages[!is.na(packages$Git),]
  statusvec <- rep(0, nrow(packages))
  pool <- curl::new_pool()
  lapply(seq_along(packages$Git), function(i){
    k <- i
    pkg <- as.list(packages[k,])
    desc_url <- paste0(pkg$Git, '/raw/master/DESCRIPTION')
    curl::curl_fetch_multi(desc_url, done = function(res){
      statusvec[k] <<- res$status
      if(res$status != 200)
        message("HTTP error: ", pkg$Package, " from ", pkg$Git,  ": ", res$status)
    }, fail = function(e){
      message("Failure for ", pkg$Package, ": ", e$message)
    }, pool = pool)
  })
  curl::multi_run(pool = pool)
  packages$status <- statusvec
  return(packages)
}

#' @export
#' @rdname cran
cran_registry_update_json <- function(){
  registry <- cran_registry_with_status()
  df <- data.frame(
    package = registry$Package,
    maintainer = registry$Maintainer,
    git = registry$Git,
    available = (registry$status == 200),
    owner = slugify_owner(registry$Git),
    stringsAsFactors = FALSE)

  # Split by owner
  paths <- vapply(split(df, df$owner), function(userdata){
    path <- paste0(userdata$owner[1], '.json')
    userdata$owner <- NULL
    jsonlite::write_json(userdata, path = path, pretty = TRUE)
    return(path)
  }, character(1))
  gert::git_add(paths)
  if(nrow(gert::git_status(staged = TRUE)) == 0){
    message("No changes in registry")
  } else {
    msg <- paste("Registry update at:", Sys.time())
    gert::git_commit(msg, author = "CRAN <cran@nowhere.com>")
    gert::git_push(verbose = TRUE)
  }
}

read_description <- function(desc_url){
  con <- curl::curl(desc_url)
  on.exit(close(con))
  read.dcf(con)
}

slugify_owner <- function(url){
  owner <- dirname(url)
  owner <- gsub("/", "_", gsub("^https?://", "", owner), fixed = TRUE)
  sub("github.com_", "", owner)
}
