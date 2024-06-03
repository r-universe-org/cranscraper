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
  winonly <- which(packages$OS_type == 'windows')
  if(length(winonly)){
    # Do not include Windows-only packages because they are trouble on our Linux servers
    packages <- packages[-winonly,]
  }

  # CRAN ships two versions of recommended packages: the 'base' and 'cran' version
  packages <- packages[!duplicated(packages$Package),]
  packages$Git <- find_git_url(packages)
  return(packages)
}

find_git_url <- function(packages){
  input <- paste(packages$BugReports, packages$URL)
  input <- normalize_github_urls(input)
  input <- paste(input, replace_rforge_urls(input)) #Prefer GitHub URL over r-forge guess
  input <- gsub('https://github.com/cran/', '', input, fixed = TRUE) # No mirror URLS here
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
  archived <- archived_registry()
  packages <- data.frame(
    Package = c(cran$Package, bioc$Package, archived$Package),
    Version = c(cran$Version, bioc$Version, archived$Version),
    Maintainer = first_maintainer(c(cran$Maintainer, bioc$Maintainer, archived$Maintainer)),
    Git = c(cran$Git, bioc$Git, archived$Git),
    Registry = rep(c(NA, 'bioc', 'archived'), c(nrow(cran), nrow(bioc), nrow(archived))),
    stringsAsFactors = FALSE
  )
  packages <- packages[!duplicated(packages$Package),]
  packages <- guess_repo_by_maintainer(packages)

  # Default to keep current values
  current <- utils::read.csv('crantogit.csv', na.strings = "")
  names(current)[1] <- 'Package'

  # Merge old values into new DB
  packages <- left_join(packages, current, by = 'Package')
  packages$found <- !is.na(packages$url)

  # If no new candidate url, reverify current url
  packages$Git[is.na(packages$Git)] <- packages$url[is.na(packages$Git)]

  # Setup scraper outputs
  packages <- packages[order(tolower(packages$Package), method = 'radix'),]
  pool <- curl::new_pool(multiplex = FALSE) # try to fix github 403 errors
  message(sprintf("=== Going to try %d DESCRIPTION urls...", sum(!is.na(packages$Git))))
  lapply(sample(which(!is.na(packages$Git))), function(i){
    k <- i
    pkg <- as.list(packages[k,])
    package <- pkg$Package
    desc_url <- paste0(pkg$Git, '/raw/HEAD/DESCRIPTION')
    curl::multi_add(make_handle(desc_url), done = function(res){
      desc <- parse_description_raw(res$content)
      if(has_noindex(desc)){
        res$status <- 404
      }
      if(res$status == 200 && test_package_match(desc, package)){
        packages$found[k] <<- TRUE
        packages$url[k] <<- get_real_url(pkg$Git, res$url)
        packages$subdir[k] <<- NA_character_
      } else if(res$status_code == 429){
        message("Got HTTP 429, pausing for a bit...")
        Sys.sleep(15)
      } else {
        # If 404, the package seems removed
        # In case of other network errors, just do nothing (keeps the current values)
        if(isTRUE(packages$found[k]) && res$status == 404 && is.na(packages$subdir[k])){
          message("REMOVING package: ", package)
          packages$found[k] <<- FALSE
          packages$url[k] <<- pkg$Git
        }
        message("HTTP or description error: ", package, " from ", pkg$Git,  ": ", res$status)
        alt_subdirs <- sprintf(c("pkg", "r", "%s", "pkg/%s"), package)
        if(package == 'duckdb') alt_subdirs <- 'tools/rpkg'
        lapply(alt_subdirs, function(alt_dir){
          alt_url <- sprintf('%s/raw/HEAD/%s/DESCRIPTION', pkg$Git, alt_dir)
          curl::multi_add(make_handle(alt_url), done = function(res2){
            desc2 <- parse_description_raw(res2$content)
            if(has_noindex(desc2)){
              res2$status <- 404
            }
            if(res2$status == 200 && test_package_match(desc2, package)){
              message("Found subdir for: ", package, " in ", alt_dir)
              packages$found[k] <<- TRUE
              packages$subdir[k] <<- alt_dir
              packages$url[k] <<- get_real_url(pkg$Git, res2$url)
            } else if(isTRUE(packages$found[k]) && res2$status == 404 && identical(packages$subdir[k], alt_dir)){
              message("REMOVING package: ", package, " with subdir: ", packages$subdir[k])
              packages$found[k] <<- FALSE #package no longer there?
              packages$url[k] <<- pkg$Git
              packages$subdir[k] <<- NA_character_
            }
          }, pool = pool)
        })
      }
    }, fail = function(msg){
      message("Failure for ", package, ": ", msg)
    }, pool = pool)
  })
  curl::multi_run(pool = pool)

  # This adds {available: false} packages in registry for detected broken URLs
  # We could just remove this if we don't care about registering these
  # packages$url[is.na(packages$url)] <- packages$Git[is.na(packages$url)]

  # Owner is either git user or maintainer
  # Set github_only=TRUE to disable non-github universes
  packages$owner <- slugify_owner(packages$url, github_only = TRUE)
  packages$owner[is.na(packages$owner)] <- tolower(packages$login[is.na(packages$owner)])
  return(packages)
}

test_package_match <- function(desc, package){
  tryCatch({
    realname <- trimws(unname(desc[,'Package']))
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

parse_description_raw <- function(buf){
  con <- rawConnection(buf)
  on.exit(close(con))
  read.dcf(con)
}

has_noindex <- function(desc){
  return('Config/runiverse/noindex' %in% colnames(desc))
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
    url = registry$url,
    subdir = registry$subdir,
    available = registry$found,
    registry = registry$Registry,
    owner = registry$owner,
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

  # bioc is now a single universe (May 2024)
  df$owner[df$registry == 'bioc'] <- 'bioc'

  # Store full universe owner map
  update_universes_csv(df)

  # Store unknowns. Some of these do have a known maintainer
  #unknowns <- df[is.na(df$url), c('package', 'registry', 'owner')]
  #unknowns$registry[is.na(unknowns$registry) | unknowns$registry == 'archived'] <- 'cran'
  #utils::write.csv(unknowns, file = 'unknown.csv', quote = FALSE, row.names = FALSE, na = "")
  #gert::git_add('unknown.csv')

  # Add CRAN mirrors for remaining packages
  # NB: Right now only packages with an 'owner' are actually used in json
  use_mirror <- is.na(df$url) & !is.na(df$owner)
  mirror_server <- ifelse(is.na(df$registry) | df$registry == 'archived', 'https://github.com/cran/', 'https://git.bioconductor.org/packages/')
  df$url[use_mirror] <- paste0(mirror_server, df$package)[use_mirror]
  df$available[use_mirror] <- NA

  # Print some summary
  message(sprintf('SUMMARY: Found a total of %d git urls (of which %d using the cran/bioc mirror)', sum(!is.na(df$url)), sum(use_mirror)))

  # Split by owner
  df <- df[!is.na(df$url),]
  paths <- vapply(split(df, df$owner), function(userdata){
    path <- paste0(userdata$owner[1], '.json')
    userdata$owner <- NULL
    userdata$available <- NULL
    if(identical(path, 'bioconductor.json')){
      userdata <- set_bioc_branch(userdata)
    }
    jsonlite::write_json(userdata, path = path, pretty = TRUE)
    return(path)
  }, character(1))

  # A dummy packages.json that is used as fallback for empty universes
  jsonlite::write_json(list(), path = 'packages.json', pretty = TRUE)

  # Index all repos
  message("Generating index.json")
  ownerlist <- as.list(table(sort(df$owner, method = 'radix')))
  jsonlite::write_json(ownerlist, path = 'index.json', auto_unbox = TRUE, pretty = TRUE)
  paths <- c(paths, 'packages.json', 'index.json')

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

#' @export
#' @rdname cran
update_crantogit_csv <- function(){
  update_archived_csv()
  gert::git_add('archived.csv')
  cran_registry_update_json()
}

update_universes_csv <- function(universes){
  universes$registry[is.na(universes$registry) | universes$registry == 'archived'] <- 'cran'
  no_owner <- is.na(universes$owner)
  universes$owner[no_owner] <- universes$registry[no_owner]
  universes <- universes[, c('package', 'owner')]
  # Make rOpenSci primary first if exists
  roregistry <- jsonlite::fromJSON('https://ropensci.github.io/roregistry/packages.json')
  universes <- rbind(data.frame(package = roregistry$package, owner = 'ropensci'), universes)
  universes <- universes[!duplicated(universes$package),] # ropensci first gets preference
  universes <- universes[order(tolower(universes$package), method = 'radix'),] #after that, re-order alphabetically
  utils::write.csv(universes, file = 'universes.csv', quote = FALSE, row.names = FALSE, na = "")
  gert::git_add('universes.csv')
}

read_description <- function(desc_url){
  con <- url(desc_url, encoding = 'UTF-8')
  on.exit(close(con))
  read.dcf(con)
}

slugify_owner <- function(url, github_only = FALSE){
  owner <- sub('.*://([a-z]+).*/([^/]*)/.*', '\\1-\\2', url)
  if(isTRUE(github_only))
    owner[!grepl('^github-', owner)] <- NA
  sub('github-', '', owner)
}

replace_rforge_urls <- function(input){
  input <- gsub("www.rforge.net", "github.com/s-u", input, fixed = TRUE)
  input <- gsub('r-forge\\.r-project\\.org/projects/', 'github.com/r-forge/', input, ignore.case = TRUE)
  input <- gsub('r-forge\\.r-project\\.org/scm/[^ ]*\\?root=(\\S*)', 'github.com/r-forge/\\1', input, ignore.case = TRUE)
  input <- gsub("https?://lists\\.r-forge\\.r-project\\.org", "", input, ignore.case = TRUE) # Remove URLS to mailing list
  gsub("https?://([A-Za-z0-9_.-]+)\\.r-forge\\.r-project\\.org", 'https://github.com/r-forge/\\1', input, ignore.case = TRUE)
}

normalize_github_urls <- function(input){
  input <- gsub('//www.github.com', '//github.com', input, fixed = TRUE)
  input <- gsub("https?://([A-Za-z0-9-]+)\\.github\\.io/([A-Za-z0-9_.-]+)", 'https://github.com/\\1/\\2', input)
  sub("https://([A-Za-z0-9-]+)\\.r-universe.dev/([A-Za-z0-9_.-]+)", 'https://github.com/\\1/\\2', input)
}

make_handle <- function(desc_url){
  handle <- curl::new_handle(url = desc_url, useragent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36')
  if(grepl('github.com', desc_url, fixed = TRUE)){
    token <- Sys.getenv('GITHUB_TOKEN')
    if(nchar(token)){
      curl::handle_setheaders(handle, Authorization = paste('token', token))
    }
  }
  if(grepl('gitlab.com', desc_url, fixed = TRUE)){
    token <- Sys.getenv('GITLAB_TOKEN')
    if(nchar(token)){
      # Not sure this works, Gitlab still gives 403 sometimes
      curl::handle_setheaders(handle, Authorization = paste('Bearer', token))
    }
  }
  handle
}

# Dedupe y first to prevent returning multiple matches
# Result should have same number of rows as x
left_join <- function(x, y, by){
  if(anyDuplicated(y[[by]])){
    y <- y[!duplicated(y[[by]]),]
  }
  merge(x, y, by = by, all.x = TRUE)
}
