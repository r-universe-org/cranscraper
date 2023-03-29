# For packages that do not have any git URL in their DESCRIPTION, we try the URL
# under the GitHub account from the maintainer, if known.
# If the package and version seem OK, we use this in lieu of a Git URL.
# Then proceed as usual.
guess_repo_by_maintainer <- function(packages){
  packages$hash <- openssl::sha1(tolower(sub("^.*<(.*)>$", "\\1", packages$Maintainer)))
  maintainerdb <- read.csv('maintainers.csv')
  packages <- left_join(packages, maintainerdb, by = 'hash')
  unknowns <- which(is.na(packages$Git) & !is.na(packages$Version) & !is.na(packages$login))
  message(sprintf("Checking %d packages with unknown repo but known maintainer", length(unknowns)))
  pool <- curl::new_pool(multiplex = FALSE)
  lapply(unknowns, function(i){
    pkg <- packages$Package[i]
    login <- packages$login[i]
    desc_url <- sprintf('https://raw.githubusercontent.com/%s/%s/HEAD/DESCRIPTION', login, pkg)
    curl::multi_add(make_handle(desc_url), done = function(res){
      if(res$status_code == 429){
        message("Got HTTP 429, pausing for a bit...")
        Sys.sleep(15)
      } else if(res$status_code == 200){
        try({
          pkginfo <- parse_description_raw(res$content)
          found_repo <- get_real_url(sprintf('https://github.com/%s/%s', login, pkg), res$url)
          if(pkginfo$Package == pkg){
            repo_version <- as.package_version(pkginfo$Version)
            cran_version <- as.package_version(packages$Version[i])
            if(repo_version >= cran_version){
              message("Found new git repo at: ", found_repo)
              packages$Git[i] <<- found_repo
            } else {
              message("Found repo seems outdated: ", found_repo)
            }
          } else {
            message("Package name mismatch for: ", found_repo)
          }
        })
      } else if(res$status_code != 404) {
        message(sprintf("HTTP %d: %s", res$status_code, res$url))
      }
    }, fail = message, pool = pool)
  })
  curl::multi_run(pool = pool)

  # Print some stats
  total_found  <- sum(!is.na(packages$Git[unknowns]))
  message(sprintf('Discovered %d of %d unknown packages in the GitHub home of the maintainer!', total_found, length(unknowns)))
  packages
}


parse_description_raw <- function(buf){
  con <- rawConnection(buf)
  on.exit(close(con))
  desc <- read.dcf(con)
  lapply(as.data.frame(desc), trimws)
}
