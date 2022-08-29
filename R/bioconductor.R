bioc_registry <- function(){
  # Bioc does not seem to have a packages.rds containing URL and BugReports
  # Bioc registry version can be overridden with R_BIOC_VERSION
  # bioc_version <- as.character(tools:::.BioC_version_associated_with_R_version())
  # Use devel branch of registry, to get latest package set and metadata
  yml <- yaml::read_yaml("https://bioconductor.org/config.yaml")
  bioc_version <- yml$devel_version
  bioc <- jsonlite::read_json(sprintf('https://bioconductor.org/packages/json/%s/bioc/packages.json', bioc_version))
  names(bioc) <- NULL
  packages <- jsonlite:::simplify(bioc)
  stopifnot(is.data.frame(packages), nrow(packages) > 1000)
  packages$Git <- find_git_url(packages)
  return(packages)
}

set_bioc_branch <- function(registry){
  bioc_version <- as.character(tools:::.BioC_version_associated_with_R_version())
  branchname <- paste0("RELEASE_", gsub(".", "_", bioc_version, fixed = TRUE))
  hasrelease <- remote_heads_many(registry$url, rep(branchname, nrow(registry)))
  registry$branch <- ifelse(nchar(hasrelease) > 0, branchname, NA_character_)
  registry
}

## Below copied form 'sync' package
parse_raw_gitpack <- function(buf){
  con <- rawConnection(buf)
  on.exit(close(con))
  txt <- readLines(con, warn = FALSE)
  stopifnot(grepl('^[0-9a-f]{4}#', txt[1]))
  stopifnot(grepl('service=', txt[1]))
  stopifnot(utils::tail(txt, 1) == '0000')
  refs <- utils::head(txt, -1)
  if(grepl("git-upload-pack0000", txt[1])){
    # bitbucket.org seems to omit LF after 1st line
    refs[1] <- sub('.*git-upload-pack', "", refs[1])
  } else {
    refs <- utils::tail(refs, -1)
  }
  refs[1] <- sub("^0000", "", refs[1])
  substring(refs, 5)
}

remote_heads_many <- function(repos, refs = NULL, verbose = TRUE){
  pool <- curl::new_pool()
  len <- length(repos)
  out <- character(len)
  completed <- 0
  lapply(seq_len(len), function(i){
    k <- i
    url <- sprintf('%s/info/refs?service=git-upload-pack', repos[i])
    ref <- ifelse(length(refs) && !is.na(refs[i]), refs[i], "HEAD")
    h <- curl::new_handle(useragent = 'git/2.35.1.windows.2', failonerror = TRUE)
    curl::curl_fetch_multi(url, handle = h, done = function(res){
      txt <- parse_raw_gitpack(res$content)
      pattern <- ifelse(ref=='HEAD', 'HEAD$', sprintf("\\/%s$", ref))
      match <- grep(pattern, txt, value = TRUE)
      out[k] <<- ifelse(length(match), sub(" .*$", "", match), NA_character_)

      # In case of annotated tags, we actually need the dereferenced ^{} value
      if(!identical(ref, 'HEAD')){
        match <- grep(sprintf('refs/tags/%s^{}', ref), txt, fixed = TRUE, value = TRUE)
        if(length(match)){
          out[k] <<- sub(" .*$", "", match)
        }
      }

      if(verbose) {
        completed <<- completed + 1
        if((len-completed) %% 100 == 0)
          cat(sprintf("\rScanning for changes... %d/%d", as.integer(completed), as.integer(len)), file = stderr())
      }
    }, pool = pool)
  })
  curl::multi_run(pool = pool)
  cat("\n", file = stderr())
  out
}
