archived_registry <- function(max_age = 60, skip = 'request'){
  df <- read.csv('archived.csv')
  age <- Sys.Date() - as.Date(df$Date)
  df[age < max_age & !grepl(skip, df$Reason, ignore.case = TRUE),]
}

update_archived_csv <- function(){
  old <- read.csv('archived.csv')
  new <- cran_archived_db()
  stopifnot(nrow(new) > 1800) # Sanity check
  message("Newly archived: ", paste(setdiff(new$Package, old$Package), collapse = ', '))
  message("Unarchived packages: ", paste(setdiff(old$Package, new$Package), collapse = ', '))
  old$Date <- NULL
  old$Reason <- NULL
  db <- left_join(new, old, by = "Package")
  db <- db[order(db$Package, method = 'radix'),]
  lapply(which(is.na(db$Version)), function(i){
    pkg <- db$Package[i]
    message("Downloading archived description for: ", pkg)
    tryCatch({
      pkginfo <- as.data.frame(read_description(sprintf('https://raw.githubusercontent.com/cran/%s/HEAD/DESCRIPTION', pkg)))
      if(length(pkginfo$Encoding)){
        Encoding(pkginfo$Maintainer[1]) <- pkginfo$Encoding
      }
      db$Version[i] <<- pkginfo$Version
      db$Maintainer[i] <<- enc2utf8(pkginfo$Maintainer[1])
      db$Git[i] <<- find_git_url(pkginfo)[1]
    }, error = message)
  })
  out <- db[c("Package", "Version", "Maintainer", "Git", "Date", "Reason")]
  write.csv(out, 'archived.csv', row.names = FALSE)
}

cran_archived_db <- function(){
  con <- url("https://cran.r-project.org/src/contrib/PACKAGES.in")
  on.exit(close(con))
  db <- as.data.frame(read.dcf(con))
  comments <- db[['X-CRAN-Comment']]
  pattern <- "(Removed|Archived) on ([0-9-]+)"
  m <- regexec(pattern, comments)
  db$Date <- as.Date(vapply(regmatches(comments, m), function(str){
    if(length(str)){
      sub(pattern, '\\2', str[1])
    } else {
      NA_character_
    }
  }, character(1)))
  db <- db[!is.na(db$Date) & db$Date >= '2022-01-01',]
  db$Reason <- gsub("\\s", " ", trimws(sub(" as|for", "", sub(pattern, '', db[['X-CRAN-Comment']]))))
  db[order(db$Package),c("Package", "Date", "Reason")]
}
