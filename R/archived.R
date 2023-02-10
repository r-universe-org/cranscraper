archived_registry <- function(max_age = 60, skip = 'request'){
  df <- read.csv('archived.csv')
  age <- Sys.Date() - as.Date(df$Date)
  df[age < max_age & !grepl(skip, df$Reason, ignore.case = TRUE),]
}

update_archived_csv <- function(){
  old <- if(file.exists('archived.csv')){
    read.csv('archived.csv')
  } else {
    data.frame(Package=character())
  }
  new <- cran_archived_db()
  stopifnot(nrow(new) > 1800) # Sanity check
  m <- match(new$Package, old$Package)
  message("Newly archived: ", paste(setdiff(new$Package, old$Package), collapse = ', '))
  message("Expired packages: ", paste(setdiff(old$Package, new$Package), collapse = ', '))
  new$Maintainer = old$Maintainer[m]
  new$Git = old$Git[m]
  for(i in which(is.na(m))){
    x <- as.list(new[i,])
    message("Downloading archived description for: ", x$Package)
    tryCatch({
      pkginfo <- as.data.frame(read_description(sprintf('https://raw.githubusercontent.com/cran/%s/master/DESCRIPTION', x$Package)))
      new[i, 'Maintainer'] <- pkginfo$Maintainer[1]
      new[i, 'Git'] <- find_git_url(pkginfo)[1]
    }, error = message)
  }
  out <- new[c("Package", "Maintainer", "Git", "Date", "Reason")]
  write.csv(out, 'archived.csv', row.names = FALSE)
}

cran_archived_db <- function(){
  con <- url("https://cloud.r-project.org/src/contrib/PACKAGES.in")
  on.exit(close(con))
  db <- as.data.frame(read.dcf(con))
  comments <- db[['X-CRAN-Comment']]
  pattern <- "Archived on [0-9-]+"
  m <- regexec(pattern, comments)
  db$Date <- as.Date(vapply(regmatches(comments, m), function(str){
    if(length(str)){
      substring(str, 13)
    } else {
      NA_character_
    }
  }, character(1)))
  db <- db[!is.na(db$Date) & db$Date >= '2022-01-01',]
  db$Reason <- gsub("\\s", " ", trimws(sub(" as|for", "", sub(pattern, '', db[['X-CRAN-Comment']]))))
  db[order(db$Package),c("Package", "Date", "Reason")]
}
