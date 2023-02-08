archived_registry <- function(){
  read.csv('archived.csv')
}

update_archived_csv <- function(max_age = 30){
  old <- if(file.exists('archived.csv')){
    read.csv('archived.csv')
  } else {
    data.frame(Package=character())
  }
  new <- cran_archived_db(max_age = max_age)
  m <- match(new$Package, old$Package)
  message("Newly archived: ", paste(setdiff(new$Package, old$Package), collapse = ', '))
  message("Expired packages: ", paste(setdiff(old$Package, new$Package), collapse = ', '))
  new$Maintainer = old$Maintainer[m]
  new$Git = old$Git[m]
  for(i in which(is.na(m))){
    x <- as.list(new[i,])
    message("Retrieving updated data for: ", x$Package)
    tryCatch({
      pkginfo <- as.data.frame(read_description(sprintf('https://raw.githubusercontent.com/cran/%s/master/DESCRIPTION', x$Package)))
      new[i, 'Maintainer'] <- pkginfo$Maintainer[1]
      new[i, 'Git'] <- find_git_url(pkginfo)[1]
    }, error = message)
  }
  write.csv(new, 'archived.csv', row.names = FALSE)
}

cran_archived_db <- function(max_age = 30){
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
  db$age <- Sys.Date() - db$Date
  db <- db[!is.na(db$age) & db$age < max_age,c("Package", "Date")]
  db[order(db$Package),]
}
