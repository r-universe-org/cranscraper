# The tree-commit API is hacked from the GH front-end
scrape_cran_authors <- function(packages){
  pool <- curl::new_pool(multiplex = FALSE, host_con = 2)
  logins <- rep(NA, length(packages))
  homerepos <- rep(NA, length(packages))
  lapply(seq_along(packages), function(i){
    pkg <- packages[i]
    info_url <- sprintf('https://github.com/cran/%s/tree-commit/HEAD', pkg)
    curl::multi_add(make_handle(info_url), done = function(req1){
      cat("HTTP ", req1$status_code, " - ", info_url, "\n", file = stderr())
      if(req1$status_code == 200){
        doc <- xml2::read_html(rawToChar(req1$content))
        author <- xml2::xml_find_first(doc, "//*[contains(@class, 'commit-author')]")
        if(xml2::xml_name(author) == 'a'){
          logins[i] <<- xml2::xml_text(author)
          home_url <- sprintf('https://github.com/%s/%s/blob/master/DESCRIPTION', logins[i], pkg)
          curl::multi_add(make_handle(home_url), done = function(req2){
            if(req2$status_code == 200){
              api_url <- sprintf('https://api.github.com/repos/%s/%s', logins[i], pkg)
              curl::multi_add(make_handle(api_url), done = function(req3){
                if(req3$status_code == 200){
                  repodata <- jsonlite::parse_json(rawToChar(req3$content))
                  homerepos[i] <<- if(isTRUE(repodata$fork)){
                    repodata$parent$html_url
                  } else {
                    repodata$html_url
                  }
                  message("Found undocumented repo: ", homerepos[i])
                } else {
                  message(sprintf("Failure: %s (HTTP %d)", api_url, req3$status_code))
                }
              }, pool = pool)
            }
          }, pool = pool)
        }
      }
    }, pool = pool)
  })
  curl::multi_run(pool = pool)
  return(homerepos)
}
