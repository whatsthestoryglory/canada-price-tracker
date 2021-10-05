# This file is scripts to scrape listing pages for individual product pages
# Includes are skipped, this is not meant to be used standalone
# This is only for me to fill the db with products to track


getToys <- function(url) {
  xPath <- '//*[contains(concat( " ", @class, " " ), concat( " ", "js-pdp-link", " " ))]'

  page_html <- read_html(url, encoding = "UTF-8")

  listOfProducts <- page_html %>% html_nodes(xpath=xPath) %>% html_attr("href")

  listToAdd <- paste0("https://www.toysrus.ca", listOfProducts)
  
  listToAdd <- tibble(listToAdd) %>% distinct()

  listToAdd <- listToAdd$listToAdd
  
  addProducts(listToAdd)
}

getChapters <- function(url) {
  xPath <- "//*[contains(concat( \" \", @class, \" \" ), concat( \" \", \"product-list__product-title-link--grid\", \" \" ))]"
  page_html <- read_html(url, encoding = "UTF-8")
  
  listOfProducts <- page_html %>% html_nodes(xpath=xPath) %>% html_attr("href")
  
  listToAdd <- paste0("https://www.chapters.indigo.ca", listOfProducts)
  
  listToAdd <- tibble(listToAdd) %>% distinct()
  
  listToAdd <- listToAdd$listToAdd
  
  addProducts(listToAdd)
}


getCTire <- function(url) {
    
    page_html <- read_html(url, encoding = "UTF-8")
    
    listOfProducts <- page_html %>% html_nodes(".product-tile-srp__main-link") %>% html_attr("href") 
    
    listToAdd <- paste0("https://www.canadiantire.ca", listOfProducts[!is.na(listOfProducts)])
    
    listToAdd <- tibble(listToAdd) %>% distinct()
    
    listToAdd <- listToAdd$listToAdd
    
    addProducts(listToAdd)
}
