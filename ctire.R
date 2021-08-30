################################################################################
# Script to scrape from canadiantire.ca
################################################################################

# Load required packages
packagesNeeded <- c('tidyverse', 'rvest', "R.utils", 'httr')
package.check <- lapply(
  packagesNeeded,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# Source the saving script for later use
source('dbsave.R')

product_url <- "https://www.canadiantire.ca/en/pdp/napoleon-ld3-3-burner-propane-grill-0853158p.html#srp"

scrape_url <- function(url) {
  system(paste("C:\\Users\\Cam\\phantomjs-2.1.1-windows\\bin\\phantomjs.exe ctscrape.js",url))
  
  product_html <- read_html('ct.html')
  
  product_price <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "price__now--value", " " ))]') %>% html_text()
  
  if (length(product_price) == 0) {
    product_price <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "price__reg-value_multisku", " " ))]') %>% html_text()
    
  } else {
    product_price <- product_price[1]
  }
  
  product_name <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "js-product-name", " " ))]') %>% html_text() %>% str_remove_all("\n")
  product_code <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "js-product-title-id", " " ))]') %>% html_text()
  product_code <- product_code[1]
  
  product <- tibble(
    name = product_name,
    price = product_price,
    code = product_code,
    request_url = url,
    date_scraped = Sys.time()
  )
  
  save_scrape(product) 
}

scrape_url(product_url)