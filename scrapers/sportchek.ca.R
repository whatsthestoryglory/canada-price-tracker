################################################################################
# Script to scrape from sportchek.ca
################################################################################

# Load required packages
packagesNeeded <- c('tidyverse', 'rvest', "R.utils", 'httr', 'urltools')
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


scrape_url <- function(url) {
  system(paste("C:\\Users\\Cam\\phantomjs-2.1.1-windows\\bin\\phantomjs.exe scrapers\\sportchek.ca.js",url))
  product_html <- read_html('sc.html')
  
  product_price <- product_html %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "product-sale-price", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "product-detail__price-text", " " ))]') %>%
    html_text() %>%
    gsub(pattern="\\$",replacement="") %>%
    gsub(pattern=",",replacement="") %>%
    as.numeric() %>%
    min()
  
  product_name <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "global-page-header__title", " " ))]') %>% html_text() %>% str_remove_all("\n")
  product_code <- product_html %>% html_nodes(xpath='//*[(@id = "product-detail__description-style-num")]') %>% html_text()
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

scrape_sportchek <- function(product_url_list) {
  lapply(product_url_list, function(x) {
    scrape_url(x)
    Sys.sleep(3)
  })  
}

sportchekurls <- price_collection$distinct("request_url") %>% 
  urltools::url_parse() %>% 
  filter(domain == "www.sportchek.ca") %>% 
  urltools::url_compose()

scrape_sportchek(sportchekurls)
