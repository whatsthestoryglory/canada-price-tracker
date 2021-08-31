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

product_url_list <- c(
  "https://www.sportchek.ca/categories/women/footwear/sneakers-lifestyle-boots/sneakers/product/converse-chuck-hi-shoes-black-white-unisex-331444675.html",
  "https://www.sportchek.ca/categories/women/mothers-day-gift-guide/activewear/product/apple-airpods-with-charging-case-color-332944799_10-332944799.html",
  "https://www.sportchek.ca/categories/women/footwear/running-shoes/neutral/product/asics-womens-gel-excite-8-running-shoes-color-333371880_01-333371880.html",
  "https://www.sportchek.ca/categories/shop-by-sport/running/electronics/product/garmin-fenix-6s-pro-solar-fitness-watch-color-333333997_54-333333997.html",
  "https://www.sportchek.ca/categories/electronics/headphones-speakers/headphones/earbuds-in-ear/true-wireless/product/jaybird-vista-2-true-wireless-sport-headphones-color-333364786_01-333364786.html",
  "https://www.sportchek.ca/categories/shop-by-sport/running/running-apparel/mens/product/nike-mens-essential-running-jacket-333170392.html",
  "https://www.sportchek.ca/categories/shop-by-sport/running/electronics/product/jbl-endurance-peak-ii-waterproof-true-wireless-sport-in-ear-head-color-333504056_01-333504056.html"
)

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
