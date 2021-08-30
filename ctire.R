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

# No longer needed product_url <- "https://www.canadiantire.ca/en/pdp/napoleon-ld3-3-burner-propane-grill-0853158p.html#srp"

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

product_url_list <- c(
  "https://www.canadiantire.ca/en/pdp/napoleon-ld4-4-burner-natural-gas-grill-0853161p.html#srp",
  "https://www.canadiantire.ca/en/pdp/napoleon-ld3-3-burner-propane-grill-0853158p.html#srp",
  "https://www.canadiantire.ca/en/pdp/weber-spirit-3-burner-propane-bbq-0853196p.html#srp",
  "https://www.canadiantire.ca/en/pdp/coleman-cookout-4-burner-propane-bbq-0853182p.html#srp",
  "https://www.canadiantire.ca/en/pdp/napoleon-ld3-3-burner-natural-gas-grill-0853159p.html#srp",
  "https://www.canadiantire.ca/en/pdp/stanley-black-chrome-socket-set-183-pc-0589287p.html",
  "https://www.canadiantire.ca/en/pdp/dewalt-dcf887b-20v-max-xr-brushless-3-speed-impact-driver-tool-only-0546726p.html#srp",
  "https://www.canadiantire.ca/en/pdp/dewalt-dcb203-2-20v-max-2-0-ah-li-ion-battery-2-pk-0543201p.html#srp",
  "https://www.canadiantire.ca/en/pdp/sodastream-fizzi-one-touch-sparkling-water-maker-black-0431019p.html",
  "https://www.canadiantire.ca/en/pdp/chamberlain-medium-lift-belt-drive-garage-door-opener-with-wi-fi-0460594p.html"
  
)

scrape_canadian_tire <- function(product_url_list) {
  lapply(product_url_list, function(x) {
    scrape_url(x)
    Sys.sleep(3)
  })  
}
