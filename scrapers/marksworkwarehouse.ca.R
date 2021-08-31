################################################################################
# Script to scrape from marksworkwarehouse.ca
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
  system(paste("C:\\Users\\Cam\\phantomjs-2.1.1-windows\\bin\\phantomjs.exe scrapers\\marksworkwarehouse.ca.js",url))
  product_html <- read_html('mww.html')
  
  product_price <- product_html %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "product-detail-price__current", " " ))]') %>% 
    html_text() %>%
    str_extract_all(pattern="([0123456789.]+,*[0123456789.]*)") %>% 
    unlist() %>% 
    gsub(pattern=",", replacement="") %>%
    as.numeric() %>%
    min()
  
  #if (length(product_price) == 0) {
  #  # product_price <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "product-detail__price-text", " " ))]') %>% html_text()
  #  print(paste("Price error on url", url, " price recorded is: ", product_price))
  #} else {
  #  product_price <- product_price[1]
  #}
  
  product_name <- product_html %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "product-detail-info__title", " " ))]') %>% 
    html_text() %>% 
    str_remove_all("\n")
    
  
  product_code <- product_html %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "product-detail-overview__style", " " ))]') %>% 
    html_text() %>%
    gsub(pattern="\\n",replacement="") %>%
    gsub(pattern="Style: ", replacement="")
  
  # product_code <- product_code[1]
  
  product <- tibble(
    name = product_name,
    price = product_price,
    code = product_code,
    request_url = url,
    date_scraped = Sys.time()
  )
  product
  save_scrape(product) 
}

product_url_list <- c(
  "https://www.marks.com/en/denver-hayes-mens-long-sleeve-classic-fit-core-casual-shirt-color-ltgrey-336632.html",
  "https://www.marks.com/en/denver-hayes-mens-fleece-hoodie-sweatshirt-color-stohea-315330.html",
  "https://www.marks.com/en/dakota-mens-trifold-wallet-with-id-window-color-black-302211.html",
  "https://www.marks.com/en/denver-hayes-mens-short-sleeve-modern-fit-core-casual-shirt-color-navy-345532.html",
  "https://www.marks.com/en/saucony-mens-grid-marauder-3-running-shoes-grey-color-grey-346259.html",
  "https://www.marks.com/en/scrubletics-womens-v-neck-athletic-kangaroo-pocket-scrub-top-black-color-black-323066.html",
  "https://www.marks.com/en/carhartt-mens-athletic-cargo-scrub-pants-color-pewter-239046.html",
  "https://www.marks.com/en/timberland-mens-logan-bay-leather-chukka-boots-brown-color-brown-103256.html"
)

scrape_marks <- function(product_url_list) {
  lapply(product_url_list, function(x) {
    scrape_url(x)
    Sys.sleep(3)
  })  
}

mwwurls <- price_collection$distinct("request_url") %>% 
  urltools::url_parse() %>% 
  filter(domain == "www.marks.com") %>% 
  urltools::url_compose()

scrape_marks(mwwurls)
