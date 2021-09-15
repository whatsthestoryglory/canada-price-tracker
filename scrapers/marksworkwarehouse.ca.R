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

# Source the products script for later use
source('products.R')

scrape_url <- function(url) {
  system(paste("C:\\Users\\Cam\\phantomjs-2.1.1-windows\\bin\\phantomjs.exe scrapers\\marksworkwarehouse.ca.js",url))
  product_html <- read_html('mww.html', encoding = "UTF-8")
  
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
  if (product_price == "Inf") { 
    print("Error reading price.  Not adding to db") 
    print(url)
  } else {
  product <- tibble(
    name = product_name,
    price = product_price,
    code = product_code,
    request_url = url,
    date_scraped = Sys.time()
  )
  print(product)
  save_scrape(product) 
  }
}

scrape_marks <- function(product_url_list) {
  lapply(product_url_list, function(x) {
    scrape_url(x)
    Sys.sleep(3)
  })  
}

mwwurls <- product_collection$find('{ "domain" : "www.marks.com" }') %>% select(url)

scrape_marks(mwwurls[,1])
