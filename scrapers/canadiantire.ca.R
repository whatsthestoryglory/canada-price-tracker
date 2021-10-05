################################################################################
# Script to scrape from canadiantire.ca
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

# No longer needed product_url <- "https://www.canadiantire.ca/en/pdp/napoleon-ld3-3-burner-propane-grill-0853158p.html#srp"

scrape_url <- function(url) {
  system(paste("C:\\Users\\Cam\\phantomjs-2.1.1-windows\\bin\\phantomjs.exe scrapers\\canadiantire.ca.js",url))
  product_html <- read_html('ct.html', encoding = "UTF-8")
  
  product_price <- product_html %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "price__now--value", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "price__total--on-sale", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "price__reg-value_multisku", " " ))] | //*[contains(concat( " ", @class, " " ), concat( " ", "price__reg-value", " " ))]') %>%
    html_text() %>%
    as.data.frame() %>% 
    distinct()
  
  #if (length(product_price) == 0) { 
  #  product_price <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "price__total--on-sale", " " ))]') %>% html_text()
  #}
  if (length(unlist(product_price)) == 1) {
    product_price <- parse_number(as.character(product_price))
  } else {
    product_price <- product_price %>% 
      str_c(collapse="") %>% 
      str_extract_all(pattern="([0123456789.]+,*[0123456789.]*)") %>% 
      unlist() %>% 
      gsub(pattern=",", replacement="") %>%
      as.numeric() %>%
      min()
  }
  #if (length(product_price) == 0) {
  #  product_price <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "price__reg-value_multisku", " " ))]') %>% html_text()
  #  
  #} else {
  #  # product_price <- product_price[1]
  #}
  
  product_name <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "js-product-name", " " ))]') %>% html_text() %>% str_remove_all("\n")
  product_code <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "js-product-title-id", " " ))]') %>% html_text()
  product_code <- product_code[1]
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

scrape_canadian_tire <- function(product_url_list) {
  lapply(product_url_list, function(x) {
    tryCatch( {
      scrape_url(x)
    },
    error=function(cond) {
      message(paste("URL Error on", x))
      message("Error message:")
      message(cond)
      return(NA)
    }
    )
    Sys.sleep(3)
  })  
}

ctireurls <- product_collection$find('{ "domain" : "www.canadiantire.ca" }') %>% select(url)
scrape_canadian_tire(ctireurls[,1])
