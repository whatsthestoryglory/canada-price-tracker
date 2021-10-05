################################################################################
# Script to scrape from toysrus.ca
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
  # print(paste("Reading from", url))
  product_html <- read_html(url, encoding = "UTF-8")
  
  product_price <- product_html %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "js-sales-price-value", " " ))]') %>% 
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
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "b-product_details-name", " " ))]') %>% 
    html_text() %>% 
    str_remove_all("\n") %>%
    gsub(pattern = '\"', replacement="'") %>%
    gsub(pattern = 'by', replacement="by ") %>%
    paste(collapse=" ")
    
  
  product_code <- product_html %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "js-attribute_container", " " ))]//strong') %>% 
    html_text() %>%
    gsub(pattern="\\n",replacement="") %>%
    gsub(pattern="Item Code:  ", replacement="")
  
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

scrape_toysrus <- function(product_url_list) {
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

toysrusurls <- product_collection$find('{ "domain" : "www.toysrus.ca" }') %>% select(url)

scrape_toysrus(toysrusurls[,1])
