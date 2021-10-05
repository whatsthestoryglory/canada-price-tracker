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
  # system(paste("C:\\Users\\Cam\\phantomjs-2.1.1-windows\\bin\\phantomjs.exe scrapers\\marksworkwarehouse.ca.js",url))
  # print(paste("Scraping url:", url))
  product_html <- read_html(url, encoding = "UTF-8")
  
  product_price <- product_html %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "price-show-panel", " " ))]') %>% 
    html_nodes('span strong') %>%
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
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "mb-0", " " ))]//strong') %>% 
    html_text() %>% 
    str_remove_all("\n") %>%
    gsub(pattern = '\"', replacement="'")
    
  
  product_code <- product_html %>% 
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "col-auto", " " )) and contains(concat( " ", @class, " " ), concat( " ", "pr-0", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "text-small", " " ))]') %>% 
    html_text() %>%
    gsub(pattern="\\n",replacement="") %>%
    gsub(pattern="Item Code:  ", replacement="")
  
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

scrape_cc <- function(product_url_list) {
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

ccurls <- product_collection$find('{ "domain" : "www.canadacomputers.com" }') %>% select(url)

scrape_cc(ccurls[,1])
