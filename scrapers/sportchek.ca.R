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

# Source the products script for later use
source('products.R')

scrape_url <- function(url) {
  system(paste("C:\\Users\\Cam\\phantomjs-2.1.1-windows\\bin\\phantomjs.exe scrapers\\sportchek.ca.js",url))
  product_html <- read_html('sc.html', encoding = "UTF-8")
  
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

scrape_sportchek <- function(product_url_list) {
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

sportchekurls <- product_collection$find('{ "domain" : "www.sportchek.ca" }') %>% select(url)

scrape_sportchek(sportchekurls[,1])
