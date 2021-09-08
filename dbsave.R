################################################################################
# Script to save individual scrapes to mongodb
################################################################################

# Load required packages
packagesNeeded <- c('tidyverse', 'mongolite', 'data.table')
package.check <- lapply(
  packagesNeeded,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

dframe <- read.table(file='.keys/mongodb.txt',header=FALSE, sep='=',col.names=c('Key','Value'))
dtable <- data.table(dframe,key='Key')

connection_string <- as.character(dtable['connect','Value'])
price_collection <- mongo(collection="prices", db="priceScrapeResults", url=connection_string)

#product <- tibble(
# name = "Product Name Goes Here",
# price = "$649.99",
# code = "123-340402",
# request_url = "https://www.testurl.com",
# date_scraped = Sys.time()
#)

#price_collection$insert(product)

save_scrape <- function(scrape) {
  result <- price_collection$insert(scrape)
  if (length(result$writeErrors) > 0) {
    print("scrape db write error")
    print(result$writeErrors) 
  } else { print("wrote scrape to db") }
  
  # Determine price aggregates for product
  db_result <- product_collection$find(paste0('{"url" : "',scrape$request_url,'"}'))
  
  # Determine max price
  highest_product_price <- max(
    if(db_result$highest_price == "Inf") { 0 } else { as.numeric(db_result$highest_price) },
    scrape$price
  )
  
  # Determine min price
  lowest_product_price <- min(
    as.numeric(db_result$lowest_price),
    scrape$price
  )
  
  # Determine typical price
  # Define typical price as the most frequent price observed
  getmode <- function(v) {
    as.numeric(names(sort(-table(v)))[1])
  }
  
  typical_product_price <- getmode(
    price_collection$find(paste0(
      '{"request_url" : "',scrape$request_url,'"}'),
      '{"price":1,"_id":0}')
    # Future update add qualifier to only return scrapes from last 90 days
  )
  
  product <- tibble(
    product_name = scrape$name,
    last_scraped = scrape$date_scraped,
    last_price = scrape$price,
    highest_price = highest_product_price,
    lowest_price = lowest_product_price,
    typical_price = typical_product_price
  )
  
  result <- product_collection$update(
    sprintf('{"url":"%s"}', scrape$request_url),
    sprintf('{ "$set": { "product_name" : "%s", "last_scraped" : "%s", "last_price" : "%s", "highest_price" : "%s", "lowest_price" : "%s", "typical_price" : "%s" } }',
            product$product_name,
            product$last_scraped,
            product$last_price,
            product$highest_price,
            product$lowest_price,
            product$typical_price)
  )
  
  if (length(result$writeErrors) > 0) {
    print("product db write error")
    print(result$writeErrors) 
  } else {
    print("product db updated successfully")
  }
}