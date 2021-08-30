################################################################################
# Script to save tibbles to mongodb
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

save_scrape <- function(product) {
  
  price_collection$insert(product)
  
}