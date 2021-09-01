################################################################################
# Script to manage add/remove products to mongodb
################################################################################

# Load required packages
packagesNeeded <- c('tidyverse', 'mongolite', 'data.table', 'urltools')
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
product_collection <- mongo(collection="products", db="priceScrapeResults", url=connection_string)

addProduct <- function (url) {
  parsed_url <- urltools::url_parse(url)
  parsed_url$fragment <- NA
  parsed_url$parameter <- NA
  cleaned_url <- urltools::url_compose(parsed_url)
  product <- tibble(
    url = cleaned_url,
    domain = parsed_url$domain,
    last_requested = Sys.Date()
  )
  product_collection$insert(product)
}

addProducts <- function(urls) {
  lapply(urls, addProduct)
}

addProduct("https://www.marks.com/en/timberland-mens-logan-bay-leather-chukka-boots-brown-color-brown-103256.html")
