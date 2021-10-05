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
  if (parsed_url$domain == "chapters.indigo.ca") {
    parsed_url$domain <- "www.chapters.indigo.ca"
  }
  cleaned_url <- urltools::url_compose(parsed_url)
  # Canada Computers uses php queries for its products.  Strip all but needed
  if (parsed_url$domain == "www.canadacomputers.com") { 
    parameters <- param_get(url, parameter_names = c("cPath","item_id"))
    cleaned_url <- param_set(cleaned_url, key = "cPath", value = parameters$cPath)
    cleaned_url <- param_set(cleaned_url, key = "item_id", value = parameters$item_id)
  }

  product <- tibble(
    url = cleaned_url,
    domain = parsed_url$domain,
    last_requested = Sys.Date()
  )
  if (length(product_collection$find(paste0('{"url" : "',cleaned_url,'"}'))) > 0) {
    #product <- tibble( last_requested = Sys.Date() )
    result <- product_collection$update(
      sprintf('{"url":"%s"}', cleaned_url),
      sprintf('{ "$set": {"last_requested":"%s"} }', product$last_requested)
    )
  } else {
    result <- product_collection$insert(product) 
  }
  if (length(result$writeErrors) > 0) {
    print("db write error")
    print(result$writeErrors) 
  }
}

addProducts <- function(urls) {
  lapply(urls, addProduct)
}
