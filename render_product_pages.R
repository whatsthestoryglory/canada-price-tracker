################################################################################
# Script to render all product pages
################################################################################

# Source the products script for later use
source('products.R')

product_urls <- product_collection$distinct("url")

render_product_page <- function(url) {
  domain <- urltools::suffix_extract(urltools::domain(url))$domain
  if (!is.na(urltools::url_parse(url)$parameter)) {
  page <- paste0(urltools::url_parse(url)$path, urltools::url_parse(url)$parameter)
  } else {
    page <- urltools::url_parse(url)$path
  }
  page <- unlist(strsplit(page, "/"))
  page <- page[length(page)]
  if (!endsWith(page, ".html")) { page <- paste0(page, ".html") }
  rmarkdown::render(
    "product_page.Rmd", 
    params = list(
      url = url
    ),
    output_file = paste0("products\\",domain,"\\",page)
  )
}

result <- lapply(product_urls, render_product_page)

