################################################################################
# Script to scrape from canadiantire.ca
################################################################################

# Load required packages
packagesNeeded <- c('tidyverse', 'rvest', "R.utils", 'httr')
package.check <- lapply(
  packagesNeeded,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


test_doc <- paste("<html>",
                  "<body>",
                  "<span class='a1 b1'> text1 </span>",
                  "<span class='price__now--value'> text2 </span>",
                  "</body>",
                  "</html>"
)

product_url <- "https://www.canadiantire.ca/en/pdp/vermont-castings-vanguard-4-burner-with-side-burner-convertible-gas-bbq-grill-0853156p.html"

system(paste("C:\\Users\\Cam\\phantomjs-2.1.1-windows\\bin\\phantomjs.exe ctscrape.js",product_url))

product_html <- read_html('ct.html')

product_price <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "price__now--value", " " ))]') %>% html_text()

if (length(product_price) == 0) {
  product_price <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "price__reg-value_multisku", " " ))]') %>% html_text()
  
} else {
  product_price <- product_price[1]
}

product_name <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "js-product-name", " " ))]') %>% html_text() %>% str_remove_all("\n")
product_code <- product_html %>% html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "js-product-title-id", " " ))]') %>% html_text()
product_code <- product_code[1]
