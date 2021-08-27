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

response <- GET(product_url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:91.0) Gecko/20100101 Firefox/91.0"))

system("C:\\Users\\Cam\\phantomjs-2.1.1-windows\\bin\\phantomjs.exe ctscrape.js")

product_html <- read_html('ct.html')

product_price <- product_html %>% html_nodes(".price__now--value") %>% html_text()

if (length(product_price) == 0) {
  product_price <- product_html %>% html_nodes(".price__reg-value_multisku") %>% html_text()
  
} else {
  product_price <- product_price[1]
}
