#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mongolite)
library(data.table)
library(DT)
library(tidyverse)
# readRenviron(".Renviron")
mongo_string <- (Sys.getenv("MONGO_STRING"))
shiny_product_collection <- mongo(collection="products", db="priceScrapeResults", url=mongo_string)
domain_list <- tibble("Domains" = shiny_product_collection$distinct("domain"))
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Price Beaver Test App"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("domain",
                        "Website:",
                        domain_list)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput('productTable')
        )
    )
)


# Define how the data table is returned

getProductList <- function(domain) {
    productList <- shiny_product_collection$find(
        query = paste0('{ "domain" : "',domain,'" }'),
        fields = '{ 
            "product_name" : true, 
            "last_price" : true, 
            "typical_price" : true, 
            "highest_price": true, 
            "lowest_price": true, 
            "last_scraped" : true,
            "_id" : false }'
    )
    sortedList <- tibble(
        "Product Name" = productList$product_name,
        "Most Recent Price" = productList$last_price,
        "Typical Price" = (productList$typical_price),
        "Discount" = (as.numeric(productList$typical_price) - as.numeric(productList$last_price)) / as.numeric(productList$typical_price)
    )
    return(sortedList)
}



# Define server logic required to draw a histogram
server <- function(input, output) {
    output$productTable <- DT::renderDataTable(
        datatable(getProductList(input$domain))
    %>%
        formatCurrency(
            c("Most Recent Price", "Typical Price"),
            currency = "$",
            interval = 3,
            mark = ",",
            digits = 2,
            dec.mark = getOption("OutDec"),
            before = TRUE
        ) %>%
        formatPercentage(
            c("Discount"),
            digits = 2,
            interval = 3,
            mark = ",",
            dec.mark = getOption("OutDec")
        )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
