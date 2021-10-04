#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(mongolite)
library(data.table)
library(DT)
library(tidyverse)
library(plotly)
# readRenviron(".Renviron")
mongo_string <- (Sys.getenv("MONGO_STRING"))
shiny_product_collection <- mongo(collection="products", db="priceScrapeResults", url=mongo_string)
shiny_price_collection <- mongo(collection="prices", db="priceScrapeResults", url=mongo_string)
domain_list <- tibble("Domains" = shiny_product_collection$distinct("domain"))


# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = bs_theme(version = 4, bootswatch = "darkly"),
    # Application title
    titlePanel("Price Beaver Test App"),

    fluidRow(
        
        # Sidebar
        column(4,
               wellPanel(
                   selectInput("domain",
                               "Website:",
                               domain_list)
               )
              ),
        
        # Main area
        column(8,
               plotlyOutput('selectedProducts'),
               textOutput('selectedProductText'),
               dataTableOutput('productTable')
              )
    )
    
    
    
    
    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #    sidebarPanel(
    #        selectInput("domain",
    #                    "Website:",
    #                    domain_list)
    #    ),

        # Show a plot of the generated distribution
    #    mainPanel(
    #       dataTableOutput('productTable')
    #    )
    # )
)

productList <- shiny_product_collection$find(
    query = paste0('{ "domain" : "www.canadiantire.ca" }'),
    fields = '{ 
            "product_name" : true, 
            "last_price" : true, 
            "typical_price" : true, 
            "highest_price": true, 
            "lowest_price": true, 
            "last_scraped" : true,
            "url" : true,
            "_id" : false }'
)

# Get list of products for selected domain
oldDomain <- ""

getDomainProducts <- function(domain) {
    if (oldDomain == "") { oldDomain <- domain }
    else if (oldDomain == domain) { 
        # if domain hasn't updated, return the same thing as last time
        return()
    }
    # if oldDomain isn't domain, then pull from mongodb
    
    
    
}



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
            "url" : true,
            "_id" : false }'
    )
    sortedList <- tibble(
        "Product Name" = productList$product_name,
        "Most Recent Price" = productList$last_price,
        "Typical Price" = (productList$typical_price),
        "Discount" = (as.numeric(productList$typical_price) - as.numeric(productList$last_price)) / as.numeric(productList$typical_price),
        "URL" = productList$url
    )
    return(sortedList %>% filter(Discount > 0))
}


plotPriceHistory <- function(price_data) {
    plotly_plot <- plot_ly(price_data, type="scatter", mode="lines") %>%
        add_trace(x = ~date_scraped, y = ~price) %>%
        layout(showlegend = F) %>%
        layout(
            xaxis = list(zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff'),
            yaxis = list(zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff'),
            plot_bgcolor='#e5ecf6')
    return(partial_bundle(plotly_plot, type = "auto", local = TRUE, minified = TRUE))
}


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$productTable <- DT::renderDT(
        datatable(
            getProductList(input$domain), 
            selection = 'single', # Enables selecting single rows only
            rownames= FALSE, # Hides the row numbers column
            options = list(columnDefs = list(list(visible=FALSE, targets=4)))) # Hides the URL column
    %>%
        formatCurrency(
            c("Most Recent Price", "Typical Price"), # Formats the price columns as currency
            currency = "$", 
            interval = 3,
            mark = ",",
            digits = 2,
            dec.mark = getOption("OutDec"),
            before = TRUE
        ) %>%
        formatPercentage(
            c("Discount"), # Formats the 'Discount' column as a percentage
            digits = 2,
            interval = 3,
            mark = ",",
            dec.mark = getOption("OutDec")
        )
    )
    
    output$selectedProductText <- renderText({
        sortedList <- getProductList(input$domain)
        urlToFind <- sortedList$`URL`[input$productTable_rows_selected]
        prices <- shiny_price_collection$find(paste0('{"request_url" : "', urlToFind, '"}'), '{"_id" : 0, "price": 1, "date_scraped" : 1}')
        colnames(prices)
    })

    output$selectedProducts <- renderPlotly({
        sortedList <- getProductList(input$domain)
        urlToFind <- sortedList$`URL`[input$productTable_rows_selected]
        prices <- shiny_price_collection$find(paste0('{"request_url" : "', urlToFind, '"}'), '{"_id" : 0, "price": 1, "date_scraped" : 1}')
        plotPriceHistory(prices)
    })
    #output$selectedProducts <- getPriceHistory(input$productTable_rows_selected)
}

# Run the application 
shinyApp(ui = ui, server = server)
