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
library(lubridate)
# readRenviron(".Renviron")
mongo_string <- (Sys.getenv("MONGO_STRING"))
shiny_product_collection <- mongo(collection="products", db="priceScrapeResults", url=mongo_string)
shiny_price_collection <- mongo(collection="prices", db="priceScrapeResults", url=mongo_string)
domain_list <- tibble("Domains" = shiny_product_collection$distinct("domain"))
domain_list <- tibble("Domains" = prepend(domain_list$Domains, ""))

# Define UI for application
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
    # print(paste("plotPriceHistory", price_data, length(price_data)))
    plotme <- tibble("X" = c(1,2,3,4,5), "Y" = c(5,4,3,2,1))
    plotly_plot <- plot_ly(plotme, type="scatter", mode="lines") %>%
        add_trace(x=~X, y=~Y)
    if (length(price_data)) {
        print("length(price_data)")
        prices_to_render <- tibble(price_data) %>%
            mutate(date_col = date(date_scraped)) %>%
            group_by(date_col) %>%
            summarize(value = mean(price))
        plotly_plot <- plot_ly(prices_to_render, type="scatter", mode="lines") %>%
            add_trace(
                x = ~date_col, 
                y = ~value,
                hovertemplate = "<b>Price:</b> %{y} <br><b>Date:</b> %{x} <extra></extra>"
                ) %>%
            layout(showlegend = F) %>%
            layout(
                xaxis = list(zerolinecolor = '#ffff',
                             zerolinewidth = 2,
                             gridcolor = 'ffff'),
                yaxis = list(zerolinecolor = '#ffff',
                             zerolinewidth = 2,
                             gridcolor = 'ffff',
                             hoverformat = '$,.2f'),
                plot_bgcolor='#e5ecf6')
        }
    return(partial_bundle(plotly_plot, type = "auto", local = TRUE, minified = TRUE))
}


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$productTable <- DT::renderDT(
        datatable(
            getProductList(input$domain), 
            selection = 'single', # Enables selecting single rows only
            rownames= FALSE, # Hides the row numbers column
            options = list(
                columnDefs = list(list(visible=FALSE, targets=4)), # Hides the URL column
                scrollY = "200px",
                scrollCollapse = TRUE,
                paging = FALSE)) 
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


    output$selectedProducts <- renderPlotly({
        sortedList <- getProductList(input$domain)
        # print(sortedList)
        urlToFind <- sortedList$`URL`[input$productTable_rows_selected]
        prices <- shiny_price_collection$find(paste0('{"request_url" : "', urlToFind, '"}'), '{"_id" : 0, "price": 1, "date_scraped" : 1}')
        toPlot <- plotPriceHistory(prices)
        # print(length(toPlot))
        toPlot
    })
    #output$selectedProducts <- getPriceHistory(input$productTable_rows_selected)
}

# Run the application 
shinyApp(ui = ui, server = server)
