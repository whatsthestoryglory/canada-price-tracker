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
thematic::thematic_shiny()
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
    return(sortedList)
}


plotPriceHistory <- function(price_data, domain) {
    # print(paste("plotPriceHistory", price_data, length(price_data)))
    
    # Default plot to be shown when no row selected
    scrape_data <- shiny_price_collection$aggregate(paste0(
        '[{ 
            "$match": {
                        "request_url" : { "$regex": "',domain,'", "$options" : "i" }
                       }
         },
         {
            "$group":{ 
                "_id": { 
                    "$dateToString": { 
                        "date": "$date_scraped",
                        "format" : "%Y-%m-%d" 
                    } 
                },
                "count": { "$sum":1 },
                "uniqueValues": { "$addToSet": "$request_url"}
            } 
        }]'))
    scrape_data$uniqueCount <- lengths(scrape_data$uniqueValues)
    plotme <- tibble("X" = c(1,2,3,4,5), "Y" = c(5,4,3,2,1))
    plotly_plot <- plot_ly(
        scrape_data, 
        x = ~`_id`,
        y = ~uniqueCount,
        type="bar") %>%
        layout(
            title=paste("Prices scraped from", domain),
            xaxis = list(
                title="Date",
                type = "date",
                tickformat = "%B %Y"),
            yaxis = list(title="Quantity")
        )
    plotly_plot <- config(plotly_plot, displayModeBar = FALSE)
    
    # Change default plot if necessary
    if (length(price_data)) {
        prices_to_render <- tibble(price_data) %>%
            mutate(date_col = date(date_scraped)) %>%
            group_by(date_col) %>%
            summarize(value = mean(price))
        plotly_plot <- plot_ly(prices_to_render, type="scatter", mode="lines") 
        plotly_plot <- config(plotly_plot, displayModeBar = FALSE)%>%
            add_trace(
                x = ~date_col, 
                y = ~value,
                hovertemplate = "<b>Price:</b> %{y} <br><b>Date:</b> %{x} <extra></extra>"
                ) %>%
            layout(showlegend = F) %>%
            layout(
                title = list(
                    text = "Price history",
                    xanchor = 'left'),
                xaxis = list(zerolinecolor = '#ffff',
                             zerolinewidth = 2,
                             gridcolor = 'ffff',
                             title="Date"),
                yaxis = list(zerolinecolor = '#ffff',
                             zerolinewidth = 2,
                             gridcolor = 'ffff',
                             hoverformat = '$,.2f',
                             tickformat = '$',
                             title="Price"),
                plot_bgcolor='#e5ecf6')
        }
    return(partial_bundle(plotly_plot, type = "auto", local = TRUE, minified = TRUE))
}


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$productTable <- DT::renderDT(
        if (input$domain != "") { 
            print(length(input$domain))
            datatable(
                getProductList(input$domain), 
                selection = 'single', # Enables selecting single rows only
                rownames= FALSE, # Hides the row numbers column
                options = list(
                    columnDefs = list(list(visible=FALSE, targets=4)), # Hides the URL column
                    scrollY = "200px",
                    scrollCollapse = TRUE,
                    paging = FALSE,
                    order = list(3,'desc'))) %>%
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
        } else { data.table(variable1 = integer(),
                           variable2 = character(),
                           variable3 = numeric()) } )


    output$selectedProducts <- renderPlotly({
        sortedList <- getProductList(input$domain)
        # print(sortedList)
        urlToFind <- sortedList$`URL`[input$productTable_rows_selected]
        prices <- shiny_price_collection$find(paste0('{"request_url" : "', urlToFind, '"}'), '{"_id" : 0, "price": 1, "date_scraped" : 1}')
        toPlot <- plotPriceHistory(prices, input$domain)
        # print(length(toPlot))
        toPlot
    })
    #output$selectedProducts <- getPriceHistory(input$productTable_rows_selected)
}

# Run the application 
shinyApp(ui = ui, server = server)
