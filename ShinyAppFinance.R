# Version 1.08
# Load necessary libraries
library(shiny)
library(bslib)    # for theme customization
library(ggplot2)  # for data visualization
library(plotly)   # for interactive charts
library(shinycssloaders)  # for loader animations
library(memoise)  # for caching

# Define the UI layout for the Shiny app with a modern theme
ui <- fluidPage(
  theme = bs_theme(
    version = 4,
    bootswatch = "cyborg",  # Dark theme suitable for financial apps
    primary = "#007bff",    # Blue accent
secondary = "#6c757d",  # Gray accent
success = "#00f2c3",    # Bright success color
danger = "#fd5d93"      # Bright danger color
),

# Custom CSS for consistent styling
tags$head(
  tags$style(HTML("
      body { font-family: 'Roboto', sans-serif; }
      .btn, .form-control { border-radius: 4px; }
      .sidebar { background-color: #1e1e2f; }
      .primary-text { color: #007bff; }
      .accent { color: #00f2c3; }
      .metric-box {
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        margin: 10px;
        background: #27293d;
        border: 1px solid rgba(255,255,255,0.1);
      }
      .metric-title {
        color: #8f8fb1;
        font-size: 14px;
        margin-bottom: 8px;
      }
      .value {
        font-size: 28px;
        font-weight: bold;
        display: flex;
        align-items: center;
        gap: 8px;
        color: white;
      }
    "))
),

# Title of the app
titlePanel("Financial Asset Portfolio Dashboard"),

# Sidebar layout with sidebar for controls and a main panel with tabs
sidebarLayout(
  sidebarPanel(
    # Stock selection and date range inputs with tooltips
    selectInput("stock_select", "Select Stock:", choices = c("AAPL", "GOOG", "TSLA", "AMZN", "NFLX"), 
                selected = "AAPL", multiple = TRUE),
    helpText("Choose one or more stocks to analyze."),
    dateRangeInput("date_range", "Select Date Range:",
                   start = as.Date("2022-01-01"), end = as.Date("2023-01-01"),
                   min = as.Date("2022-01-01"), max = as.Date("2023-01-01")),
    helpText("Adjust the date range for performance analysis."),
    
    # Add loading status indicator
    uiOutput("data_loading_status"),
    
    # Download button for exporting data
    downloadButton("download_data", "Download Portfolio Data")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Overview", 
               h4("Portfolio Summary"),
               # Dynamic Performance Cards
               fluidRow(
                 column(4,
                        div(class = "metric-box",
                            h4(class = "metric-title", "Daily Return"),
                            uiOutput("daily_return")
                        )
                 ),
                 column(4,
                        div(class = "metric-box",
                            h4(class = "metric-title", "Total Portfolio Value"),
                            uiOutput("total_value")
                        )
                 ),
                 column(4,
                        div(class = "metric-box",
                            h4(class = "metric-title", "Volatility"),
                            uiOutput("volatility")
                        )
                 )
               )
      ),
      tabPanel("Performance", 
               h4("Performance Chart"),
               withSpinner(plotlyOutput("performance_plot"))
      ),
      tabPanel("Allocation", 
               h4("Asset Allocation"), 
               withSpinner(plotlyOutput("allocation_chart"))
      )
    )
  )
)
)

# Define the server logic for the Shiny app
server <- function(input, output) {
  
  # Memoised function for caching stock data calculations
  memoized_stock_data <- memoise(function(date_range, stocks) {
    dates <- seq.Date(date_range[1], date_range[2], by = "day")
    data <- data.frame(
      Date = rep(dates, length(stocks)),
      Stock = rep(stocks, each = length(dates)),
      Price = abs(cumsum(rnorm(length(dates) * length(stocks), mean = 0.1, sd = 1))) + 50  # Ensures positive prices
    )
    data
  })
  
  # Reactive stock data with caching
  stock_data <- reactive({
    memoized_stock_data(input$date_range, input$stock_select)
  })
  
  # Dynamic Performance Cards
  output$daily_return <- renderUI({
    returns <- diff(log(stock_data()$Price))
    daily_return <- mean(returns, na.rm = TRUE) * 100
    div(class = "value",
        sprintf("%.2f%%", daily_return),
        if(daily_return > 0) {
          icon("arrow-up", class = "text-success")
        } else {
          icon("arrow-down", class = "text-danger")
        }
    )
  })
  
  output$total_value <- renderUI({
    total_value <- sum(stock_data()$Price)
    div(class = "value",
        paste("$", formatC(total_value, format = "f", big.mark = ",", digits = 0))
    )
  })
  
  output$volatility <- renderUI({
    if (nrow(stock_data()) > 1) {
      volatility <- sd(diff(log(stock_data()$Price))) * sqrt(252) * 100
      div(class = "value",
          paste(round(volatility, 2), "%")
      )
    } else {
      div(class = "value", "Insufficient data")
    }
  })
  
  # Adjusted Indexed data for performance plot with error handling
  # Replace the indexed_stock_data reactive expression with this:
  indexed_stock_data <- reactive({
    data <- stock_data()
    req(nrow(data) > 1)  # Ensure we have sufficient data
    
    # Split data by Stock and calculate indexed prices
    stocks <- unique(data$Stock)
    indexed_data <- data.frame()
    
    for(stock in stocks) {
      stock_subset <- data[data$Stock == stock, ]
      first_price <- stock_subset$Price[1]
      stock_subset$IndexedPrice <- (stock_subset$Price / first_price) * 100
      indexed_data <- rbind(indexed_data, stock_subset)
    }
    
    indexed_data
  })
  
  # Simplified Performance Plot
  output$performance_plot <- renderPlotly({
    req(nrow(indexed_stock_data()) > 1)  # Only render if data is valid and has rows
    data <- indexed_stock_data()
    plot_ly(data = data,
            x = ~Date, y = ~IndexedPrice, color = ~Stock,
            type = 'scatter', mode = 'lines', text = ~Stock) %>%
      layout(
        dragmode = "zoom",
        hovermode = "x unified",
        yaxis = list(title = "Performance Index (100 = Start)"),
        xaxis = list(title = "Date"),
        plot_bgcolor = "#27293d",
        paper_bgcolor = "#27293d",
        font = list(color = "white")
      )
  })
  
  # Asset allocation pie chart remains the same
  output$allocation_chart <- renderPlotly({
    allocation <- aggregate(Price ~ Stock, data = stock_data(), FUN = function(x) tail(x, 1))
    allocation$Allocation <- allocation$Price / sum(allocation$Price) * 100
    
    plot_ly(allocation, labels = ~Stock, values = ~Allocation, 
            type = 'pie', 
            textinfo = 'percent',
            hoverinfo = 'label+percent',
            marker = list(colors = c("#007bff", "#00f2c3", "#fd5d93", "#ff8d72", "#1d8cf8"))) %>%
      layout(
        showlegend = TRUE,
        plot_bgcolor = "#27293d",
        paper_bgcolor = "#27293d",
        font = list(color = "white")
      )
  })
  
  # Download data handler remains the same
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Portfolio_Data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(stock_data(), file)
    }
  )
}


# Create Shiny app with the defined UI and server
shinyApp(ui = ui, server = server)