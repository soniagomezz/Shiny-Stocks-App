# Install necessary packages
if (!require(shiny)) install.packages("shiny")
if (!require(quantmod)) install.packages("quantmod")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(RColorBrewer)) install.packages("RColorBrewer")

library(shiny)
library(quantmod)
library(ggplot2)
library(RColorBrewer)

# List of 200 sample stock symbols (you can replace with an actual list of 200 stocks)
stock_symbols <- c("AAPL", "MSFT", "GOOGL", "TSLA", "AMZN", "META", "NVDA", "JPM", "BAC", "XOM",
                   "IBM", "GE", "NFLX", "INTC", "NVDA", "SPY", "QQQ", "BA", "PFE", "GS", "DIS", 
                   "V", "UNH", "MS", "MA", "WMT", "T", "MCD", "PEP", "KO", "BABA", "CVX", "CSCO", 
                   "CVS", "AMD", "PYPL", "BMY", "RTX", "LMT", "GS", "ABT", "INTU", "ADBE", "COST", 
                   "VZ", "HD", "UPS", "NVDA", "WBA", "SBUX", "ADP", "COP", "BNS", "ETSY", "CHTR",
                   "TXN", "BLK", "PFE", "AXP", "GM", "USB", "NKE", "CL", "MSCI", "MU", "ZTS", "MO",
                   "LUV", "LULU", "AIG", "ROST", "LOW", "ALL", "VFC", "AMAT", "AXP", "KHC", "PNC",
                   "TGT", "MSFT", "AMT", "SYF", "V", "MMM", "ABT", "CTSH", "TMO", "CAT", "BRK.B", 
                   "JCI", "BAX", "NEM", "MKC", "COF", "SYY", "BLK", "NOG", "F", "SNAP", "RBLX", 
                   "ROKU", "XPEV", "RIVN", "NEE", "NEE", "CSX", "SBUX", "ZM", "FISV", "EXC", "VLO", 
                   "EA", "DUK", "SO", "BBY", "KLAC", "KMB", "ILMN", "CVS", "FIS", "ICE", "TD", "GS", 
                   "ADBE", "AMZN", "MSFT", "TGT", "AAPL", "SNAP")

# Define UI
ui <- fluidPage(
  titlePanel("Enhanced Stock Market Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("stocks", "Select Stock Symbols:", 
                  choices = stock_symbols, 
                  selected = "AAPL", 
                  multiple = TRUE),
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = Sys.Date() - 365,
        end = Sys.Date()
      ),
      selectInput(
        "price_type",
        "Select Price Type:",
        choices = c("Open", "High", "Low", "Close", "Adjusted"),
        selected = "Close"
      ),
      actionButton("fetch_data", "Fetch Data"),
      hr(),
      
      # New customization options for the plot
      selectInput("plot_theme", "Select Plot Theme:", 
                  choices = c("Minimal" = "theme_minimal", 
                              "Light" = "theme_light", 
                              "Dark" = "theme_dark", 
                              "Classic" = "theme_classic")),
      
      selectInput("color_palette", "Select Color Palette:", 
                  choices = c("Set1", "Paired", "Dark2", "Set2")),
      
      sliderInput("line_size", "Line Size:", 
                  min = 0.5, max = 3, value = 1),
      
      sliderInput("transparency", "Line Transparency:", 
                  min = 0, max = 1, value = 0.7),
      
      downloadButton("save_plot", "Save Plot as PNG"),
      downloadButton("download_csv", "Download Data as CSV")
    ),
    mainPanel(
      plotOutput("stock_plot"),
      tableOutput("stock_data")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive value to store stock data
  stock_data <- reactiveVal(NULL)
  
  # Fetch Stock Data
  observeEvent(input$fetch_data, {
    req(input$stocks, input$date_range)
    
    symbols <- input$stocks # Get selected symbols
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    all_data <- lapply(symbols, function(symbol) {
      tryCatch({
        getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
      }, error = function(e) {
        NULL
      })
    })
    
    names(all_data) <- symbols
    stock_data(all_data)
  })
  
  # Render Line Plot
  output$stock_plot <- renderPlot({
    data <- stock_data()
    req(data)
    
    selected_type <- input$price_type
    price_col <- switch(selected_type,
                        "Open" = 1,
                        "High" = 2,
                        "Low" = 3,
                        "Close" = 4,
                        "Adjusted" = 6)
    
    plot_data <- do.call(rbind, lapply(names(data), function(symbol) {
      df <- data.frame(Date = index(data[[symbol]]),
                       Price = as.numeric(data[[symbol]][, price_col]),
                       Symbol = symbol)
      colnames(df) <- c("Date", "Price", "Symbol")  # Consistent column names
      df
    }))
    
    # Select plot theme based on input
    theme_choice <- match.arg(input$plot_theme, c("theme_minimal", "theme_light", "theme_dark", "theme_classic"))
    theme_fun <- get(theme_choice)
    
    # Select color palette
    color_palette <- brewer.pal(length(unique(plot_data$Symbol)), input$color_palette)
    
    # Plot with customized options
    ggplot(plot_data, aes(x = Date, y = Price, color = Symbol, linetype = Symbol)) +
      geom_line(size = input$line_size, alpha = input$transparency) +
      scale_color_manual(values = color_palette) +
      labs(title = "Stock Prices Over Time",
           x = "Date",
           y = paste(selected_type, "Price")) +
      theme_fun() +
      theme(legend.title = element_blank())
  })
  
  # Download Plot as PNG
  output$save_plot <- downloadHandler(
    filename = function() {
      paste("stock_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      data <- stock_data()
      req(data)
      
      selected_type <- input$price_type
      price_col <- switch(selected_type,
                          "Open" = 1,
                          "High" = 2,
                          "Low" = 3,
                          "Close" = 4,
                          "Adjusted" = 6)
      
      plot_data <- do.call(rbind, lapply(names(data), function(symbol) {
        df <- data.frame(Date = index(data[[symbol]]),
                         Price = as.numeric(data[[symbol]][, price_col]),
                         Symbol = symbol)
        colnames(df) <- c("Date", "Price", "Symbol")  # Consistent column names
        df
      }))
      
      # Select plot theme based on input
      theme_choice <- match.arg(input$plot_theme, c("theme_minimal", "theme_light", "theme_dark", "theme_classic"))
      theme_fun <- get(theme_choice)
      
      # Select color palette
      color_palette <- brewer.pal(length(unique(plot_data$Symbol)), input$color_palette)
      
      # Create plot with customized options
      plot <- ggplot(plot_data, aes(x = Date, y = Price, color = Symbol, linetype = Symbol)) +
        geom_line(size = input$line_size, alpha = input$transparency) +
        scale_color_manual(values = color_palette) +
        labs(title = "Stock Prices Over Time",
             x = "Date",
             y = paste(selected_type, "Price")) +
        theme_fun() +
        theme(legend.title = element_blank())
      
      # Save the plot
      ggsave(file, plot = plot, width = 8, height = 6)
    }
  )
  
  # Download Data as CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("stock_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- stock_data()
      req(data)
      
      selected_type <- input$price_type
      price_col <- switch(selected_type,
                          "Open" = 1,
                          "High" = 2,
                          "Low" = 3,
                          "Close" = 4,
                          "Adjusted" = 6)
      
      plot_data <- do.call(rbind, lapply(names(data), function(symbol) {
        df <- data.frame(Date = index(data[[symbol]]),
                         Price = as.numeric(data[[symbol]][, price_col]),
                         Symbol = symbol)
        colnames(df) <- c("Date", "Price", "Symbol")  # Consistent column names
        df
      }))
      
      write.csv(plot_data, file, row.names = FALSE)
    }
  )
  
  # Render Stock Data Table
  output$stock_data <- renderTable({
    data <- stock_data()
    req(data)
    
    selected_type <- input$price_type
    price_col <- switch(selected_type,
                        "Open" = 1,
                        "High" = 2,
                        "Low" = 3,
                        "Close" = 4,
                        "Adjusted" = 6)
    
    # Combine data from all stocks
    plot_data <- do.call(rbind, lapply(names(data), function(symbol) {
      df <- data.frame(Date = index(data[[symbol]]),
                       Price = as.numeric(data[[symbol]][, price_col]),
                       Symbol = symbol)
      colnames(df) <- c("Date", "Price", "Symbol")  # Consistent column names
      df
    }))
    
    plot_data  # Display the entire dataset
  })
}

# Run the application
shinyApp(ui = ui, server = server)
