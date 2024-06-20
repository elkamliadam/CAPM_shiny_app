library(shiny)
library(shinythemes)
library(quantmod)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyquant)
library(xts)
library(broom)
library(plotly)
library(gridExtra)
library(stargazer)
library(jtools)

# The UI for application
ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("Portfolio analysis with the CAPM and market models", windowTitle = "Portfolio Analysis"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("tickers", "Enter Stock tickers (comma separated)", "AAPL, MSFT, GOOG"),
      textInput("weights", "Enter Portfolio weights (comma separated)", "0.4, 0.4, 0.2"),
      dateInput("start_date", "Start Date", value = "2020-01-01"),
      dateInput("end_date", "End Date", value = Sys.Date()),
      actionButton("analyze", "Analyse", class = "btn btn-danger")
    ),
    
    mainPanel(
      fluidRow(
        column(width = 6,
               plotlyOutput("capmPlot")
        ),
        column(width = 6,
               plotlyOutput("marketmodelPlot")
        )
      ),
      fluidRow(
        column(width = 12, 
               tags$br()  # Add space between the plots and the paragraphs
        )
      ),
      fluidRow(
        column(width = 6, 
               uiOutput("capmParagraph")
        ),
        column(width = 6, 
               uiOutput("marketModelParagraph")
        )
      ),
      fluidRow(
        column(width = 6,
               tags$div(style = "background-color: #2C3E50; padding: 10px; border-radius: 5px; margin-top: 15px;",
                        h4("CAPM Regression Analysis", style = "color: white;"),
                        verbatimTextOutput("capmRegressionOutput")
               )
        ),
        column(width = 6,
               tags$div(style = "background-color: #2C3E50; padding: 10px; border-radius: 5px; margin-top: 15px;",
                        h4("Market Model Regression Analysis", style = "color: white;"),
                        verbatimTextOutput("marketModelRegressionOutput")
               )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output){
  observeEvent(input$analyze, {
    tickers <- strsplit(input$tickers, ",\\s*")[[1]]
    weights <- as.numeric(strsplit(input$weights, ",\\s*")[[1]])
    start_date <- input$start_date
    end_date <- input$end_date
    
    if (length(tickers) != length(weights)){
      showModal(modalDialog(
        title = "Error", 
        "The number of tickers must match the number of weights."
      ))
      return(NULL)
    }
    if(sum(weights) != 1){
      showModal(modalDialog(
        title = "Error",
        "The sum of the weights does not equal to 1."
      ))
      return(NULL)
    }
    
    get_stock_data <- function(ticker){
      tq_get(ticker, from = start_date, to = end_date, get = "stock.prices")
    }
    
    stock_data <- lapply(tickers, get_stock_data)
    names(stock_data) <- tickers
    
    combined_data <- do.call(merge, lapply(stock_data, function(x) {
      stock_xts <- xts(x$adjusted, order.by = as.Date(x$date))
      Cl(to.monthly(stock_xts))
    }))
    combined_data <- na.omit(combined_data)
    
    monthly_returns <- Return.calculate(combined_data)
    portfolio_returns <- Return.portfolio(monthly_returns, weights = weights)
    portfolio_returns <- portfolio_returns[-1,]
    
    spy_data <- get_stock_data("SPY")
    spy_xts <- xts(spy_data$adjusted, order.by = as.Date(spy_data$date))
    spy_monthly <- to.monthly(spy_xts, OHLC = FALSE)
    
    # Calculate SPY monthly returns
    spy_returns <- Return.calculate(spy_monthly)
    spy_returns <- spy_returns[-1,]
    
    getSymbols("DGS3MO", src = "FRED", from = start_date, to = end_date, auto.assign = TRUE)
    rf_data <- DGS3MO
    rf_data <- xts(rf_data, order.by = index(rf_data)) 
    rf_data <- to.monthly(rf_data, OHLC = FALSE)
    rf_data <- (1 + rf_data[,1] / 100)^(1 / 12) - 1 # Convert to monthly rate
    rf_data <- rf_data[-1,]
    
    returns_data <- merge(portfolio_returns, spy_returns, rf_data)
    colnames(returns_data) <- c("port.ret", "spy.ret", "rf")
    returns_data <- as.data.frame(returns_data)
    
    returns_data <- returns_data %>% mutate(
      exret = port.ret - rf,
      exmkt = spy.ret - rf)
    
    capm_model <- lm(exret ~ exmkt, data = returns_data)
    market_model <- lm(port.ret ~ spy.ret, data = returns_data)
    
    output$capmPlot <- renderPlotly({
      p <- ggplot(returns_data, aes(x = exmkt, y = exret)) +
        geom_point(color = "black") +
        geom_smooth(method = "lm") +
        labs(
          title = "Capital Asset Pricing Model",
          x = "Excess market returns",
          y = "Excess Portfolio returns"
        ) +
        theme_minimal() +
        theme(
          text = element_text(color = "black"),
          plot.background = element_rect(fill = "lightgray"),
          panel.background = element_rect(fill = "lightgray"),
          panel.grid.major = element_line(color = "gray"),
          panel.grid.minor = element_line(color = "gray")
        )
      ggplotly(p)
    })
    
    output$marketmodelPlot <- renderPlotly({
      p <- ggplot(returns_data, aes(x = spy.ret, y = port.ret)) +
        geom_point(color = "black") +
        geom_smooth(method = "lm") +
        labs(title = "Market Model",
             x = "SPY return",
             y = "Portfolio return") +
        theme_minimal() +
        theme(
          text = element_text(color = "black"),
          plot.background = element_rect(fill = "lightgray"),
          panel.background = element_rect(fill = "lightgray"),
          panel.grid.major = element_line(color = "gray"),
          panel.grid.minor = element_line(color = "gray")
        )
      ggplotly(p)
    })
    
    output$capmParagraph <- renderUI({
      tags$div(style = "background-color: #0073B7; padding: 10px; border-radius: 5px;",
               h4("CAPM", style = "font-weight: bold;"),
               p(paste("The Alpha of this portfolio under the CAPM is",
                       round(coef(capm_model)[1], 4), "which means that the manager of this portfolio has a return of ",
                       round(coef(capm_model)[1]*100, 4),"% "),tags$br(),
                 p("The Beta of this portfolio under the CAPM is",
                   round(coef(capm_model)[2], 4), "which means that if the market moves by 1%, this portfolio will move by",
                   round(coef(capm_model)[2], 4),"%"
                 ))
               
      )
    })
    
    output$marketModelParagraph <- renderUI({
      tags$div(style = "background-color: #0073B7; padding: 10px; border-radius: 5px;",
               h4("Market Model", style = "font-weight: bold;"),
               p(paste("The Alpha of this portfolio under the market model is",
                       round(coef(market_model)[1], 4), "which means that the manager of this portfolio has a return of ",
                       round(coef(market_model)[1]*100, 4),"% "),tags$br(),
                 p("The Beta of this portfolio under the market model is",
                   round(coef(market_model)[2], 4), "which means that if the market moves by 1%, this portfolio will move by",
                   round(coef(market_model)[2], 4),"%"
                 ))
      )
    })
    
    output$capmRegressionOutput <- renderPrint({
      cat("CAPM Regression Model Summary:\n")
      print(summ(capm_model))
    })
    
    output$marketModelRegressionOutput <- renderPrint({
      cat("Market Model Regression Model Summary:\n")
      print(summ(market_model))
    })
  })
}

shinyApp(ui = ui, server = server)
