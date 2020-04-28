library(shiny)
library(shinythemes)
source('packages.R')

amazon = read.csv(paste0(my_directory, "AMZN.csv"))

ui <- fluidPage(
  #The theme
  theme = shinytheme("united"),
  
  # App title
  titlePanel("Amazon Shares Volume"),
  
  #Sidebar layout with input definitions
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "p",
        label = "Select the AR component",
        value = 1,
        min = 0,
        max = 9
      ),
      sliderInput(
        inputId = "d",
        label = "Select the number of Differences",
        value = 0,
        min = 0,
        max = 9
      ),
      sliderInput(
        inputId = "q",
        label = "Select the MA component",
        value = 0,
        min = 0,
        max = 9
      ),
      sliderInput(
        inputId = "year_forecast",
        label = "Select the number of months for forecasting",
        value = 6,
        min = 1,
        max = 12,
        step = 1
      ),
      actionButton("arima_calculate", "Calculate"),
      width = 3
      
    ),
    
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel("Summary Output",
               column(width = 4,verbatimTextOutput("AR")),
               column(width = 4,verbatimTextOutput("Diff")),
               column(width = 4,verbatimTextOutput("MA")),
               column(width = 8,verbatimTextOutput("summary_output")
               )),
      tabPanel("Residual Plot", plotOutput("plota", width = 600)),
      tabPanel("Forecast", plotOutput("plotb", width = 600))
    ))
  )
)
