library(shiny)
library(shinythemes)

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
        label = "select the 'p'",
        value = 1,
        min = 0,
        max = 9
      ),
      sliderInput(
        inputId = "d",
        label = "select the 'd'",
        value = 1,
        min = 0,
        max = 9
      ),
      sliderInput(
        inputId = "q",
        label = "select the 'q'",
        value = 1,
        min = 0,
        max = 9
      ),
      actionButton("arima_calculate", "Calculate"),
      width = 3
      
    ),
    
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel("Summary Output",
               column(
                 width = 8,
                 verbatimTextOutput("summary_output")
               )),
      tabPanel("Residual Plot", plotOutput("plota", width = 600)),
      tabPanel("Forecast", plotOutput("plotb", width = 600))
    ))
  )
)
