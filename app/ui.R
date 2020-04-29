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
    sidebarPanel(column(width = 4, 
                        sliderInput(inputId = "p",label = "AR component",value = 0,min = 0,max = 4),
                        sliderInput(inputId = "d",label = "Number of Differences",value = 0,min = 0,max = 4),
                        sliderInput(inputId = "q",label = "MA component",value = 1,min = 0,max = 3)),
                 column(width = 4,
                        sliderInput(inputId = "seasP",label = "Seasonal AR component",value = 0,min = 0,max = 4),
                        sliderInput(inputId = "seasD",label = "Seasonal number of Differences",value = 0,min = 0,max = 4),
                        sliderInput(inputId = "seasQ",label = "Seasonal MA component",value = 1,min = 0,max = 4)),
                 sliderInput(inputId = "year_forecast", label = "Number of months for forecasting",value = 6,min = 1,max = 12,step = 1),
                 actionButton("arima_calculate", "Calculate"),
                 width = 4),
    
    mainPanel(tabsetPanel(
      type = "tabs",
      tabPanel("Input Values",
               column(width = 4,verbatimTextOutput("AR"),verbatimTextOutput("Diff"),verbatimTextOutput("MA")),
               column(width = 4,verbatimTextOutput("SAR"),verbatimTextOutput("SDiff"),verbatimTextOutput("SMA")),
               column(width = 5, verbatimTextOutput("MAPE.Train"),verbatimTextOutput("MAPE.Test"))
               ),
      tabPanel("Summary Output", column(width = 8,verbatimTextOutput("summary_output"))),
      tabPanel("Residual Plot", plotOutput("plota", width = 600)),
      tabPanel("Forecast", plotOutput("plotb", width = 600)),
      tabPanel("VAR",
               tabsetPanel(
                 type = "tabs",
                 tabPanel("VAR Plot", plotOutput("plotc", width = 600)),
                 tabPanel("Linear Model Summary", verbatimTextOutput("var_summary")),
                 tabPanel("Auto ARIMA fit Summary", verbatimTextOutput("var_summary1")),
                 tabPanel("ARIMA Summary", verbatimTextOutput("var_summary2")),
                 tabPanel("VAR Residual Plot", plotOutput("plotd", width = 600))
                          )
               )
    ))
  )
)
