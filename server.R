source('packages.R')

amazon = read.csv(paste0(my_directory, "AMZN.csv"))

amazon.vol <- amazon %>%
  select (Volume)

amazon.vol.ts <- as.ts(amazon.vol)



server <- function(input, output) {
  
  observeEvent(input$arima_calculate, {
    #arima100 <- arima(amazon.vol.ts,order=c(input$p,input$d,input$q))
    
    arima_value <- reactive({
      
      p <- input$p
      
      d <- input$d
      
      q <- input$q
      
      arima100 <- arima(amazon.vol.ts, order = c(p, d, q)) 
      arima100
    })
    
    output$summary_output <- renderPrint({
      arima_value()
    })
    
    #checkresiduals(arima100)
    
    output$plota<- renderPlot({
      checkresiduals(arima_value())
      })
    
    output$plotb<- renderPlot({
      plot(forecast(arima_value(),h=30))
    })
    
  
  })
}