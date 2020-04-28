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
    
    
    output$AR <- renderPrint({
      paste0("Value of input AR : ",input$p)
      # paste0("Value of input d",input$d)
      # paste0("Value of input q",input$q)
    })
    
    output$Diff <- renderPrint({
       paste0("Value of input Diff : ",input$d)
      # paste0("Value of input q",input$q)
    })
    
    
    output$MA <- renderPrint({
      paste0("Value of input MA : ",input$q)
      # paste0("Value of input d",input$d)
      # paste0("Value of input q",input$q)
    })
    
    
    output$summary_output <- renderPrint({
      arima_value()
    })
    
    #checkresiduals(arima100)
    
    output$plota<- renderPlot({
      checkresiduals(arima_value())
      })
    
    output$plotb<- renderPlot({
      plot(forecast(arima_value(),h=input$year_forecast))
    })
    
  
  })
}