source('packages.R')

amazon = read.csv(paste0(my_directory, "AMZN.csv"))

amazon$month <- as.factor(month.abb[(month(amazon$Date))])

levels(amazon$Months)

amazon$year <- as.factor(year(amazon$Date))

amazon <- amazon %>% 
  mutate(Months = fct_relevel(month,"Jan",
                              "Feb",
                              "Mar",
                              "Apr",
                              "May",
                              "Jun",
                              "Jul",
                              "Aug",
                              "Sep",
                              "Oct",
                              "Nov",
                              "Dec"))

amazon.vol <- amazon %>% 
  filter(year!=2020 & year!=2015) %>% 
  select (Volume) 

amazon.vol.ts <- ts(amazon.vol,frequency = 12, start = c(2016,1))

trainData <-  window(amazon.vol.ts, start=2016, end=c(2018,12))

testData  <-  window(amazon.vol.ts, start=2019, end=c(2019,12))


# ma1.100 <- arima(trainData, order=c(0,0,1), season=list(order=c(1,0,0), period=12))
# ma1.100.fc <- forecast(ma1.100,h=12)
# 
# plot(ma1.100.fc)
# lines(testData, col="red")
# legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("Test Data","ARIMA Predicted"))
# 
# forecasted.val11 <-forecast(ma1.100.fc,h=12)
# 
# model1.accuracy11 <- accuracy(forecasted.val11,testData)
# 
# model1.accuracy11 <- data.frame(model1.accuracy11)
# 
# model1.train.MAPE11 <- model1.accuracy11[1, 'MAPE']
# model1.test.MAPE11 <- model1.accuracy11[2, 'MAPE']


server <- function(input, output) {
  
  observeEvent(input$arima_calculate, {
    #arima100 <- arima(amazon.vol.ts,order=c(input$p,input$d,input$q))
    
    arima_value <- reactive({
      
      p <- input$p
      
      d <- input$d
      
      q <- input$q
      
      seasP <- input$seasP
      
      seasD <- input$seasD
      
      seasQ <- input$seasQ
      
      arima000 <- arima(trainData, order=c(p,d,q), season=list(order=c(seasP,seasD,seasQ), period=12)) 
      arima000
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
    
    
    output$SAR <- renderPrint({
      paste0("Value of input Seasonal AR : ",input$seasP)
      # paste0("Value of input d",input$d)
      # paste0("Value of input q",input$q)
    })
    
    output$SDiff <- renderPrint({
      paste0("Value of input Seasonal Diff : ",input$seasD)
      # paste0("Value of input q",input$q)
    })
    
    
    output$SMA <- renderPrint({
      paste0("Value of input Seasonal MA : ",input$seasQ)
      # paste0("Value of input d",input$d)
      # paste0("Value of input q",input$q)
    })
    
    
    
    
    output$MAPE.Train <- renderPrint({
        
       ma1.100.fc <- forecast(arima_value(),h=input$year_forecast)
      
       forecasted.val11 <-forecast(ma1.100.fc,h=input$year_forecast)
       
       model1.accuracy11 <- accuracy(forecasted.val11,testData)
       
       model1.accuracy11 <- data.frame(model1.accuracy11)
       
       model1.train.MAPE11 <- model1.accuracy11[1, 'MAPE']
       #model1.test.MAPE11 <- model1.accuracy11[2, 'MAPE']
       
       paste0("Value of train MAPE: ",model1.train.MAPE11)
      
    })
    
    output$MAPE.Test <- renderPrint({
      
      ma1.100.fc <- forecast(arima_value(),h=input$year_forecast)
      
      forecasted.val11 <-forecast(ma1.100.fc,h=input$year_forecast)
      
      model1.accuracy11 <- accuracy(forecasted.val11,testData)
      
      model1.accuracy11 <- data.frame(model1.accuracy11)
      
      #model1.train.MAPE11 <- model1.accuracy11[1, 'MAPE']
      model1.test.MAPE11 <- model1.accuracy11[2, 'MAPE']
      
      paste0("Value of test MAPE: ",model1.test.MAPE11)
      
    })
    
    
    output$summary_output <- renderPrint({
      arima_value()
    })
    
    #checkresiduals(arima100)
    
    output$plota<- renderPlot({
      checkresiduals(arima_value())
      })
    
    output$plotb<- renderPlot({
      plot(forecast(arima_value(), h=input$year_forecast))
      lines(testData, col="red")
      legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("Test Data","ARIMA Predicted"))
    })
    
  
  })

  }