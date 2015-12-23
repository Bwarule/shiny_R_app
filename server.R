## create the Data for time stamp application 
## time series model development
library(forecast)
library(dplyr)
library(xts)
library(shiny)

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com
# Create the sample data logic
## This M-Competition data
## more gereral data library library(Mcomp)

## Data repository 
## setwd("C:\\Users\\bharat.warule\\Downloads")
daily_minimum_temperatures <- read.csv("daily-minimum-temperatures-in-me.csv")

## Change the Variable format
daily_minimum_temperatures$Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990 <- 
  as.numeric(daily_minimum_temperatures$Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990)
daily_minimum_temperatures$Date <- as.Date(daily_minimum_temperatures$Date)
daily_minimum_temperatures <- rename(daily_minimum_temperatures, Daily_minimum_temperatures_Melbourne =
                                       Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990 )

## Timebstamp_timeseries used for the data processing 
## Time_stamp = "Date"
## convert <- "Monthly"## c("qtr", "Monthly", "Weekly", "day", "hourly")
time_var <- "Daily_minimum_temperatures_Melbourne"
Data <- daily_minimum_temperatures


source("Timestamp_timeseries_new.R")

shinyServer(function(input, output) {
  
  # By declaring datasetInput as a reactive expression we ensure 
  # that:
  #
  #  1) It is only called when the inputs it depends on changes
  #  2) The computation and result are shared by all the callers 
  #	  (it only executes a single time)
  #
  datasetInput <- reactive({
    ## input <- list(period = "day")
    eventdata <-  Timestamp_timeseries(Time_stamp="Date",
                                       Data ,
                                       convert = as.character(input$period),
                                       time_var= "Daily_minimum_temperatures_Melbourne")
    print(eventdata)
    eventdata
    
  })
  
  
  # The output$view depends on both the databaseInput reactive
  # expression and input$obs, so will be re-executed whenever
  # input$dataset or input$obs is changed. 
  output$view <- renderTable({
    datasetInput()
  })
  
  output$distPlot <- renderPlot({
    
    # forecasting code procedure will run 
    # please set the all input information from ui
    eventdata <- datasetInput()
    
    # Plot the forecasting plot based on the statistical model
    ## This in general model for : monthly, qtr, annual series
    plot(forecast(auto.arima(eventdata)))
    ## daywise, weekly seires are different model
    # plot(forecast(HoltWinters(eventdata,seasonal = "multiplicative")))
    
    
  })
  
})
