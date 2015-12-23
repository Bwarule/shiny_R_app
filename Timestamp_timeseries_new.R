Timestamp_timeseries <- function(Time_stamp="Time_stamp",Data,convert,time_var){
  
  require(dplyr)
  require(zoo)
  
  ## Type_serv : It is kind of services offered 
  ## Time_stamp : it is time at which costumer enter in the DMV
  ## convert into the Monthly, Weekly, day wise, Hourly series.
  
  time_vec <- Data[,Time_stamp]
  date_qtr <- as.yearqtr(time_vec)
  date_Monthly <-  as.yearmon(time_vec, "%b %Y")
  date_Weekly <-  format(time_vec, '%Y.%W')
  date_day <- as.POSIXct(strptime(time_vec, "%Y-%m-%d"))
  date_hourly <-  as.POSIXct(strptime(time_vec, "%Y-%m-%d %H"))
  # format(time, '%Y-%m-%d %H')
  ## as.POSIXct(strptime("2010-10-31 01:30:00", "%Y-%m-%d %H"))
  
  Data_formated <- Data %>%
    mutate(date_hourly = date_hourly,
           date_day = date_day,
           date_Weekly = date_Weekly,
           date_Monthly= date_Monthly,
           date_qtr = date_qtr)
  date_var <- paste("date", convert, sep="_")	   
  
  ## Data_formated <- Data_formated[,c(date_var,time_var)]
  Datan <- Data_formated %>%
    group_by(eval(parse(text=date_var))) %>%
    summarise_(paste("mean(",time_var,")",sep="")) ## summarise( n = n())
  ## summarise(mean(Daily_minimum_temperatures_Melbourne)) ## summarise( n = n())
  
  names(Datan)   <- c(date_var,time_var) 
  
  if(convert == "Monthly"){
    date1 <- as.yearmon(as.character(Datan$date_Monthly)[[1]], "%b %Y")
    Month <- as.numeric(format(date1, "%m"))
    Year <- as.numeric(format(date1, "%Y"))
    eventdata <- 
      ts( Datan[,time_var], frequency=12, start=c(Year,Month))
    return(eventdata)
  }
  
  if(convert == "qtr"){
    date1 <- as.yearqtr(as.character(Datan$date_qtr)[[1]])
    qtr_num <- as.numeric(format(date1, format = "0%q"))
    Year <- as.numeric(format(date1, "%Y"))
    eventdata <- 
      ts( Datan[,time_var], frequency=4, start=c(Year,qtr_num))
    return(eventdata)
  }
  
  if(convert == "Weekly"){
    week_num <- as.numeric(
      unlist(strsplit(Datan$date_Weekly[[1]],"\\."))[[2]])
    week_year <- as.numeric(
      unlist(strsplit(Datan$date_Weekly[[1]],"\\."))[[1]])
    eventdata <-  ts( Datan[,time_var], frequency=52, start=c(week_year,week_num))
    ## ts( Datan$n, frequency=52, start=c(week_year,week_num))
  }else{
    ## eventdata <- xts( Datan$n, order.by = Datan$date_qtr)
    eventdata <- xts( Datan[,time_var], order.by =  eval(parse(text=paste("Datan$",date_var,sep=""))))
    if(convert == "day"){
      eventdata <- msts(as.numeric(eventdata),seasonal.periods=c(7,365.25))
    }else{
      ## This is for hourly series
      ## http://robjhyndman.com/hyndsight/seasonal-periods/
      eventdata <- msts(as.numeric(eventdata),seasonal.periods=c(7,7*24,365.25))
    }
    
  }
  
  return(eventdata)
  
}
