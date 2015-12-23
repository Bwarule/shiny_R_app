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
setwd("C:\\Users\\bharat.warule\\Downloads")
## https://datamarket.com/data/list/?q=interval:day provider:tsdl
daily_minimum_temperatures <- read.csv("daily-minimum-temperatures-in-me.csv")

## Change the Variable format
daily_minimum_temperatures$Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990 <- 
  as.numeric(daily_minimum_temperatures$Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990)
daily_minimum_temperatures$Date <- as.Date(daily_minimum_temperatures$Date)
daily_minimum_temperatures <- rename(daily_minimum_temperatures, Daily_minimum_temperatures_Melbourne =
                                       Daily.minimum.temperatures.in.Melbourne..Australia..1981.1990 )

## Timebstamp_timeseries used for the data processing 
Time_stamp = "Date"
# convert <- "Weekly"## c("qtr", "Monthly", "Weekly", "day", "hourly")
time_var <- "Daily_minimum_temperatures_Melbourne"
Data <- daily_minimum_temperatures

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
    
	## date_hourly remove the variable which is not useful
	Data_formated$date_hourly <- NULL
	
	## Create the variable based on weekly basis 
	convert <- "Weekly"
	date_var <- paste("date", convert, sep="_")	   

	Data_formated <- 
    Data_formated %>%
	group_by(date_Weekly) %>% 
	mutate(avg_Weekly = mean(Daily_minimum_temperatures_Melbourne),
			min_Weekly = min(Daily_minimum_temperatures_Melbourne), 
			max_Weekly = max(Daily_minimum_temperatures_Melbourne))

    ## Create the variable based on monthly basis 
	convert <- "Monthly"
	date_var <- paste("date", convert, sep="_")	   
    
	Data_formated <- 
    Data_formated %>%
	group_by(date_Monthly) %>% 
	mutate(avg_Monthly = mean(Daily_minimum_temperatures_Melbourne),
			min_Monthly = min(Daily_minimum_temperatures_Melbourne), 
			max_Monthly = max(Daily_minimum_temperatures_Melbourne))
			
	## create the variable based on lubridate function
	library(lubridate)
	Data_formated$Date <- as.Date(Data_formated$Date)
	Data_formated$wdayval <- wday(Data_formated$Date, label=FALSE)
	Data_formated$weekday_value <- 1*isWeekday(Data_formated$Date, wday=1:5)
		  
    ## Data_formated <- Data_formated[,c(date_var,time_var)]
    ## Datan <- Data_formated %>%
	##	group_by(eval(parse(text=date_var))) %>%
	##	summarise_(paste("mean(",time_var,")",sep="")) ## summarise( n = n())
	##  summarise(mean(Daily_minimum_temperatures_Melbourne)) ## summarise( n = n())
		
    ## Here are some analysis work based on above manipulation
	## split the data into two part train data and test data
	
	k <- round(0.8 * nrow(Data_formated))
    train_data <- Data_formated[  c(1:k),]
	test_data <- Data_formated[ -c(1:k),]
	vars_List <- c("avg_Weekly", "min_Weekly", "max_Weekly",
    	"avg_Monthly", "min_Monthly", "max_Monthly", "weekday_value")                     
	
	# Find ARIMAX model
	temperatures <- ts(train_data$Daily_minimum_temperatures_Melbourne, frequency=7)
    modArima <- auto.arima(temperatures, xreg=train_data[,c( "wdayval","avg_Weekly",
	"min_Weekly", "max_Weekly", "avg_Monthly", "min_Monthly", "max_Monthly",  "weekday_value")])
	ts.plot(temperatures,fitted(modArima),col=c("blue","red"))

#################################################################################################	
	Series: temperatures 
ARIMA(0,0,0) with non-zero mean 

Coefficients:
      intercept  wdayval  avg_Weekly  min_Weekly  max_Weekly  avg_Monthly  min_Monthly
         1.8891  -0.5315      0.9964      0.0010     -0.0039       0.0078       0.0602
s.e.     9.0779   0.5457      0.0518      0.0314      0.0303       0.0506       0.1699
      max_Monthly  weekday_value
           0.0144        -5.2740
s.e.       0.0369         2.4159

sigma^2 estimated as 3477:  log likelihood=-16048
AIC=32116   AICc=32116.07   BIC=32175.79

Training set error measures:
                       ME     RMSE      MAE       MPE     MAPE      MASE         ACF1
Training set 3.697324e-12 58.96544 43.73546 -74.46668 98.60148 0.6941229 -0.009509237

#################################################################################################
	
	library(forecast)
	## http://stats.stackexchange.com/questions/41070/how-to-setup-xreg-argument-in-auto-arima-in-r
	## Create matrix of numeric predictors
	# Create matrix of numeric predictors
	xreg <- cbind(Weekday=model.matrix(~as.factor(train_data$wdayval)), 
                  train_data[,vars_List])

    ## Remove intercept
    xreg <- xreg[,-1]
	
	# Rename columns
	colnames(xreg) <- c("Mon","Tue","Wed","Thu","Fri","Sat", "avg_Weekly",
	"min_Weekly", "max_Weekly", "avg_Monthly", "min_Monthly", "max_Monthly",  "weekday_value" )
    # Variable to be modelled
    temperatures <- ts(train_data$Daily_minimum_temperatures_Melbourne, frequency=7)

    # Find ARIMAX model
    modArima <- auto.arima(temperatures, xreg=xreg[,c("Mon","Tue","Wed","Thu","Fri","Sat")])
	ts.plot(temperatures,fitted(modArima),col=c("blue","red"))
	> summary( modArima)
	
#################################################################################################
	Series: temperatures 
ARIMA(2,0,1)(0,0,2)[7] with non-zero mean 

Coefficients:
         ar1      ar2      ma1    sma1     sma2  intercept     Mon      Tue       Wed      Thu
      1.1646  -0.1735  -0.9244  0.0381  -0.0279   116.3752  0.8955  -8.9304  -11.2284  -6.7574
s.e.  0.0209   0.0202   0.0089  0.0192   0.0192    10.6513  4.1537   4.5033    4.5632   4.5608
          Fri      Sat
      -5.2141  -1.7920
s.e.   4.5017   4.1509

sigma^2 estimated as 4414:  log likelihood=-16396.91
AIC=32819.82   AICc=32819.94   BIC=32897.55

Training set error measures:
                     ME     RMSE      MAE       MPE     MAPE      MASE        ACF1
Training set -0.0757094 66.43888 52.56404 -91.04653 118.1051 0.8342408 0.000524741
#################################################################################################
> 

    # Find ARIMAX model
    modArima <- auto.arima(temperatures, xreg=xreg[,c("Mon","Tue","Wed","Thu","Fri","Sat","avg_Weekly")])
	ts.plot(temperatures,fitted(modArima),col=c("blue","red"))
> summary( modArima)
Series: temperatures 
ARIMA(1,0,1)(0,0,1)[7] with non-zero mean 

Coefficients:
         ar1      ma1    sma1  intercept     Mon      Tue       Wed      Thu      Fri      Sat
      0.6947  -0.9900  0.0345     3.9149  1.1751  -8.4875  -10.5624  -1.9314  -5.7462  -1.9808
s.e.  0.0135   0.0023  0.0190     2.9384  4.5327   4.4687    4.4321   4.4304   4.4655   4.5452
      avg_Weekly
          1.0000
s.e.      0.0017

sigma^2 estimated as 2984:  log likelihood=-15825.02
AIC=31674.04   AICc=31674.14   BIC=31745.79

Training set error measures:
                     ME     RMSE      MAE      MPE     MAPE     MASE      ACF1
Training set -0.5725703 54.62091 39.47876 -62.2867 85.60683 0.626565 0.1238027

#################################################################################################
> 	

    # Find ARIMAX model
    modArima <- auto.arima(temperatures, xreg=xreg[,c( "avg_Weekly",
	"min_Weekly", "max_Weekly", "avg_Monthly", "min_Monthly", "max_Monthly",  "weekday_value")])
	ts.plot(temperatures,fitted(modArima),col=c("blue","red"))
> summary( modArima)
Series: temperatures 
ARIMA(0,0,0) with non-zero mean 

Coefficients:
      intercept  avg_Weekly  min_Weekly  max_Weekly  avg_Monthly  min_Monthly  max_Monthly
        -0.2164      0.9964      0.0010     -0.0039       0.0076       0.0593       0.0145
s.e.     8.8182      0.0518      0.0314      0.0303       0.0506       0.1699       0.0369
      weekday_value
            -5.2753
s.e.         2.4163

sigma^2 estimated as 3478:  log likelihood=-16048.47
AIC=32114.95   AICc=32115.01   BIC=32168.76

Training set error measures:
                       ME     RMSE      MAE       MPE     MAPE      MASE        ACF1
Training set 3.675858e-13 58.97502 43.73869 -74.53641 98.67826 0.6941742 -0.00920735
################################################################################################# 
	## http://robjhyndman.com/hyndsight/dailydata/
	z <- fourier(ts(temperatures, frequency=365.25), K=7)
	zf <- fourierf(ts(temperatures, frequency=365.25), K=7, h=nrow(xreg_test_data))
	fit <- auto.arima(temperatures, 
	xreg=cbind(z,xreg[,c("Mon","Tue","Wed","Thu","Fri","Sat","avg_Weekly")]), seasonal=FALSE)
	summary(fit)
	xreg_test_data <- cbind(Weekday=model.matrix(~as.factor(test_data$wdayval)), 
                  test_data[,vars_List])
	colnames(xreg_test_data) <- c("Mon","Tue","Wed","Thu","Fri","Sat", "avg_Weekly",
	"min_Weekly", "max_Weekly", "avg_Monthly", "min_Monthly", "max_Monthly",  "weekday_value" )			  
	fc <- forecast(fit, xreg=cbind(zf,
	xreg_test_data[,c("Mon","Tue","Wed","Thu","Fri","Sat","avg_Weekly")]), h=nrow(xreg_test_data))
	plot(fc)
	Series: temperatures 
ARIMA(1,0,0) with non-zero mean 

Coefficients:
         ar1  intercept    S1-365    C1-365  S2-365  C2-365  S3-365   C3-365  S4-365  C4-365
      0.2032   111.6852  -21.7614  -50.2444  6.6803  4.3545  5.1846  -3.6021  3.2824  2.1097
s.e.  0.0181     1.5165    2.1440    2.1451  2.1437  2.1448  2.1432   2.1443  2.1425  2.1436
      S5-365  C5-365
      0.3732  0.2962
s.e.  2.1416  2.1427

sigma^2 estimated as 4265:  log likelihood=-16346.12
AIC=32718.25   AICc=32718.37   BIC=32795.98

Training set error measures:
                       ME     RMSE      MAE       MPE     MAPE      MASE         ACF1
Training set 0.0006581043 65.30323 51.00724 -91.55753 118.3695 0.8095328 -0.003044162
> 

### Assume above approach may not help us then select the every daywise data 
## then forecast
	Data_formated$Fwdayval <- wday(Data_formated$Date, label=TRUE)
	
	require(tidyr)
	Data_formatednew <- Data_formated[,c("Daily_minimum_temperatures_Melbourne","Fwdayval")]
	Data_formatednew <- 
	Data_formatednew %>% spread(key="Fwdayval", value="Daily_minimum_temperatures_Melbourne",drop=TRUE)
	Data_formatednew

	names(Data_formatednew)
    ## [1] "Sun"   "Mon"   "Tues"  "Wed"   "Thurs" "Fri"   "Sat"  
 
	Sun_ts <- ts(Data_formatednew$Tues[!(is.na(Data_formatednew$Tues))])
	plot(forecast(auto.arima(Sun_ts)))
		
	## month of day level forecast
	Data_formated$mday_key <- mday(Data_formated$Date)
	Data_formatednew <- Data_formated[,c("Daily_minimum_temperatures_Melbourne","mday_key")]
	Data_formatednew <- 
	Data_formatednew %>% spread(key="mday_key", value="Daily_minimum_temperatures_Melbourne",drop=TRUE)
	Data_formatednew

	names(Data_formatednew) <- paste("Day",names(Data_formatednew),sep="")
	Day_ts <- ts(Data_formatednew$Day2[!(is.na(Data_formatednew$Day2))],frequency=12) ## 
	plot(forecast(auto.arima(Day_ts)))
	plot(forecast(ets(Day_ts,s.window=7)))
    plot(forecast(stlm(Day_ts,s.window=7)))
	
	Day_ts <- ts(Data_formatednew$Day3[!(is.na(Data_formatednew$Day3))],frequency=12) ## 
	plot(forecast(auto.arima(Day_ts,lambda=BoxCox.lambda(Day_ts))))
	



    
		
