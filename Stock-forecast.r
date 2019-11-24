
##Predicted price for tomorrow & in a week
#install.packages("quantmod", repos = "http://cran.us.r-project.org")
#install.packages("forecast", repos = "http://cran.us.r-project.org")
#install.packages("rlang", repos = "http://cran.us.r-project.org")
library("quantmod")
library("forecast")
library("rlang")



#search for the stock
searchsymbol=Sys.getenv("R_STOCK")
#searchsymbol <- "AAPL"
symbol_result<-getSymbols(searchsymbol,src="yahoo",auto.assign = FALSE)
#symbol_result
stock_ts <-ts(symbol_result)

data_length <- nrow(stock_ts)
plot(stock_ts[,4])
stock_ts_window <- window(stock_ts, start= (data_length),end= data_length)
stock_ts_val<- stock_ts_window[,4]
#plot(stock_ts_val)

stock_ts_val<-stock_ts[,4]
stock_auto_arima<-auto.arima(stock_ts_val, seasonal = "TRUE")
#summary(stock_auto_arima)
lines(stock_auto_arima$fitted, col="blue")
forecast_stock <- forecast(stock_auto_arima, 7)
plot(forecast_stock)
#predict_point <- c(data_length+1,data_length+2,data_length+3,data_length+4,data_length+5)
#forecast(predict_point, h=5, model=stock_auto_arima)
#accuracy(forecast_stock)
Tomorrow_predict <- forecast_stock$mean[1]
week_predict <- forecast_stock$mean[6]
Tomorrow_predict
week_predict







