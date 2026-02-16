library(forecast)
library(ggplot2)

data = read.csv("portal_timeseries.csv")
NDVI_ts = ts(data$NDVI, start = c(1992,3), end = c(2014,11), frequency = 12)
tsdisplay(NDVI_ts)

NDVI_train = window(NDVI_ts, end = c(2011,11))
NDVI_test = window(NDVI_ts, start = c(2011,12))

arima_model = auto.arima(NDVI_train, seasonal = FALSE)
arima_forecast = forecast(arima_model, h=36)

autoplot(arima_forecast) + autolayer(NDVI_test)

plot(as.vector(arima_forecast$mean), as.vector(NDVI_test))
abline(0,1)

arima_seasonal_model = auto.arima(NDVI_train, seasonal = TRUE)
arima_seasonal_forecast = forecast(arima_seasonal_model , h=36,level = c(80,99))

autoplot(arima_seasonal_forecast)+ autolayer(NDVI_test)

accuracy(arima_forecast,NDVI_test)
accuracy(arima_seasonal_forecast,NDVI_test)

arima_forecast$lower
arima_forecast$upper

in_interval = NDVI_test > arima_forecast$lower[,1] & NDVI_test < arima_forecast$upper[,1]
in_interval

length(in_interval[in_interval==TRUE])/length(in_interval)

seasonal_in_interval = NDVI_test > arima_seasonal_forecast$lower[,1] & NDVI_test < arima_seasonal_forecast$upper[,1]
seasonal_in_interval

length(seasonal_in_interval[seasonal_in_interval==TRUE])/length(seasonal_in_interval)

rmses = sqrt((arima_forecast$mean - NDVI_test)^2)

seasonal_rmses = sqrt((arima_seasonal_forecast$mean - NDVI_test)^2)
plot(rmses) + lines(seasonal_rmses, col= "blue") 
