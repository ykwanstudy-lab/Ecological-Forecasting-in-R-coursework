install.packages(c('fable', 'feasts', 'tsibble', 'dplyr', 'ggplot2'))

library(tsibble)
library(fable)
library(feasts)
library(dplyr)
library(forecast)

data = read.csv("portal_timeseries.csv")
data$date = as.Date(data$date, format = "%m/%d/%Y")
NDVI_ts=ts(data$NDVI,start = c(1992,3),end = c(2014,11), frequency = 12)

plot(NDVI_ts)
acf(NDVI_ts)

avg_model = arima(NDVI_ts, c(0,0,0))
str(avg_model)

avg_forecast = forecast(avg_model)
str(avg_forecast)

avg_forecast$mean

avg_forecast = forecast(avg_model,48,level = c(50,95))
avg_forecast$mean

plot(NDVI_ts)
lines(avg_forecast$mean, col="pink")
      
plot(avg_forecast)
autoplot(avg_forecast)

arima_model = auto.arima(NDVI_ts,seasonal = FALSE)
arima_forecast = forecast(arima_model)
plot(arima_forecast)


arima_model = forecast::auto.arima(NDVI_ts, seasonal = FALSE)
summary(arima_model)

seasonal_arima_model = auto.arima(NDVI_ts, seasonal = TRUE)
seasonal_arima_forecast = forecast(seasonal_arima_model, h=36, level = c(80,99))
plot(seasonal_arima_forecast)

plot(NDVI_ts)
lines(fitted(arima_model), col="green")