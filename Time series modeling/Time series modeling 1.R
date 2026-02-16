library(forecast)

set.seed(20)

whitenoise = ts(rnorm(273,mean=0.18))

data= read.csv("portal_timeseries.csv", stringsAsFactors = FALSE)
NDVI.ts= ts(data$NDVI, start=c(1992,3), end=c(2014,11), frequency = 12)
rain.ts= ts(data$rain, start=c(1992,3), end=c(2014,11), frequency = 12)

avg_model_w = forecast::meanf(whitenoise)
plot(whitenoise)
lines(fitted(avg_model_w), col="blue")

avg_model = forecast::meanf(NDVI.ts)
plot(NDVI.ts)
lines(fitted(avg_model), col="blue")

forecast::tsdisplay(NDVI.ts)

MA2 = forecast::Arima(NDVI.ts, c(0,0,2))
plot(NDVI.ts)
lines(fitted(MA2), col="blue")

summary(MA2)

forecast::checkresiduals(MA2)

autoarima = forecast::auto.arima(NDVI.ts)
summary(autoarima)
forecast::checkresiduals(autoarima)
autoarima_3 = forecast::auto.arima(NDVI.ts, max.P = 3)
#c(p,d,q), seasonal= c(P,D,Q)
#default max c(5,2,5), seasonal=c(2,1,2)

summary(autoarima_3)
forecast::checkresiduals(autoarima_3)

autoarima_rain = forecast::auto.arima(rain.ts)
summary(autoarima_rain)
forecast::checkresiduals(autoarima_rain)

rain_arima = forecast::auto.arima(NDVI.ts, max.P = 3, xreg = rain.ts)
summary(rain_arima)
forecast::checkresiduals(rain_arima)

plot(NDVI.ts)
lines(fitted(autoarima_3), col="green")
lines(fitted(rain_arima), col="purple")