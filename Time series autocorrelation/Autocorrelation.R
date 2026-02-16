set.seed(20)
whitenoise = ts(rnorm(273,mean=0.18))
plot(whitenoise, main="White noise")
abline(h=0.18)

data = read.csv("portal_timeseries.csv")
NDVI.ts = ts(data$NDVI, start=c(1992,3), end=c(2014,11), frequency=12)

plot(NDVI.ts, xlab="Year", ylab="greenness", main="NDVI")
abline(h=0.18)

lag.plot(NDVI.ts, lags= 12, do.lines=FALSE)
lag.plot(whitenoise, lags=12, do.lines = FALSE)

acf(NDVI.ts)
acf(whitenoise)

rats.ts = ts(data$rodents, start=c(1992,3), end=c(2014,11), frequency = 12)
lag.plot(rat.ts, lags = 12, do.lines = FALSE)
acf(rats.ts)

pacf(rats.ts)

rain.ts = ts(data$rain, start=c(1992,3), end=c(2014,11), frequency = 12)
lag.plot(rain.ts, lags = 12, do.lines = FALSE)

library(forecast)

forecast::tsdisplay(rats.ts)
forecast::tsdisplay(NDVI.ts)
forecast::tsdisplay(rain.ts)

install.packages(c("fpp3", "slider", "dplyr"))


library(astsa)

astsa::lag2.plot(rain.ts, NDVI.ts, 12)
ccf(rain.ts, NDVI.ts)

astsa::lag2.plot(NDVI.ts, rats.ts, 12)
ccf(NDVI.ts, rats.ts)
