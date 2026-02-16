data = read.csv("portal_timeseries.csv",stringsAsFactors = FALSE)

data$as.date = as.Date(data$date, format = "%m/%d/%Y")

data = data[order(data$as.date),]

min_date = min(data$as.date)

max_date = max(data$as.date)

NDVI.ts = ts(data$NDVI, start = c(1992,3), end = c(2014,11), frequency = 12)

head(NDVI.ts)

plot(NDVI.ts, xlab="Year",ylab="greeness")


install.packages(c("fpp3", "slider", "dplyr"))
install.packages("forecast")

library(forecast)
MA_m13 = forecast::ma(NDVI.ts, order=13, centre=TRUE)
plot(NDVI.ts)
lines(MA_m13, col="blue", lwd=3)

seasonal_add = NDVI.ts - MA_m13
plot(seasonal_add)

seasonal_multi = NDVI.ts/MA_m13
plot(seasonal_multi)

fit_add = decompose(NDVI.ts, type = "additive")
plot(fit_add)

#Season Trend Decomposition Using Loess

fit_stl = stl(NDVI.ts, s.window=10)
plot(fit_stl)
