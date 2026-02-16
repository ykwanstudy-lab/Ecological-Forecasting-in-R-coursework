library(tsibble)
library(fable)
library(feasts)
library(dplyr)

# Load data and format the date column
data = read.csv("portal_timeseries.csv")
data_ts <- data |>
  mutate(month = yearmonth(date)) |>
  as_tsibble(index = month)

# Fit the model
arima_model = model(data_ts, ARIMA(NDVI))

# View the model report (Coefficients and Structure)
report(arima_model)

# Plot the model fit against the data
arima_model_aug = augment(arima_model)
autoplot(arima_model_aug, NDVI) +
  autolayer(arima_model_aug, .fitted, color = "orange")

# Check the residuals (Goal: No significant autocorrelation bars)
gg_tsresiduals(arima_model)