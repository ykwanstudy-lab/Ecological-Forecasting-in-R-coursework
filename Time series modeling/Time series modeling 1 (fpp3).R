# 1. Load the necessary packages [2, 3]
library(tsibble)
library(fable)
library(feasts)
library(dplyr)

# 2. Load and Prepare Data (Create a tsibble) [3]
# Note: Fable requires a 'yearmonth' index, not a base 'ts' object.
data <- read.csv("portal_timeseries.csv")
data_ts <- data |>
  mutate(month = yearmonth(date)) |>
  as_tsibble(index = month)

# 3. Visualizing Data (ACF/PACF) [4, 5]
# gg_tsdisplay gives you the time plot, ACF, and PACF in one view
gg_tsdisplay(data_ts, NDVI, plot_type = "partial")

# 4. Fit a White Noise / Mean Model [4, 6]
# The course uses MEAN() for the average model (equivalent to ARIMA(0,0,0))
avg_model <- data_ts |>
  model(avg = MEAN(NDVI))

avg_model_aug <- augment(avg_model)
avg_model_aug

autoplot(avg_model_aug, NDVI) + autolayer(avg_model_aug, .fitted, color = "red")

#just the lag 1 and lag 2 autocorrelation
ar_model = model(data_ts, AR(NDVI ~ order(2)))
report(ar_model)

ar_model_aug = augment(ar_model)
ar_model_aug

autoplot(ar_model_aug, NDVI) + autolayer(ar_model_aug, .fitted, color = "orange")

gg_tsresiduals(ar_model)