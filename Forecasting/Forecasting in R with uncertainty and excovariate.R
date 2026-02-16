# 1. Setup: Load packages
library(tsibble)
library(fable)
library(feasts)
library(dplyr)

# 2. Load the data and convert to tsibble
pp_data = read.csv("pp_abundance_by_month.csv") |>
  mutate(month = yearmonth(month)) |>
  as_tsibble(index = month)

# View the data object
pp_data

# 3. Exploratory Analysis: Visualize the time series
gg_tsdisplay(pp_data, abundance)

# 4. Fit a simple AR(1) model
# pdq(1,0,0) sets Autoregressive order to 1
# PDQ(0,0,0) forces seasonal components to 0
ar1_model = model(pp_data, ARIMA(abundance ~ pdq(1,0,0) + PDQ(0,0,0)))

# View model details
report(ar1_model)

# 5. Make Forecasts
# Default forecast (usually 2 seasonal cycles)
ar1_forecast = forecast(ar1_model)
ar1_forecast

# Change forecast horizon to 20 time steps
ar1_forecast = forecast(ar1_model, h = 20)

# 6. Visualize Forecasts
# Plot just the forecast
autoplot(ar1_forecast)

# Plot forecast combined with the original data
autoplot(ar1_forecast, pp_data)

# 7. Explore Uncertainty with Bootstrapping
# Run a single simulation
ar1_forecast = forecast(ar1_model, bootstrap = TRUE, times = 1)
autoplot(ar1_forecast, pp_data)

# Run 1000 simulations to visualize uncertainty (prediction intervals)
ar1_forecast = forecast(ar1_model, bootstrap = TRUE, times = 1000)
autoplot(ar1_forecast, pp_data)

# 8. Adjust Prediction Intervals
# Change confidence levels to 50% and 80%
ar1_forecast = forecast(ar1_model)
autoplot(ar1_forecast, pp_data, level = c(50, 80))

# 9. Forecasting with External Co-variates (ARIMAX)
# Fit model using Minimum Temperature (mintemp) as a driver
arima_exog_model = model(pp_data, ARIMA(abundance ~ mintemp))
report(arima_exog_model)

# Load future climate data for the forecast period
climate_forecasts = read.csv("pp_future_climate.csv") |>
  mutate(month = yearmonth(month)) |>
  as_tsibble(index = month)

climate_forecasts

# Forecast utilizing the new climate data
arima_exog_forecast = forecast(arima_exog_model, new_data = climate_forecasts)

# Visualize the ARIMAX forecast
autoplot(arima_exog_forecast, pp_data)
