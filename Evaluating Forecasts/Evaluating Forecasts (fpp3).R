# 1. Setup: Load packages [1]
library(tsibble)
library(fable)
library(feasts)
library(dplyr)
library(ggplot2)

# 2. Load the data [1]
# Note: Ensure the file path matches your directory structure
portal_data = read.csv("portal_timeseries.csv") |>
  mutate(month = yearmonth(date)) |>
  as_tsibble(index = month)

# 3. Evaluating forecast model fits [1]
# Fit multiple models to a single dataset to compare them
portal_models = model(portal_data,
                      arima = ARIMA(NDVI),
                      tslm = TSLM(NDVI),
                      arima_exog = ARIMA(NDVI ~ rain)
)

portal_models
glance(portal_models)

# 4. Hindcasting: Create Test and Training Data [2, 3]
# Filter data to create a training set (before Dec 2011) and a test set (after Dec 2011)
train <- filter(portal_data, month < yearmonth("2011 Dec"))
test <- filter(portal_data, month >= yearmonth("2011 Dec"))

# 5. Build model on training data [3]
# Fit a non-seasonal MA2 model (Moving Average order 2)
ma2_model = model(train, ARIMA(NDVI ~ pdq(0, 0, 2) + PDQ(0, 0, 0)))

# 6. Make forecast for the test data [4]
# Forecast 36 months into the future to match the test data length
ma2_forecast = forecast(ma2_model, h = 36)

# 7. Visual Evaluation [4, 5]
# Plot the forecast against the training data
autoplot(ma2_forecast, train)

# Add the observed test data (NDVI) to see how well the forecast performed
autoplot(ma2_forecast, train) +
  autolayer(test, NDVI)

# Add the model fitted values (blue line) to see how it fit the training data
autoplot(ma2_forecast, train) +
  autolayer(test, NDVI) +
  autolayer(augment(ma2_model), .fitted, color = "blue")

# 8. Quantitative Evaluation (Point Estimates) [6]
# Calculate accuracy metrics (like RMSE) by comparing the forecast to the test data
accuracy(ma2_forecast, test)

# 9. Compare Multiple Models [7]
# Fit both the MA2 model and a full Auto-ARIMA model to the training data
models = model(train,
               ma2 = ARIMA(NDVI ~ pdq(0, 0, 2) + PDQ(0, 0, 0)),
               arima = ARIMA(NDVI)
)

# Forecast both models
forecasts = forecast(models, test)

# Visualize the comparison
autoplot(forecasts, train, level = 50, alpha = 0.75) +
  autolayer(test, NDVI)

# Compare accuracy metrics
accuracy(forecasts, test)

# Evaluating Uncertainty: Coverage
# Extract 80% prediction intervals for the MA2 model
ma2_intervals <- hilo(ma2_forecast, level = 80) |>
  unpack_hilo("80%")

lower <- ma2_intervals$`80%_lower`
upper <- ma2_intervals$`80%_upper`

# Check if observations fall within the interval
observation <- test$NDVI
in_interval <- observation > lower & observation < upper

# Calculate coverage (proportion of TRUE values)
length(in_interval[in_interval == TRUE]) / length(in_interval)

# Challenge: Evaluate Coverage for the Seasonal ARIMA model
# Fit, forecast, and calculate coverage for the full ARIMA model
arima_model = model(train, ARIMA(NDVI))
arima_forecast = forecast(arima_model, test)

arima_intervals <- hilo(arima_forecast, level = 80) |>
  unpack_hilo("80%")

in_interval = test$NDVI > arima_intervals$`80%_lower` & 
  test$NDVI < arima_intervals$`80%_upper`

length(in_interval[in_interval == TRUE]) / length(in_interval)

#Scores Incorporating Uncertainty
accuracy(forecasts, test, list(winkler = winkler_score), level = 80)
accuracy(forecasts, test, list(winkler = winkler_score, crps = CRPS), level = 80)
accuracy(forecasts, test, list(winkler = winkler_score, crps = CRPS, mae = MAE), level = 80)

accuracies = accuracy(
  forecasts,
  test,
  list(winkler = winkler_score, crps = CRPS, mae = MAE),
  level = 80,
  by = c(".model", "month")
)
accuracies

ggplot(data = accuracies, mapping = aes(x = month, y = crps, color = .model)) +
  geom_line()

print(accuracies, n = 72)
