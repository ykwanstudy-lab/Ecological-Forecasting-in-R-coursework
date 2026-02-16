# 1. Load packages
# Note: loading dismo may mask select from dplyr. Use dplyr::select if errors occur.
library(dismo)
library(ggplot2)
library()
library(terra)

# 2. Load Data
# Load species presence/absence data
hooded_warb_data = read.csv("hooded_warb_locations.csv")
head(hooded_warb_data)

# Load environmental raster data
env_data_current = rast("env_current.grd")
env_data_forecast = rast("env_forecast.grd")

# Visualize environmental layers
plot(env_data_current$tmin)
plot(env_data_current$precip)

# 3. Determine environment where species is (and isn't)
# Extract environmental data at the sampled locations
# Note: If this line fails, use dplyr::select(hooded_warb_data, lon, lat)
hooded_warb_data = env_data_current |>
  terra::extract(dplyr::select(hooded_warb_data, lon, lat)) |>
  bind_cols(hooded_warb_data)

# Visualize species occurrence in 2D environmental space
ggplot(hooded_warb_data, aes(x = tmin, y = precip, color = present)) +
  geom_point()

# 4. Modeling species distribution
# Build a multivariate logistic regression (GLM)
logistic_regr_model <- glm(present ~ tmin + precip,
                           family = binomial(link = "logit"),
                           data = hooded_warb_data)
summary(logistic_regr_model)

# 5. Plot the model predictions
# Make predictions using the environmental raster
predictions <- predict(env_data_current, logistic_regr_model, type = "response")

# Identify locations where the species is present for plotting
present_loc <- dplyr::select(filter(hooded_warb_data, present == 1), lon, lat)

# Plot probabilities
plot(predictions, ext = extent(-140, -50, 25, 60))
points(present_loc, pch = '+', cex = 0.5)

# Plot with a 50% threshold
plot(predictions > 0.5, ext = extent(-140, -50, 25, 60))
# Note: 'present' in the line below likely refers to 'present_loc' defined earlier
points(present, pch = '+', cex = 0.5)

# Plot with a 25% threshold
plot(predictions > 0.25, ext = extent(-140, -50, 25, 60))
points(present_loc, pch = '+', cex = 0.5)

# 6. Evaluate model performance (ROC Curves)
# Split data into presences and absences
presence_data = filter(hooded_warb_data, present == 1)
absence_data = filter(hooded_warb_data, present == 0)

# Run evaluation function from dismo
evaluation <- evaluate(presence_data, absence_data, logistic_regr_model)

# Plot ROC curve
plot(evaluation, 'ROC')

# 7. Automatically choosing thresholds
# Calculate threshold based on prevalence
thresh <- threshold(evaluation, stat = 'prevalence')

# Plot using the calculated threshold
plot(predictions > thresh, ext = extent(-140, -50, 25, 60))
points(present_loc, pch = '+', cex = 0.5)

# 8. Make forecasts
# Predict using the future climate data (CMIP5)
forecasts <- predict(env_data_forecast, logistic_regr_model, type = "response")

# Plot forecasts
plot(forecasts, ext = extent(-140, -50, 25, 60))

# Plot forecasts with threshold
# Note: 'tr' in the source text below is a typo for 'thresh' calculated above
plot(forecasts > tr, ext = extent(-140, -50, 25, 60))

# Plot the difference (Change expected to occur)
plot(forecasts - predictions, ext = extent(-140, -50, 25, 60))
