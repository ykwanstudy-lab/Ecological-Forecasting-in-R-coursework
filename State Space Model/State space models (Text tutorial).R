# =========================================================
# Tutoial 1: State-Space Modeling with RJAGS
# Data: Google Flu Trends (Florida)
# =========================================================

# 1. Load Packages and Data
library(rjags)

# Load data directly from the course repository
gflu = read.csv("https://raw.githubusercontent.com/EcoForecast/EF_Activities/master/data/gflu_data.txt", skip=11)
time = as.Date(gflu$Date)
y = gflu$Florida

# Visualize the data
plot(time, y, type='l', ylab="Flu Index", lwd=2, log='y')

# 2. Define the JAGS Model
# This includes the Data Model (Gaussian observation error) 
# and the Process Model (Random Walk: x[t] ~ x[t-1])
RandomWalk = "
model{
  #### Data Model
  for (t in 1:n){
    y[t] ~ dnorm(x[t], tau_obs)
  }
  
  #### Process Model
  for (t in 2:n){
    x[t] ~ dnorm(x[t-1], tau_proc)
  }
  
  #### Priors
  x[1] ~ dnorm(x_ic, tau_ic)
  tau_obs ~ dgamma(a_obs, r_obs)
  tau_proc ~ dgamma(a_proc, r_proc)
}
"

# 3. Prepare Data and Initialization for JAGS
# Organize data into a list
data <- list(y=y, 
             n=length(y), 
             x_ic=1000, tau_ic=1, 
             a_obs=1, r_obs=1, 
             a_proc=1, r_proc=1)

# Set initial parameter values to help convergence
init <- list(list(tau_proc=1/var(diff(y)), tau_obs=5/var(y)))

# 4. Run the Model
# Compile the model
j.model <- jags.model(file = textConnection(RandomWalk), 
                      data = data, 
                      inits = init, 
                      n.chains = 1)

# Burn-in (run iterations to discard early samples)
jags.out <- coda.samples(model = j.model, 
                         variable.names = c("tau_proc","tau_obs"), 
                         n.iter = 10000)
# Check convergence (optional)
plot(jags.out) 

# 5. Generate Posterior Samples
# Now sample the latent state 'x'
jags.out <- coda.samples(model = j.model, 
                         variable.names = c("x","tau_proc","tau_obs"), 
                         n.iter = 10000)

# 6. Visualize Estimates
# Convert output to matrix
out <- as.matrix(jags.out)
xs <- out[,3:ncol(out)] # Extract state variables

# Calculate means and 95% Credible Intervals
predictions <- colMeans(xs)
ci <- apply(xs, 2, quantile, c(0.025, 0.975))

# Plot results
plot(time, predictions, type = "l", main = "JAGS State-Space Model")
lines(time, ci[1,], lty = "dashed", col = "blue")
lines(time, ci[2,], lty = "dashed", col = "blue")

# =========================================================
# R Tutorial Part 2: Missing Data & Dynamic Linear Models
# Prerequisite: This script assumes you have already loaded 
# the 'gflu' data and fit the basic Random Walk model from Part 1.
# =========================================================

# ---------------------------------------------------------
# 1. Handling Missing Data (Imputation)
# ---------------------------------------------------------
# Bayesian state-space models can handle missing data (NA) by estimating 
# the values as latent states.

# Introduce missing data: Random individual weeks
data$y[c(26, 50, 90, 260, 261, 262)] = NA

# Introduce missing data: An entire year (2008)
library(lubridate) # Needed for year() function if not loaded
data$y[year(time) == 2008] = NA

# Re-run the JAGS model (assuming 'j.model' and 'variable.names' are defined as in Part 1)
# Note: You would typically re-compile the model here with the modified 'data' list
# jags.out <- coda.samples(model = j.model, variable.names = c("y","tau_proc","tau_obs"), n.iter = 10000)

# ---------------------------------------------------------
# 2. Evaluating Forecast Error
# ---------------------------------------------------------
# Analyze how error changes with forecast horizon using the hindcast data 
# (last 52 weeks) established in the previous tutorial.

# Identify indices for the forecast period (last 52 weeks)
forecast_data = (length(y)-51):length(y)

# Plot Root Mean Squared Error (RMSE) over the forecast horizon
# Note: 'predictions' must be defined from your model output (colMeans of latent state x)
plot(time[forecast_data], sqrt((predictions[forecast_data] - y[forecast_data])^2),
     ylab = "Forecast Error (RMSE)", xlab = "Time")

# ---------------------------------------------------------
# 3. Dynamic Linear Modeling (Adding Covariates)
# ---------------------------------------------------------
# Improve the model by adding external drivers (weather).

# Install and load package for Daymet weather data
# install.packages('daymetr')
library(daymetr)

# Download weather data for Orlando, FL
weather = download_daymet(site = "Orlando",
                          lat = 28.54,
                          lon = -81.34,
                          start = 2003,
                          end = 2016,
                          internal = TRUE)
weather_data = weather$data

# Format dates to match Google Flu data
weather_data$date = as.Date(paste(weather_data$year, weather_data$yday, sep = "-"), "%Y-%j")
weather_data = subset(weather_data, weather_data$date %in% time)

# Add covariates to the data list
# Tmin: Minimum temperature
# yday: Julian day (seasonality)
data$Tmin = weather_data$tmin..deg.c.
data$yday = weather_data$yday

# Log transform the response variable (flu index) for better fitting
data$logy = log(data$y)

# ---------------------------------------------------------
# 4. Fitting the DLM with ecoforecastR
# ---------------------------------------------------------
# Use the ecoforecastR package to generate JAGS code for Dynamic Linear Models

library(ecoforecastR)

# Fit Model 1: Random Walk + Minimum Temperature
# 'obs' is the observed data, 'fixed' defines the regression formula
dlm = fit_dlm(model = list(obs = "logy", fixed = "~ 1 + X + Tmin"), data)

# Inspect the generated JAGS code
cat(dlm$model)

# Check convergence (remove burn-in first)
params = dlm$params
params <- window(dlm$params, start = 1000)
plot(params, ask = TRUE) # Optional visualization

# Visualize Forecasts (transforming back from log scale)
out <- as.matrix(dlm$predict)
ci <- apply(exp(out), 2, quantile, c(0.025, 0.5, 0.975))

plot(time, y, main = "DLM with Temperature")
lines(time, ci[1,], col = "red")       # Median
lines(time, ci[2,], lty = "dashed")    # Lower CI
lines(time, ci[3,], lty = "dashed")    # Upper CI

# ---------------------------------------------------------
# 5. Adding Seasonality (Julian Day)
# ---------------------------------------------------------
# Fit Model 2: Random Walk + Temperature + Julian Day
dlm = fit_dlm(model = list(obs = "logy", fixed = "~ 1 + X + Tmin + yday"), data)

# Visualize improved forecast
out <- as.matrix(dlm$predict)
ci <- apply(exp(out), 2, quantile, c(0.025, 0.5, 0.975))

plot(time, y, main = "DLM with Temp + Seasonality")
lines(time, ci[1,], col = "red")
lines(time, ci[2,], lty = "dashed")
lines(time, ci[3,], lty = "dashed")
