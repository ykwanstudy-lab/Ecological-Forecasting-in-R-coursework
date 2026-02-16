# Load the necessary packages first
library(tsibble)   # Contains the as_tsibble function
library(fable)     # Contains the model functions (TSLM, ARIMA)
library(feasts)    # Contains visualization tools
library(dplyr)     # Helpful for data manipulation (often used with |> pipes)


# Load the abundance data
pp_data = read.csv("pp_abundance_timeseries.csv")|>
as_tsibble(index = newmoonnumber)

# Visualize the response variable
gg_tsdisplay(pp_data, abundance)

# Fit a TSLM with climate drivers
tslm_model = model(pp_data, TSLM(abundance ~ mintemp))
report(tslm_model)

tslm_model_aug = augment(tslm_model)
autoplot(tslm_model_aug, abundance) +
  autolayer(tslm_model_aug, .fitted, color = "orange")

#Update code by adding cool_precip
tslm_model = model(pp_data, TSLM(abundance ~ mintemp + cool_precip))
report(tslm_model)

tslm_model_aug = augment(tslm_model)
autoplot(tslm_model_aug, abundance) +
  autolayer(tslm_model_aug, .fitted, color = "orange")


# Check the residuals
gg_tsresiduals(tslm_model)

#Add time itself as a predictor
tslm_model = model( pp_data,TSLM(abundance ~ mintemp + cool_precip + trend()))
report(tslm_model)

gg_tsresiduals(tslm_model)

#ARIMA model to specify external covariates 
arima_exog_model <- model(pp_data, ARIMA(abundance ~ mintemp))
report(arima_exog_model)

arima_exog_model_aug <- augment(arima_exog_model)
autoplot(arima_exog_model_aug, abundance) +
  autolayer(arima_exog_model_aug, .fitted, color = "orange")
gg_tsresiduals(arima_exog_model)

#ARIMA model with mintemp + cool_precip
arima_exog_model <- model(pp_data, ARIMA(abundance ~ mintemp+ cool_precip))
report(arima_exog_model)

arima_exog_model_aug <- augment(arima_exog_model)
autoplot(arima_exog_model_aug, abundance) +
  autolayer(arima_exog_model_aug, .fitted, color = "orange")
gg_tsresiduals(arima_exog_model)
