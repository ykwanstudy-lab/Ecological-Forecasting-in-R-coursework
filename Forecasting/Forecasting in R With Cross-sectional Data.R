library(dplyr)
library(forecast)
library(lubridate)

data = read.csv("portal_timeseries.csv")
data$date = as.Date(data$date, format = "%m/%d/%Y")

monsoon_data = data %>%
  mutate(month = month(date), year = year(date))%>%
  filter(month %in% c(7,8,9)) %>%
  group_by(year) %>%
  summarize(monsoon_rain = sum(rain),monsoon_NDVI = mean(NDVI))

plot(monsoon_data$monsoon_rain, monsoon_data$monsoon_NDVI)

rain_model = lm("monsoon_NDVI~monsoon_rain", data = monsoon_data)

rain_forecast = forecast(rain_model,newdata = data.frame(monsoon_rain = c(120,226,176,244)))
plot(rain_forecast)
