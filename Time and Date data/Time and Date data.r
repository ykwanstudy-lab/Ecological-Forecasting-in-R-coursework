library(ggplot2)
daily = read.csv("NEON_Harvardforest_date_2001_2006.csv")

class(daily$date)

head(daily$date)

daily = daily[order(daily$date),]
head(daily$date)

random_dates = c("02-15-2015","02-14-2016","02-13-2017")
random_dates = sort(random_dates)
random_dates


qplot(x=date, y=airt,data=daily,main="Daily Air Temp")
daily$formatted_date = as.Date(daily$date)
head(daily$formatted_date)
class(daily$formatted_date)

qplot(x=formatted_date,y=airt,data=daily,main="Daily Air Temp with formatted date")

test = "Aug 5 25"
format = as.Date(test, format = "%B %d %y")

datatime = read.csv("NEON_Harvardforest_datetime.csv")
View(datatime)

asDate_date = as.Date(datatime$datetime)
asDate_date

test_datetime = "2025-10-15 03:15"
asDate_datetime = as.Date(test_datetime)
asDate_datetime

unclass(asDate_datetime)


ct_datetime = as.POSIXct(test_datetime)
ct_datetime
unclass(ct_datetime)
class(ct_datetime)

lt_datetime= as.POSIXlt(test_datetime)
lt_datetime
unclass((lt_datetime))

datatime$ctdatetime = as.POSIXct(datatime$datetime, format = "%Y-%m-%dT%H:%M")

datatime$ddatetime = as.POSIXct(datatime$datetime)

library(lubridate)
View(daily)

lub = lubridate::ymd(daily$date)
head(lub)
class(lub)

test_date = "1900, July 29"
test_date = lubridate::ymd(test_date)

lub_time = lubridate::ymd_hm(datatime$datetime)
head(lub_time)

today()
today() + days(10)
