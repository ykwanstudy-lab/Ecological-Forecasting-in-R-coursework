install.packages('rjags')
install.packages("devtools")
devtools::install_github('EcoForecast/ecoforecastR')
install.packages('daymetr')

library(rjags)
library(lubridate)
library(daymetr)
library(ecoforecastR)

gflu = read.csv("https://raw.githubusercontent.com/EcoForecast/EF_Activities/master/data/gflu_data.txt", skip=11)
time = as.Date(gflu$Date)
y = gflu$Florida
#plot(time,y,type='l')
plot(time,y,type='l',ylab="Flu Index",lwd=2,log='y')

random_walk = "
model{

  ####Observation model
  for(t in 1:n){
  y[t] ~dnorm(x[t],tau_obs)
  }
  
  ####Process model
  for (t in 2:n){
  x[t] ~ dnorm(x[t-1], tau_proc)
  }
  
  ####Priors
  x[1] ~ dnorm(x_ic,tau_ic)
  tau_obs ~ dgamma(a_obs,r_obs)
  tau_proc ~ dgamma(a_proc, r_proc)
  
}
"

data = list(
  y= y,
  n= length(y),
  x_ic = 1000,
  tau_ic= 1,
  a_obs= 1,
  r_obs= 1,
  a_proc= 1,
  r_proc= 1
)


data$y[(length(y)-51):length(y)]= NA
data$y[c(26,50,90,260,261,262)] = NA
data$y[year(time)== 2008]= NA

init = list(list(tau_proc= 1/var(diff(y)), tau_obs= 5/var(y)))

j_model= jags.model(file= textConnection(random_walk),
                    data= data,
                    init= init,
                    n.chains= 1)

jags_out= coda.samples(model= j_model,
                       variable.names = c("y","tau_proc","tau_obs"),
                       n.iter = 10000)
plot(jags_out)

output= as.matrix(jags_out)
xs= output[,3:ncol(output)]

predictions = colMeans(xs)
plot(time, predictions, type= "l")

points(time,y)

pred_interval= apply(xs,2,quantile, c(0.025,0.975))
lines(time,pred_interval[1,],lty= "dashed", col= "blue")
lines(time,pred_interval[2,],lty= "dashed", col= "red")

hist(1/sqrt(output[,1]))
hist(1/sqrt(output[,2]))
plot(1/sqrt(output[,1]),1/sqrt(output[,2]))

forecast_window= (length(y)-51):length(y)
plot(time[forecast_window],
     sqrt((predictions[forecast_window]-y[forecast_window])^2),
     lwd= 5)

weather = download_daymet(site= "Orlando",
                          lat= 28.54,
                          lon= -81.34,
                          start= 2003,
                          end= 2016,
                          internal= TRUE)

weather_data= weather$data
weather_data$date= as.Date(paste(weather_data$year,
                                 weather_data$yday,
                                 sep= "-"),"%Y-%j")

weather_data= subset(weather_data, weather_data$date %in% time)

data$Tmin= weather_data$tmin..deg.c.
data$yday= weather_data$yday
data$logy= log(data$y)

dlm= fit_dlm(model= list(obs= "logy", fixed= "~ 1 + X + Tmin + yday"), data)
cat(dlm$model)

output= as.matrix(dlm$predict)
pred_interval= apply(exp(output),2,quantile,c(0.025,0.5,0.975))
plot(time, y, lwd= 5)
lines(time, pred_interval[2,], lwd= 5)
lines(time, pred_interval[1,], lwd= 5, col= "blue", lty= "dotted")
lines(time, pred_interval[3,], lwd= 5, col= "red", lty= "dotted")
