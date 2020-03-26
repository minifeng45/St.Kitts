## Day, Night temperature Separation
# Insert climate data(minute)
setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("WeatherStation_Min_data cleaner.R")        #import r.v 

# Insert HOBE_Min_data(minute)
setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("HOBO_Min_data cleaner.R")           
 
WeatherStation = Rawdata_min[,c(1,5,6,14)]
Senorcollate = merge.data.frame(HOBO_Rawdata_min123,WeatherStation, by = "TIMESTAMP")

# All data
weather_reg = lm(Senorcollate$`LA3T(DegC)`~Senorcollate$AirTC_Avg, data = Senorcollate)

coeff=coefficients(weather_reg)
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))

plot(Senorcollate$AirTC_Avg,Senorcollate$`LA3T(DegC)` , main = eq)
abline((weather_reg), 
       col = "red", lwd=3)
summary(lm(Senorcollate$`LA3T(DegC)`~Senorcollate$AirTC_Avg))