#### HOBO_data sort out
library(tidyverse)
#insert inspection date
setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("Inspection date.R")
#insert HOBO sensor data
setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("HOBO_Min_data cleaner.R")
#insert harvest day
setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("Sweetpepper_data_insert.R")


# extract the date data based on "inspection" day
day_select = function(day){
  exp_day = Needmust_Rawdata_min[grep(day,Needmust_Rawdata_min$TIMESTAMP),]
  return(exp_day)
}

# apply day_select function to extract the certain day's data
start_to_endday = apply(matrix(day,nrow = 1),2,day_select)

### calculate the daily temperature
# define rv.

daily_temperature = numeric()
temperature_sd = numeric()
daily_temperature_sd = numeric()
# loop to caculate by each row
for (i in 1:length(day)) {
  daily_temperature[[i]] = mean(start_to_endday[[i]]$`NeedmustT(DegC)`)
  daily_temperature_sd[[i]] = sd(start_to_endday[[i]]$`NeedmustT(DegC)`)
}
day_inform = data.frame(day,daily_temperature,daily_temperature_sd)
colnames(day_inform) = c("Date","Avr_Temp","Sd_Temp")

### calculate the mean temperature of selected term
# define rv.

j=1
temperature = c()
SD =c()

# fill the NA by the yesterday and tomorrow daily temperature

na.pos = is.na(day_inform$Avr_Temp)
day_inform$Avr_Temp[which(na.pos)] = 
  mean(na.omit(day_inform$Avr_Temp))

# set a function for total day we want to include as "including_period", and how long for meaning

month_temperature = function(including_period,divided_period){
  while (j < length(day)) {
    a= mean(day_inform$Avr_Temp[j:(j+including_period)])
    b = sd(day_inform$Avr_Temp[j:(j+including_period)])
    temperature = c(temperature,a)
    SD = c(SD,b)
    j = j+divided_period
  }
  return(cbind(temperature,SD))
}

###########################
######parameter type:######
###########################
month_raw = month_temperature(34,7)
mouth_temp= month_raw[1:((length(month_raw)/ncol(month_raw))-floor(34/7)),]

