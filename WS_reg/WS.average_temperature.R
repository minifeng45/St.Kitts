####FUNCTION:Avg_Temperature_calculate####
library(dplyr)
source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.Data_insert.R")


Dailytemperature_calculate = function(
  start_day = "",
  end_day = "",
  filename = "",
  days.for.average = 1,
  days.for.divid = 1
){
  
  # insert "interested" day period
  day = seq(as.Date(start_day), 
            as.Date(end_day),"day") %>% 
    format(., format = "%Y-%m-%d")
  
  
  # insert Weather Station data
  Rawdata_min = Dataclean(filename = filename)
  
  # extract the date data based on "interested" day 
  day_select = function(day){
 Rawdata_min[grep(day,Rawdata_min$TIMESTAMP),]
  }
  
  # apply day_select function to extract the certain day's data
  start.to.end_day = apply(matrix(day,nrow = 1),2,function(day)Rawdata_min[grep(day,Rawdata_min$TIMESTAMP),])
  
  ### calculate the daily temperature
  # define rv.
  
  daily_temperature = numeric()
  daily_temperature_sd = numeric()
  # loop to caculate by each row
  
  for (i in 1:length(day)) {
    daily_temperature[[i]] = mean(start.to.end_day[[i]]$AirTC_Avg)
    daily_temperature_sd[[i]] = sd(start.to.end_day[[i]]$AirTC_Avg)
  }
  day_inform = data.frame(day,daily_temperature,daily_temperature_sd)
  colnames(day_inform) = c("Date","Avr_Temp","Sd_Temp")
  
  # fill the NA with average daily temperature
  
  na.pos = is.na(day_inform$Avr_Temp)
  day_inform$Avr_Temp[which(na.pos)] = 
    mean(na.omit(day_inform$Avr_Temp))
  
  
  ### calculate the mean temperature of selected term
  # define rv.
  
  j=1
  temperature = c()
  SD =c()
  
  # set a function for "including_period": how long for meaning, "divided_period": how timelapse for output
  
  
  period_temperature = function(including_period,divided_period){
    while (j < length(day)) {
      a= mean(day_inform$Avr_Temp[j:(j+including_period)])
      b = sd(day_inform$Avr_Temp[j:(j+including_period)])
      temperature = c(temperature,a)
      SD = c(SD,b)
      j = j+divided_period
    }
    return(cbind(temperature,SD))
  }
  
  
  if (daysforavg > length(day)) {
    print("Don't have that long data for averaging")
  }else{
    period_temp = period_temperature(daysforavg,daysfordivid) %>% 
      .[1:((length(.)/ncol(.))-floor(daysforavg/daysfordivid)),]
    print(paste("first column:",day[1],"to", day[1+daysforavg]))
    return(period_temp)
  }
}
