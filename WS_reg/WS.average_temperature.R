####FUNCTION:Avg_Temperature_calculate####
#specific to weather station data (dat)
library(dplyr)
library(lubridate)
source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.Data_insert.R")


Dailytemperature_calculate = function(
  start_day = "",
  end_day = "",
  filename = "",
  including.period = 1,
  divided.period = 1
){
  
  # insert "interested" day period
  day = seq(as.Date(start_day), 
            as.Date(end_day),"day") %>% 
    format(., format = "%Y-%m-%d") %>%
    ymd()
  
  # insert Weather Station data
  Rawdata_min = Dataclean(filename = "WS.Needsmust.Min.record.dat")
  
  month_information = Rawdata_min %>%
    mutate(month = month(Rawdata_min$TIMESTAMP),
           year = year(Rawdata_min$TIMESTAMP))%>%
    group_by(year,month) %>%
    summarise(monthly.average = mean(AirTC_Avg),
              sd = sd(AirTC_Avg))
  
  daily_information = Rawdata_min %>%
    mutate(month = month(Rawdata_min$TIMESTAMP),
           year = year(Rawdata_min$TIMESTAMP),
           day = day(Rawdata_min$TIMESTAMP))%>%
    group_by(year,month, day ) %>%
    summarise(daily.average = mean(AirTC_Avg),
              sd = sd(AirTC_Avg))
  # extract the date data based on "interested" day
  # apply day_select function to extract the certain day's data
  start.to.end_day = lapply(day,function(day)Rawdata_min[grep(day,Rawdata_min$TIMESTAMP),])
  


  
  
  ### calculate the mean temperature of selected term
  # define rv.
  
  # set a function for "including_period": how long for meaning, "divided_period": how timelapse for output
  
    j=1
    temperature = c()
    day.period = c()
    while (j < length(day)) {
      if((day[j]+ days(including.period)) > ymd(end_day)){
        break
      }
      else{
        temperature[j]= mean(daily_information$daily.average[j:(j+including.period)])
        day.period[j]  = paste(ymd(day[j]),(ymd(day[j])+ days(including.period)),sep="-") 
      }
      j = j+divided.period
    }
    output = data.frame(day.span =day.period, temperature.average =  temperature) %>%
      filter(!is.na(day.span),!is.na(temperature.average)) 
      return(output)
  }
