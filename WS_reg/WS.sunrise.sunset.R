####FUNCTION:Sunrise.Sunset_time####
library(StreamMetabolism)
library(dplyr)

SunriseSunset_time = function(
  start_day = "",
  end_day = "",
  latitude = 17.15,
  longitude = -62.40
){
  # give date information
  day = seq(as.Date(start_day), 
            as.Date(end_day),"day") %>% 
    format(., format = "%Y-%m-%d")
  
  # return the sunrise and set time as second(the very beginning is 1970-01-01)
  ss = function(day){
    individual = sunrise.set(latitude, longitude, day, timezone="UTC+4")
    return(c(individual$sunrise,individual$sunset))
  }
  
  timeperiod = matrix(day,nrow  = 1)
  
  sunriset_second = t(apply(timeperiod,2,ss))

  inform = data.frame(day,sunriset_second)
  colnames(inform) = c("Date","Sunrise","Sunset")
  return(inform)
}