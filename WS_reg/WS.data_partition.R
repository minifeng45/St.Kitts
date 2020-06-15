####FUNCTION:WS.Data_partition####
# insert self-made function
source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg//WS.data_insert.R")
library(lubridate)
library(dplyr)


data_partition = function(
  Datasource = "",
  #Interested Data period
  start_day = "",
  end_day = "",
  # way for partition: 1 for DayNight, 2 for Monthly 
  Partition_Way = 1
){
  
  Rawdata = Dataclean(filename = Datasource)
  Way = switch(Partition_Way, "DayNight", "Monthly","9:00-17:00")
  
  if (Way == "DayNight") {
    
    # insert self-made function
    source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.sunrise.sunset.R")
    # set the data, inspected day and sunrise, sunset time
    Sriseset = SunriseSunset_time(start_day = start_day, end_day = end_day)
    #### transform timestamp into second
    # set function for each item
    
    Rawdata_with_second = Rawdata %>%
      mutate(TIMESasSECOND = as.POSIXct(Rawdata$TIMESTAMP, tryFormats = c("%Y-%m-%d %H:%M:%OS"), tz ="UTC+4", origin = "1970-01-01") %>% 
               as.double())

    ##use second data to separate every day
    # sort out day
    dclmdat = c()
    for (i in 1:nrow(Sriseset)) {
      dclmdat = Rawdata_with_second %>% 
        dplyr::filter(TIMESasSECOND >= Sriseset[i,2], TIMESasSECOND <= Sriseset[i,3]) %>%
        rbind(dclmdat,.)
    }
    # sort out night
    nclmdat = c()
    for (i in 1:(nrow(Sriseset)-1)) {
      nclmdat = Rawdata_with_second %>%
        dplyr::filter(TIMESasSECOND >= Sriseset[i,3], TIMESasSECOND <= Sriseset[i+1,2]) %>%
        rbind(nclmdat,.) 
    }
    
    Output = list(Day = as.tbl(dclmdat), Night = as.tbl(nclmdat))
    return(Output)
  }
  
  if (Way == "9:00-17:00"){
    Output = Rawdata %>%
      filter(hour(TIMESTAMP)>=9,hour(TIMESTAMP)<17)
    return(Output)
  }
  if (Way == "Monthly"){
    
    month = seq(as.Date(start_day), 
                as.Date(end_day),"month") %>% 
      format(., format = "%Y-%m")
    ##Function: separate month
    month_select = function(x){
      Rawdata %>%
        filter(grepl(x,TIMESTAMP))%>%
      return()
    }
    
    Output = lapply(month,month_select) 
    names(Output) = c(month)
    return(Output)
  }
}
