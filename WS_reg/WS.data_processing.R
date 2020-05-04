####FUNCTION: Climate minute data cleaner####
## transfer the data value into numeric form, remove unit's row
library(readr)
setwd("~/Desktop/academy/programing/R/St.Kitts/Database")

Dataclean = function(
  filename = ""
){
  if ((length(grep(".dat",filename))) > 0)  {
    Rawdata_min = read_csv(filename,
                           col_types = cols(AirTC_Avg = col_number(),
                                            BP_mbar_Avg = col_number(),
                                            BattV_Avg = col_number(),
                                            EC_Avg = col_number(),
                                            ETos = col_number(),
                                            HalfBr = col_number(),
                                            PTemp_C_Avg = col_number(),
                                            RECORD = col_number(),
                                            RH = col_number(),
                                            Rso = col_number(),
                                            SlrMJ_Tot = col_number(),
                                            SlrW = col_number(),
                                            T_Avg = col_number(),
                                            VWC_Avg = col_number(),
                                            WS_ms_Avg = col_number(),
                                            WS_ms_S_WVT = col_number(),
                                            WindDir_D1_WVT = col_number(),
                                            SlrkW_Avg = col_number(),
                                            Rain_mm_Tot = col_number(),
                                            WindDir_SD1_WVT = col_number()),
                           skip = 1)
    Rawdata_min = Rawdata_min[-1:-2,]
    return(Rawdata_min)
  }
  if ((length(grep(".csv",filename))) > 0){
    LaG = read.csv(filename, skip = 1)
    LaG = LaG[,-5:-7]
    colnames(LaG) = c("TIMESTAMP", "AirTC_Avg","RH","DewPointTC_Avg")
    LaG$AirTC_Avg= round(((LaG$AirTC_Avg-32)*5/9),digits = 1)
    LaG$DewPointTC_Avg= round(((LaG$DewPointTC_Avg-32)*5/9),digits = 1)
    LaG$TIMESTAMP = as.character(LaG$TIMESTAMP)
    return(LaG)
  }
}


####FUNCTION:Avg_Temperature_calculate####
library(dplyr)

Dailytemperature_calculate = function(
  start_day = "",
  end_day = "",
  filename = "",
  daysforavg = 1,
  daysfordivid = 1
){
  
  # insert "interested" day period
  day = seq(as.Date(start_day), 
            as.Date(end_day),"day") %>% 
    format(., format = "%Y-%m-%d")
  
  
  # insert Weather Station data
  Rawdata_min = Dataclean(filename = filename)
  
  # extract the date data based on "interested" day 
  day_select = function(day){
    exp_day = Rawdata_min[grep(day,Rawdata_min$TIMESTAMP),]
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
    daily_temperature[[i]] = mean(start_to_endday[[i]]$AirTC_Avg)
    daily_temperature_sd[[i]] = sd(start_to_endday[[i]]$AirTC_Avg)
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

####FUNCTION:Sunrise.Sunset_time####
library(StreamMetabolism)
library(dplyr)



SunriseSunset_time = function(
  start_day = "2019/10/7",
  end_day = "2020/4/10",
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

####FUNCTION:WS.Data_partition####
# insert self-made function

data_partition = function(
  Datasource = "",
  #Interested Data period
  start_day = "",
  end_day = "",
  # way for partition: 1 for DayNight, 2 for Monthly, 3 for both
  Partition_Way = 3
){
  
  ########                   ########
  ######## UNDERCONSTRUCTION ######## Data NA value patching might be here
  ########                   ########
  
  
  Way = switch(Partition_Way, "DayNight", "Monthly", "DayNight&Monthly")
  
  if (Way == "DayNight") {
    
    # set the data, inspected day and sunrise, sunset time
    Sriseset = SunriseSunset_time(start_day = start_day, end_day = end_day)
    #### transform timestamp into second
    # set function for each item
    datetosecond = function(time_min){
      adj.time = as.POSIXct(time_min,
                            tryFormats = c("%Y-%m-%d %H:%M:%OS"), 
                            tz ="UTC+4",
                            origin = "1970-01-01") 
      evesec = as.double(adj.time)
      return(evesec)
    }
    
    
    # apply for whole data
    timelayout = matrix(Rawdata$TIMESTAMP,nrow  = 1)
    TIMESasSECOND = apply(timelayout, 2 , datetosecond)
    
    intermediate = cbind(Rawdata,TIMESasSECOND)
    
    ##use second data to separate every day
    # sort out day
    dclmdat = c()
    for (i in 1:nrow(Sriseset)) {
      clmdatt = intermediate[which(intermediate$TIMESasSECOND >= Sriseset[i,2]),]
      dclmdat = clmdatt[which(clmdatt$TIMESasSECOND <= Sriseset[i,3]),] %>% 
        rbind(dclmdat,.)
    }
    # sort out night
    nclmdat = c()
    for (i in 1:nrow(Sriseset)-1) {
      clmdatt = intermediate[which(intermediate$TIMESasSECOND >= Sriseset[i,3]),]
      nclmdat = clmdatt[which(clmdatt$TIMESasSECOND <= Sriseset[i+1,2]),] %>% 
        rbind(nclmdat,.)
    }
    
    
    # drop the second data
    dclmdat = dclmdat[,-ncol(dclmdat)] %>% 
      as.tbl()
    nclmdat = nclmdat[,-ncol(nclmdat)]%>% 
      as.tbl()
    
    Output = list(Day = dclmdat, Night = nclmdat)
    return(Output)
  }
  
  if (Way == "Monthly"){
    
    month = seq(as.Date(start_day), 
                as.Date(end_day),"month") %>% 
      format(., format = "%Y-%m")
    
    month_select = function(month){
      exp_day = Rawdata[grep(month,Rawdata$TIMESTAMP),]
      return(exp_day)
    }
    
    Output = apply(matrix(month,nrow = 1),2,month_select) 
    names(Output) = c(month)
    return(Output)
  }
  
  if (Way == "DayNight&Monthly"){
    
    setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
    source("(Function)Sunrise-Sunset time.R")
    # set the data, inspected day and sunrise, sunset time
    Sriseset = SunriseSunset_time(start_day = start_day, end_day = end_day)
    #### transform timestamp into second
    # set function for each item
    datetosecond = function(time_min){
      adj.time = as.POSIXct(time_min,
                            tryFormats = c("%Y-%m-%d %H:%M:%OS"), 
                            tz ="UTC+4",
                            origin = "1970-01-01") 
      evesec = as.double(adj.time)
      return(evesec)
    }
    
    
    # apply for whole data
    timelayout = matrix(Rawdata$TIMESTAMP,nrow  = 1)
    TIMESasSECOND = apply(timelayout, 2 , datetosecond)
    
    intermediate = cbind(Rawdata,TIMESasSECOND)
    
    ##use second data to separate every day
    # sort out day
    dclmdat = c()
    for (i in 1:nrow(Sriseset)) {
      clmdatt = intermediate[which(intermediate$TIMESasSECOND >= Sriseset[i,2]),]
      dclmdat = clmdatt[which(clmdatt$TIMESasSECOND <= Sriseset[i,3]),] %>% 
        rbind(dclmdat,.)
    }
    # sort out night
    nclmdat = c()
    for (i in 1:nrow(Sriseset)-1) {
      clmdatt = intermediate[which(intermediate$TIMESasSECOND >= Sriseset[i,3]),]
      nclmdat = clmdatt[which(clmdatt$TIMESasSECOND <= Sriseset[i+1,2]),] %>% 
        rbind(nclmdat,.)
    }
    
    
    # drop the second data
    dclmdat = dclmdat[,-ncol(dclmdat)] %>% 
      as.tbl()
    nclmdat = nclmdat[,-ncol(nclmdat)]%>% 
      as.tbl()
    
    
    
    month = seq(as.Date(start_day), 
                as.Date(end_day),"month") %>% 
      format(., format = "%Y-%m")
    
    
    month_select_day = function(month){
      exp_day = dclmdat[grep(month,dclmdat$TIMESTAMP),]
      return(exp_day)
    }
    
    Day_Output = apply(matrix(month,nrow = 1),2,month_select_day) 
    names(Day_Output) = c(month)
    
    month_select_night = function(month){
      exp_day = nclmdat[grep(month,nclmdat$TIMESTAMP),]
      return(exp_day)
    }
    
    Night_Output = apply(matrix(month,nrow = 1),2,month_select_night) 
    names(Night_Output) = c(month)
    
    
    
    Output = list(Day = Day_Output, Nigtht = Night_Output)
    return(Output)
  }
  
}




