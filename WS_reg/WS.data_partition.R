####FUNCTION:WS.Data_partition####
# insert self-made function
source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.data_insert.R")

data_partition = function(
  Datasource = "",
  #Interested Data period
  start_day = "",
  end_day = "",
  # way for partition: 1 for DayNight, 2 for Monthly, 3 for both
  Partition_Way = 3
){
  
  Rawdata = Dataclean(filename = Datasource)
  
  
  ########                   ########
  ######## UNDERCONSTRUCTION ######## Data NA value patching might be here
  ########                   ########
  
  
  Way = switch(Partition_Way, "DayNight", "Monthly", "DayNight&Monthly")
  
  if (Way == "DayNight") {
    
    # insert self-made function
    source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.sunrise.sunset.R")
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
    
    # insert self-made function
    source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.sunrise.sunset.R")
    
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
