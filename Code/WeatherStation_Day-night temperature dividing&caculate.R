## Day, Night temperature Separation
# Insert climate data(minute)
setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("WeatherStation_Min_data cleaner.R")         


setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")

# display the time of sunset and sunrise of trial period
source("Sunrise-Sunset time.R")
# Data cleaning
dat = Rawdata_min[,c(1,5,14)] #remain time, temperature, isolation.MJ
rm(Rawdata_min)

# TIMESTAMP transfer to second(numeric)
datetosecond = function(time_min){
  adj.time = as.POSIXct(time_min,
                        tryFormats = c("%Y-%m-%d %H:%M:%OS"), 
                        tz ="UTC+4",
                        origin = "1970-01-01") 
  evesec = as.double(adj.time)
  return(evesec)
}

timelayout = matrix(dat$TIMESTAMP,nrow  = 1)
TIMESasSECOND = apply(timelayout, 2 , datetosecond)

#bind on second data
dat = cbind(dat,TIMESasSECOND)



##use second data to separate every day
# sort out day
dclmdat = c()
for (i in 1:nrow(sunriset_second)) {
  clmdatt = dat[which(dat$TIMESasSECOND >= sunriset_second[i,1]),]
  clmdatt = clmdatt[which(clmdatt$TIMESasSECOND <= sunriset_second[i,2]),]
  dclmdat = rbind(dclmdat,clmdatt)
}
# sort out night
nclmdat = c()
for (i in 1:nrow(sunriset_second)-1) {
  clmdatt = dat[which(dat$TIMESasSECOND >= sunriset_second[i,2]),]
  clmdatt = clmdatt[which(clmdatt$TIMESasSECOND <= sunriset_second[i+1,1]),]
  nclmdat = rbind(nclmdat,clmdatt)
}
head = dat[which(dat$TIMESasSECOND <= sunriset_second[1,1]),] #complement the head data 
nclmdat = rbind(head,nclmdat)

rm(head,timelayout,sunriset_second,i,TIMESasSECOND,datetosecond,clmdatt)


## Calculate the daily night temperature 
# mean calculate function
date_temperature = function(day){
  ndate = nclmdat[grep(day,nclmdat$TIMESTAMP),]                   #grab the specific date
  nmean=  mean(ndate$AirTC_Avg)                                   #calculate the mean
  ddate = dclmdat[grep(day,dclmdat$TIMESTAMP),]  
  dmean=  mean(ddate$AirTC_Avg)
  return(c(nmean,dmean))
}

#make a matrix for all array for apply
timeperiod = matrix(day,nrow  = 1)

#calculate the daily mean
daily_mean = apply(timeperiod, 2, date_temperature)
daily_temperature = as.data.frame(t(daily_mean)) #t(): from 2 row to 2 column
colnames(daily_temperature) = c("Day","Night")

rm(daily_mean,timeperiod,dat)


