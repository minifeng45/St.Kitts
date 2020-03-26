### experiment date to second

setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("Inspection date.R")
library(StreamMetabolism)


## sunrise, sunset time

ss = function(day){
  individual = sunrise.set(17.15, -62.40, day, timezone="UTC+4")
  return(c(individual$sunrise,individual$sunset))
}


timeperiod = matrix(day,nrow  = 1)

sunriset_second = t(apply(timeperiod,2,ss))



for (i in 1:ncol(sunriset_second)) {
  if (i == 1) {
    print("Sunrise")
  }else{
    print("Sunset")
  }
  for (j in 1:nrow(sunriset_second)) {
    print(as.POSIXct(c(sunriset_second[j,i]), origin = "1970-01-01"))
  }
}
# date transfer into second
rm(timeperiod,i,j,ss)
