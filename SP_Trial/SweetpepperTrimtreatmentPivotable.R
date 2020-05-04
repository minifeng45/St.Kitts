##### trim data Pviot Table
#source raw data 

setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("Sweetpepper_data_insert.R")


#### PivotTable caculate function establishment
#for purple
weekly_totalweight = function(
  date = NULL,
  trim = purple_trim,
  notrim = purple_notrim
    ){
  weekly_trim        =trim[which(trim$Date == date),]
  weekly_notrim      =notrim[which(notrim$Date == date),]
  totalweight = c(sum(weekly_trim$`Weight(g)`),sum(weekly_notrim$`Weight(g)`))
  sd = c(sd(weekly_trim$`Weight(g)`),sd(weekly_notrim$`Weight(g)`))
  mu = c(mean(weekly_trim$`Weight(g)`),mean(weekly_notrim$`Weight(g)`))
  return(c(totalweight,sd,mu))
}

# apply for weekly caculation. Using parameter:harvest_day 
daylayout = matrix(harvest_day, nrow =1)
weekly_harvest = apply(daylayout, 2, weekly_totalweight)

# summarize PivotTable to an data frame 
Harvest_treatment_p = data.frame(harvest_day,round(t(weekly_harvest),digit = 3))
colnames(Harvest_treatment_p) = c("Date","Purple_trim","Purple_notrim","sd_trim","sd_notrim","mu_trim","mu_notrim")


#### PivotTable caculate function establishment
#for Red
weekly_totalweight = function(
  date = NULL,
  trim = red_trim,
  notrim = red_notrim
){
  weekly_trim        =trim[which(trim$Date == date),]
  weekly_notrim      =notrim[which(notrim$Date == date),]
  totalweight = c(sum(weekly_trim$`Weight(g)`),sum(weekly_notrim$`Weight(g)`))
  sd = c(sd(weekly_trim$`Weight(g)`),sd(weekly_notrim$`Weight(g)`))
  mu = c(mean(weekly_trim$`Weight(g)`),mean(weekly_notrim$`Weight(g)`))
  return(c(totalweight,sd,mu))
}

# apply for weekly caculation. Using parameter:harvest_day
daylayout = matrix(harvest_day, nrow =1)
weekly_harvest = apply(daylayout, 2, weekly_totalweight)

# summarize PivotTable to an data frame 
Harvest_treatment_r = data.frame(harvest_day,round(t(weekly_harvest),digit = 3))
colnames(Harvest_treatment_r) = c("Date","Red_trim","Red_notrim","sd_trim","sd_notrim","mu_trim","mu_notrim")
