library(devtools)
source("/Users/supermonk00/Desktop/programing/R/St.Kitts/SP_Trial/SP_Data_insert.R")

#Total harvest data
purple_total = rbind(purple_before_trim,purple_notrim,purple_trim)[,-1]
red_total = rbind(red_before_trim,red_notrim,red_trim)[,-1]



####FUNCTION:PivotTable caculation[total]####
weekly_totalweight = function(date){
  weekly_P         =purple_total[which(purple_total$Date == date),]
  weekly_R         =red_total[which(red_total$Date == date),]
  totalweight = c(sum(weekly_P$`Weight(g)`),sum(weekly_R$`Weight(g)`))
  sd = c(sd(weekly_P$`Weight(g)`),sd(weekly_R$`Weight(g)`))
  mu = c(mean(weekly_P$`Weight(g)`),mean(weekly_R$`Weight(g)`))
  return(c(totalweight,sd,mu))
}


# apply for weekly caculation. Using parameter:harvest_day
daylayout = matrix(harvest_day, nrow =1)
weekly_harvest = apply(daylayout, 2, weekly_totalweight)

# summarize PivotTable to an data frame 
Harvest = data.frame(harvest_day,round(t(weekly_harvest),digit = 3))
colnames(Harvest) = c("Date","Purple_weight","Red_weight","Purple_sd","Red_sd","Purple_mu","Red_mu")

####FUNCTION:PivotTable caculation[TrimTrial]####
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


