#source raw data 

setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("Sweetpepper_data_insert.R")

#Total harvest data
purple_total = rbind(purple_before_trim,purple_notrim,purple_trim)[,-1]
red_total = rbind(red_before_trim,red_notrim,red_trim)[,-1]



#PivotTable caculate function establishment
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

# elminate useless rv.
rm(daylayout,purple_before_trim,purple_notrim,purple_trim,purple_total,
   red_before_trim,red_notrim,red_total,red_trim,weekly_harvest)

