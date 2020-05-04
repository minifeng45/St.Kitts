# install packages
install.packages("ggthemes")
require(ggthemes)

# move climate data file to desktop

setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("Sweetpepper_data_insert.R")

# total harvest data

purple_total = rbind(purple_before_trim,purple_notrim,purple_trim)[,-1]
red_total = rbind(red_before_trim,red_notrim,red_trim)[,-1]


### weight distribution (red vs. purple)
# weekly weight dist.
weekly_totalweight = function(date){
  
  weekly_P       =purple_total[which(purple_total$Date == date),]
  weekly_R         =red_total[which(red_total$Date == date),]
 
  totalweight = c(sum(weekly_P$`Weight(g)`),sum(weekly_R$`Weight(g)`))
  return(totalweight)
}

daylayout = matrix(harvest_day, nrow =1)
weekly_harvest = apply(daylayout, 2, weekly_totalweight)

Harvest = as.data.frame(cbind(harvest_day,t(weekly_harvest)))
colnames(Harvest) = c("Date","Purple","Red")








pest_on_fruit = purple_before_trim[which(purple_before_trim$Pest==1),]
mean(pest_on_fruit$`Weight(g)`)
mean(purple_before_trim$`Weight(g)`)

pest_on_fruit = purple_before_trim[which(purple_before_trim$Rotten==1),]
mean(pest_on_fruit$`Weight(g)`)
mean(purple_before_trim$`Weight(g)`)

Rotten_on_fruit = red_before_trim[which(red_before_trim$Rotten==1),]
mean(Rotten_on_fruit$`Weight(g)`)
mean(red_before_trim$`Weight(g)`)



###histogram 
library(ggplot2)

#two variety mean 
library(plyr)
mu <- ddply(dat, "Variety", summarise, grp.mean=mean(Weight))



## two variety compare
#split by color
#position : identity
#break = 3g
# dashed line :  mu value
p<-ggplot(Harvest, aes(x=Date, fill = Variety, color=Variety)) +     # fill color follow category"Variety"
  geom_histogram(position="identity",binwidth = 3, alpha = 0.5)     # separate data, bar width, transparency

  geom_vline(data=mu, aes(xintercept=grp.mean, color=Variety),     # the mean line
             linetype="dashed")+
  theme(legend.position="top")+                                    # the label's position
  scale_color_manual(values=c("purple", "red"))+                   # change the bar color
  scale_fill_manual(values=c("purple","red"))

p + xlab("Weight(g)")+               #x-axis text
  ylab("Pieces")+                    #y-axis text
 ggtitle("Weight Distribution")+     #Title text
 theme_wsj()                         #Fancy theme

ggsave("Weight Distribution", device = "pdf", dpi=300) #save file





  