setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("Day-night temperature dividing.R")

#data transfer
dat = data.frame(
  Date = as.Date(rep(day,2)),
  Time   = factor(c(rep("Nocturnal",length(daily_temperature$Night)),
                    rep("Diurnal",length(daily_temperature$Day)))),
  Temperature = c(daily_temperature$Day, daily_temperature$Night))

##line graph 
library(ggplot2)
p = ggplot(data=dat, aes(x=Date, y=Temperature)) +
  geom_line(aes(color=Time))+
  geom_point(aes(color=Time))+
  ylab("Temperature(DegC)")+
  scale_color_manual(values=c("#006400", "black"))   #change the line color(darkgreen & black)


ggsave(filename = "Daily Temperature",               #save file
       dpi=300, device = "pdf") 








