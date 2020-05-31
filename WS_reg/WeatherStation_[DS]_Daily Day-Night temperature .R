source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.average_temperature.R")

Dataclean(filename = "WS.Needsmust.Min.record.dat")
daily_temperature = Dailytemperature_calculate(start_day = "2019/08/20",end_day = "2020/03/17",
                           days.for.average = 1,days.for.divid = 1,
                           filename = "WS.Needsmust.Min.record.dat")

#data transfer
dat = data.frame(
  Date = seq(as.Date("2019/08/20"), 
                   as.Date("2020/03/17"),"day") %>%
    format(., format = "%Y-%m-%d"),
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








