library(ggplot2)
setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("Sweet pepper PivotTable report.R")
source("HOBO_Needmust_caculate.R")
# Pre-process the data to a plot form
Week = rep(c(1:length(Harvest$Date)),2)

Variety = rep("Purple",length(Harvest$Date))
Weight = Harvest$Purple_weight
purple = data.frame(Weight,Variety)

Variety = rep("Red",length(Harvest$Date))
Weight = Harvest$Red_weight
red = data.frame(Weight,Variety)

rm(Weight,Variety)

##### tidy up to an intergated data form #####
dat = data.frame(Week,rbind(purple,red))


# insert temperature data
temperature = rep(c(mouth_temp[,1]),2)



p = ggplot(data = dat, aes(x = Week))

p = p + geom_bar(aes(y= Weight,fill = Variety),
                 stat = "identity", 
                 position="dodge",
                 width = 0.5)+
  scale_fill_manual(values = c("purple", "red"))

ylim.prim <- c(0, 100000)   # in this example, precipitation
ylim.sec <- c(25, 30)    # in this example, temperature

b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

p = p +
  geom_line(aes(y = a+temperature*b))+
  geom_point(aes(y = a+temperature*b))+
  theme(axis.title.y = element_text(vjust = 1,size = 15),
        axis.title.x = element_text(size = 15))+
  scale_y_continuous(name = "Weight(g)", 
                     sec.axis = sec_axis(~(.-a)/b ,name ="Temperature(°C)"))+
  scale_x_continuous(name = "Week(2019/11/11-Present)", breaks = c(Week))+
  ggtitle("Sweet Pepper Harvest Overall")



rm(p)

#Scatter of Temperature & Productivity Correlation, for Purple

dat_P = data.frame(mouth_temp[,1],Harvest$Purple_weight)
colnames(dat_P) = c("Avr_temperature","Purple_productivity")
lm_p = summary(lm(Harvest$Purple_weight~mouth_temp[,1]))

p = ggplot(dat_P, aes(x = Avr_temperature, y = Purple_productivity))

p = p+ 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, col = "purple")+
  labs(title = "Temperature &  Productivity Correlation(Purple)")+
  annotate("text", label = paste("R-square =", round(lm_p$r.squared,3))
           , x = 27.2, y = 20000, size = 6)
rm(p)
#Scatter of Temperature & Productivity Correlation, for Purple

dat_R = data.frame(mouth_temp[,1],Harvest$Red_weight)
colnames(dat_R) = c("Avr_temperature","Red_productivity")
lm_r = summary(lm(Harvest$Red_weight~mouth_temp[,1]))

p = ggplot(dat_R, aes(x = Avr_temperature, y = Red_productivity))

p = p+ 
  geom_point()+
  geom_smooth(method = "lm", se = FALSE, col = "red")+
  labs(title = "Temperature &  Productivity Correlation(Red)")+
  annotate("text", label = paste("R-square =", round(lm_r$r.squared,3))
           , x = 27, y = 3000, size = 6)







plot(mouth_temp, Harvest$Purple_weight)
abline(lm_p, col = "purple")
text(27.5,70000,as.expression(substitute(italic(R)^2 == r, list(r = round(lm_p$r.squared, 
                                                                        3)))))
plot(mouth_temperature, Harvest$Red_weight)
abline(lm_r, col = "red")
text(27.5,20000,as.expression(substitute(italic(R)^2 == r, list(r = round(lm_r$r.squared, 
                                                                          3)))))
lm_p = summary(lm(purple_productivity~mouth_temperature))
lm_r = summary(lm(red_productivity~mouth_temperature))
