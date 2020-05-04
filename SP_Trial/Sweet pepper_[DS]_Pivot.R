library(ggplot2)
source("(Function) Data_partition.R")
setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("Sweet pepper PivotTable report.R")
source("(Function)AvgTempinPeriod_caculate.R")
##### Pre-process the data to a plot form#####

#### (lead in the temperature information)
month_temp = Dailytemperature_calculate(
  filename = "Needsmust.csv",
  start_day = "2019/10/11",
  end_day = "2020/03/23",
  daysforavg = 30,daysfordivid = 7)

# insert temperature data, replicate for two variety
temperature = rep(c(month_temp[,1]),2)

### set the week data, replicate for two variety
Week = rep(c(1:length(Harvest$Date)),2)

### reform to the plot-acceptable dataframe
Variety = rep("Purple",length(Harvest$Date))
Weight = Harvest$Purple_weight
purple = data.frame(Weight,Variety)

Variety = rep("Red",length(Harvest$Date))
Weight = Harvest$Red_weight
red = data.frame(Weight,Variety)

rm(Weight,Variety)

# tidy up to an intergated dataframe #
dat = data.frame(Week,rbind(purple,red))



#####barplot & linegraph for weekly harvest(Sweet Pepper Harvest Overall)######

# set plot, x-axis
p = ggplot(data = dat, aes(x = Week))

# draw barchart
p = p + geom_bar(aes(y= Weight,fill = Variety), 
                 stat = "identity", 
                 position="dodge",
                 width = 0.5)+
  scale_fill_manual(values = c("purple", "red"))

ylim.prim <- c(0, 100000)   # in this example, precipitation
ylim.sec <- c(25, 30)    # in this example, temperature


# linear transformation
b <- diff(ylim.prim)/diff(ylim.sec)
a <- b*(ylim.prim[1] - ylim.sec[1])

# draw linegraph
p = p +
  geom_line(aes(y = a+temperature*b))+
  geom_point(aes(y = a+temperature*b))+
  theme(axis.title.y = element_text(vjust = 1,size = 15), 
        axis.title.x = element_text(size = 15))+
  scale_y_continuous(name = "Weight(g)", 
                     sec.axis = sec_axis(~(.-a)/b ,name ="Temperature(°C)"))+ #set second axis
  scale_x_continuous(name = "Week(2019/11/11-2020/03/23)", breaks = c(Week))+
  ggtitle("Sweet Pepper Harvest Overall")



#####Scatter of Temperature & Productivity Correlation####

## Purple 

lm_p = summary(lm(Harvest$Purple_weight~month_temp[,1]))

plot(month_temp[,1], Harvest$Purple_weight,pch=".",cex= 5, 
     ylab = "Weekly harvest(g)",
     xlab = "Monthly temperature average before harvest",
     main = "Linear regression of harvest~temperature(Purple)")
abline(lm_p, col = "purple")
text(29,70000,as.expression(substitute(italic(adj.R)^2 == r, list(r = round(lm_p$adj.r.squared, 
                                                                            3)))))


## Red

lm_r = summary(lm(Harvest$Red_weight~month_temp[,1]))
# 'cause R-squared (Red) is low, strech the loess line
loes = loess(Harvest$Red_weight~month_temp[,1])
smoothed <- predict(loes)

plot(month_temperature, Harvest$Red_weight)
abline(lm_r, col = "red")
lines(smoothed, x=month_temp[,1], col="grey")
text(29,20000,as.expression(substitute(italic(adj.R)^2 == r, list(r = round(lm_r$adj.r.squared, 
                                                                            3)))))

####Fruit Weight distribution####

# reform to the plot-acceptable dataframe

Variety = rep("Purple",length(purple_total$Date))
Weight = purple_total$`Weight(g)`
purple = data.frame(Weight,Variety)

Variety = rep("Red",length(red_total$Date))
Weight = red_total$`Weight(g)`
red = data.frame(Weight,Variety)

# tidy up to an intergated dataframe #

dat = data.frame(rbind(purple,red))

#### boxplot, show the diversity of weight, max, mid, min, etc####

box = ggplot(dat,aes(x = Variety, y=Weight,fill=Variety))+
  geom_boxplot()+
  scale_fill_manual(values = c("purple", "red"))+
  ggtitle("Boxplot: Fruit Weight Distribution")+
  ylab("Fruit weight(g)")

rm(box)

#### boxplot, show the trim/notrim weight(Purple) distribution, max, mid, min, etc####

dat = rbind(purple_notrim,purple_trim)
Treatment =c(rep("notrim",nrow(purple_notrim)),rep("trim",nrow(purple_trim)))
data = data_frame(weight = dat$`Weight(g)`,trt = Treatment)

# boxplot show the trim/notrim(purple) distribution
p = ggplot(data = data, aes(x = trt))
p = p + geom_boxplot(aes(y= weight))+
  labs(title = "Fruit Weight Distribution",
       subtitle = "Purple",
       x = "Treatment",y = "Weight(g)")

# t.test for assessment of difference of the average weight 
t.test(data$weight[which(data$trt =="notrim")], 
       data$weight[which(data$trt =="trim")], paired=FALSE)

##### Trim Treatment result ####

setwd("/Users/supermonk00/Desktop/academy/programing/R/St.Kitts/Code")
source("SweetpepperTrimtreatmentPivotable.R")

## for purple

Treatment = rep("Trim",4)
Weight = c(Harvest_treatment_p$Purple_trim[17:20])
TRIM = data.frame(Weight,Treatment)

Treatment = rep("Notrim",4)
Weight = c(Harvest_treatment_p$Purple_notrim[17:20])
Notrim = data.frame(Weight,Treatment)

Treatment = rep("Trim",4)
mu = c(Harvest_treatment_p$mu_trim[17:20])
sd = c(Harvest_treatment_p$sd_trim[17:20])
pv_trim = data.frame(mu,sd)

Treatment = rep("Notrim",4)
mu = c(Harvest_treatment_p$mu_notrim[17:20])
sd = c(Harvest_treatment_p$sd_notrim[17:20])
pv_notrim = data.frame(mu,sd)

rbind(pv_trim,pv_notrim)
Week = rep(c(1:4),2)

# tidy up to an intergated dataframe #

dat = data.frame(cbind(rbind(TRIM,Notrim),rbind(pv_trim,pv_notrim)))

#Barchart, show total weight of Treatment or not

p = ggplot(data = dat, aes(x = Week))

p = p + geom_bar(aes(y= Weight,fill = Treatment),
                 stat = "identity", 
                 position="dodge",
                 width = 0.5)+
  scale_fill_manual(values = c("black", "grey"))+
  scale_x_continuous(name = "Week(2020/02/24-2020/03/23)", breaks = c(Week))+
  ylab("Total weight(g)")+
  ggtitle("TrimTrial:Total Weight",subtitle = "Purple")

p2 = ggplot(data = dat, aes(x = Week,y = mu,fill = Treatment))+
  geom_col(position = 'dodge', width = 0.5)+
  scale_fill_manual(values = c("#DCDCDC", "#696969"))+
  geom_errorbar(aes(x=Week, ymin=mu-sd, ymax=mu+sd),  # 添加误差线
                width=0.1, color='black', position = position_dodge(0.5),  # 设置误差线颜色，宽度等
                size=0.3)+
  scale_x_continuous(name = "Week(2020/02/24-2020/03/23)", breaks = c(Week))+
  ylab("Average weight(g)")+
  ggtitle("TrimTrial:Average Weight",subtitle =  "Purple")







##for red
Treatment = rep("Trim",4)
Weight = c(Harvest_treatment_r$Red_trim[17:20])
TRIM = data.frame(Weight,Treatment)

Treatment = rep("Notrim",4)
Weight = c(Harvest_treatment_r$Red_notrim[17:20])
Notrim = data.frame(Weight,Treatment)


Treatment = rep("Trim",4)
mu = c(Harvest_treatment_r$mu_trim[17:20])
sd = c(Harvest_treatment_r$sd_trim[17:20])
pv_trim = data.frame(mu,sd)

Treatment = rep("Notrim",4)
mu = c(Harvest_treatment_r$mu_notrim[17:20])
sd = c(Harvest_treatment_r$sd_notrim[17:20])
pv_notrim = data.frame(mu,sd)

rbind(pv_trim,pv_notrim)
Week = rep(c(1:4),2)

dat = data.frame(cbind(rbind(TRIM,Notrim),rbind(pv_trim,pv_notrim)))

#plot 
p = ggplot(data = dat, aes(x = Week))

p = p + geom_bar(aes(y= Weight,fill = Treatment),
                 stat = "identity", 
                 position="dodge",
                 width = 0.5)+
  scale_fill_manual(values = c("black", "grey"))+
  scale_x_continuous(name = "Week(2020/02/24-2020/03/23)", breaks = c(Week))+
  ylab("Total weight(g)")+
  ggtitle("TrimTrial:Total Weight",subtitle = "Red")
p2 = ggplot(data = dat, aes(x = Week,y = mu,fill = Treatment))+
  geom_col(position = 'dodge', width = 0.5)+
  scale_fill_manual(values = c("#DCDCDC", "#696969"))+
  geom_errorbar(aes(x=Week, ymin=mu-sd, ymax=mu+sd), 
                width=0.1, color='black', position = position_dodge(0.5),  
                size=0.3)+
  scale_x_continuous(name = "Week(2020/02/24-2020/03/23)", breaks = c(Week))+
  ylab("Average weight(g)")+
  ggtitle("TrimTrial:Average Weight",subtitle =  "Red")