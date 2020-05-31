library(ggplot2)
library(dplyr)

source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.average_temperature.R")
source("/Users/supermonk00/Desktop/programing/R/St.Kitts/SP_Trial/SP_PivotTable_report.R")
##### Pre-process the data to a plot form#####

#### (lead in the temperature information)
month_temp = Dailytemperature_calculate(
  filename = "Needsmust.csv",
  start_day = "2019/10/11",
  end_day = "2020/03/23",
  including.period = 30,divided.period  = 7)

# insert temperature data, replicate for two variety
temperature = rep(c(month_temp$temperature.average),2)

### set the week data, replicate for two variety
Week = rep(c(1:length(Harvest$Date)),2) 

### reform to the plot-acceptable dataframe

purple = data.frame(Weight = Harvest$weight_purple,Variety = "Purple")

red = data.frame(Weight = Harvest$weight_red, Variety = "Red")



# tidy up to an intergated dataframe #
Dataset_weekly.harvest = data.frame(Week,rbind(purple,red))



#####barplot & linegraph for weekly harvest(Sweet Pepper Harvest Overall)######

# set plot, x-axis
p.overall.harvest = ggplot(data = Dataset_weekly.harvest, aes(x = Week))

# draw barchart
p.overall.harvest = p.overall.harvest + geom_bar(aes(y= Weight,fill = Variety), 
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
p.overall.harvest = p.overall.harvest +
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
lm.p.data = data.frame(temperature = month_temp$temperature.average, weight =Harvest$weight_purple )
lm_p = lm(weight~temperature,data =lm.p.data) %>%
  summary()



lm.information.label.p = as.expression(substitute(italic(adj.R)^2 == r, list(r = round(lm_p$adj.r.squared,3))))

p.lm.purple = ggplot(lm.p.data,aes(x =temperature, y  =  weight))+
         geom_point()+
         labs(x = "Monthly temperature average before harvest",
              y = "Weekly harvest(g)")+
  geom_smooth(method = "lm", se = FALSE,col = "purple")+
  annotate("text", x = 28.5, y = 70000, 
           label = lm.information.label.p)


## Red
lm.r.data = data.frame(temperature = month_temp$temperature.average, weight =Harvest$weight_red)
lm_r = lm(weight~temperature,data = lm.r.data)%>%
  summary()

lm.information.label.r = as.expression(substitute(italic(adj.R)^2 == r, list(r = round(lm_r$adj.r.squared,3))))


p.lm.red = ggplot(lm.r.data,aes(x =temperature, y  =  weight))+
  geom_point()+
  labs(x = "Monthly temperature average before harvest",
       y = "Weekly harvest(g)")+
  geom_smooth(method = "lm", se = FALSE,col = "red")+
  geom_smooth(method = "loess", se = FALSE,col = "grey")+ # 'cause R-squared (Red) is low, strech the loess line
  annotate("text", x = 28.5, y = 25000, 
           label = lm.information.label.r)+
  annotate("segment", x = 26.3, xend = 26.5, y = 5000, yend = 9500, colour = "grey", size=1, alpha=0.9, arrow=arrow())+
  annotate("text", x = 26.6, y = 4300,label="Extra:Loess for fitting data",colour = "grey",size=4)

####Fruit Weight distribution####

# reform to the plot-acceptable dataframe
Dataset_individual.weight = rbind(
  data.frame(Weight = purple_total$`Weight(g)`,Variety ="Purple"),
  data.frame(Weight = red_total$`Weight(g)`,Variety = "Red")
)

#### boxplot, show the diversity of weight, max, mid, min, etc####

p.box_weight.distribution = ggplot(Dataset_individual.weight,aes(x = Variety, y=Weight,fill=Variety))+
  geom_boxplot(alpha = 0.8)+
  scale_fill_manual(values = c("purple", "red"))+
  ggtitle("Boxplot: Fruit Weight Distribution")+
  ylab("Fruit weight(g)")


#### boxplot, show the trim/notrim weight(Purple) distribution, max, mid, min, etc####

Dataset_trimtrial.individual= rbind(
  data.frame(Weight = purple_notrim$`Weight(g)`,
            Treatment = "notrim"),
  data.frame(Weight = purple_trim$`Weight(g)`,
             Treatment = "trim"))

# boxplot show the trim/notrim(purple) distribution
p.box_trimtrial = ggplot(data = Dataset_trimtrial.individual, aes(x = Treatment))
p.box_trimtrial = p.box_trimtrial + geom_boxplot(aes(y= Weight))+
  labs(title = "Fruit Weight Distribution",
       subtitle = "Purple",
       y = "Weight(g)")

# t.test for assessment of difference of the average weight 
t.test(Dataset_trimtrial.individual$Weight[which(Dataset_trimtrial.individual$Treatment =="notrim")], 
       Dataset_trimtrial.individual$Weight[which(Dataset_trimtrial.individual$Treatment =="trim")], paired=FALSE)

##### Trim Treatment result ####

## for purple

Dataset_trimtrial.weekly.harvest.p = Harvest_treatment_p %>%
  mutate(Week = rep(c(1:4),2))

#Barchart, show total weight of Treatment or not

p.trimtrial.total.weight.p = ggplot(data = Dataset_trimtrial.weekly.harvest.p, aes(x = Week))

p.trimtrial.total.weight.p = p.trimtrial.total.weight.p + geom_bar(aes(y= Weight,fill = Treatment),
                 stat = "identity", 
                 position="dodge",
                 width = 0.5)+
  scale_fill_manual(values = c("black", "grey"))+
  scale_x_continuous(name = "Week(2020/02/24-2020/03/23)", breaks = c(Week))+
  ylab("Total weight(g)")+
  ggtitle("TrimTrial:Total Weight",subtitle = "Purple")

p.trimtrial.average.weight.p = ggplot(data = Dataset_trimtrial.weekly.harvest.p, aes(x = Week,y = mu,fill = Treatment))+
  geom_col(position = 'dodge', width = 0.5)+
  scale_fill_manual(values = c("#DCDCDC", "#696969"))+
  geom_errorbar(aes(x=Week, ymin=mu-sd, ymax=mu+sd),  # add errorbar
                width=0.1, color='black', position = position_dodge(0.5),  # set colors, width of errorbar
                size=0.3)+
  scale_x_continuous(name = "Week(2020/02/24-2020/03/23)", breaks = c(Week))+
  ylab("Average weight(g)")+
  ggtitle("TrimTrial:Average Weight",subtitle =  "Purple")







##for red

Dataset_trimtrial.weekly.harvest.r = Harvest_treatment_r %>%
  mutate(Week = rep(c(1:4),2))

#plot 
p.trimtrial.total.weight.r = ggplot(data = Dataset_trimtrial.weekly.harvest.r, aes(x = Week))

p.trimtrial.total.weight.r = p.trimtrial.total.weight.r + geom_bar(aes(y= Weight,fill = Treatment),
                 stat = "identity", 
                 position="dodge",
                 width = 0.5)+
  scale_fill_manual(values = c("black", "grey"))+
  scale_x_continuous(name = "Week(2020/02/24-2020/03/23)", breaks = c(Week))+
  ylab("Total weight(g)")+
  ggtitle("TrimTrial:Total Weight",subtitle = "Red")
p.trimtrial.average.weight.r = ggplot(data = Dataset_trimtrial.weekly.harvest.r, aes(x = Week,y = mu,fill = Treatment))+
  geom_col(position = 'dodge', width = 0.5)+
  scale_fill_manual(values = c("#DCDCDC", "#696969"))+
  geom_errorbar(aes(x=Week, ymin=mu-sd, ymax=mu+sd), 
                width=0.1, color='black', position = position_dodge(0.5),  
                size=0.3)+
  scale_x_continuous(name = "Week(2020/02/24-2020/03/23)", breaks = c(Week))+
  ylab("Average weight(g)")+
  ggtitle("TrimTrial:Average Weight",subtitle =  "Red")


