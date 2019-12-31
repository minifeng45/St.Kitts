# install packages
install.packages("ggthemes")
require(ggthemes)

# move climate data file to desktop

setwd("/Users/supermonk00/desktop")


### weight distribution (red vs. purple)

weightdat = read.csv("Sweetpepperwieght.csv",header = T)
head(weightdat) #check data
colnames(weightdat) = c("Red","Purple")

#data transfer 

dat = data.frame(
  Variety = factor(c(rep("Red",length(na.omit(weightdat$Red))),
                   rep("Purple",length(weightdat$Purple)))),
  Weight = c(na.omit(weightdat$Red), weightdat$Purple))

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
p<-ggplot(dat, aes(x=Weight, fill = Variety, color=Variety)) +     # fill color follow category"Variety"
  geom_histogram(position="identity",binwidth = 3, alpha = 0.5)+   # separate data, bar width, transparency
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





  