### Day, Night temperature Line graph
install.packages("ggthemes")
# Insert climate data(minute)
setwd("/Users/supermonk00/desktop")
clmdato = read.csv("climatedata.csv")
clmdat = clmdato[-1,]                   #omit unit information

# Choose isolation <10e-4 as night data >10e-4 as day data
nclmdat = clmdat[which(as.numeric(as.character(clmdat$Insolation.MJ.))<=10e-4),]
dclmdat = clmdat[which(as.numeric(as.character(clmdat$Insolation.MJ.))>=10e-4),]

## Calculate the daily night temperature 
# mean calculate function
nmeanc = function(day){
  ndate = nclmdat[grep(day,nclmdat$TimesSeries),]                #grab the specific date
  nmean=  mean(as.numeric(as.character(ndate$Air.temperature)))  #calculate the mean
  ddate = dclmdat[grep(day,dclmdat$TimesSeries),]  
  dmean=  mean(as.numeric(as.character(ddate$Air.temperature)))
  return(c(nmean,dmean))
}

# layout the time period
dato = seq(as.Date("2019/8/21"), 
        as.Date("2019/11/11"),"day") #from start day to end day
day = format(dato, format = "%Y/%m/%d") #change form to fit excel data

#make a matrix for all array for apply
treatday = matrix(as.character(day),nrow  = 1)

#calculate the daily mean
mdaily = apply(treatday, 2, nmeanc)
avetemp = as.data.frame(t(mdaily)) #t(): from 2 row to 2 column
colnames(avetemp) = c("Day","Night")

#data transfer
dat = data.frame(
  Date = as.Date(rep(day,2)),
  Time   = factor(c(rep("Nocturnal",length(avetemp$Night)),
                     rep("Diurnal",length(avetemp$Day)))),
  Temperature = c(avetemp$Day, avetemp$Night))

##line graph 
library(ggplot2)
p = ggplot(data=dat, aes(x=Date, y=Temperature)) +
    geom_line(aes(color=Time))+
    geom_point(aes(color=Time))+
  scale_color_manual(values=c("#006400", "black"))   #change the line color(darkgreen & black)


require(ggthemes)
p + ylab("Temperature(Celsius)")+                    #y-axis text
    ggtitle("Daily Temperature")+                    #Title text
    theme_wsj()                                      #Fancy theme

ggsave(filename = "Daily Temperature",               #save file
       dpi=300, device = "pdf") 








