### solar radiation bar chart
install.packages("ggthemes")


# Insert climate data(daily)
setwd("/Users/supermonk00/desktop")
clmdato = read.csv("needsmustdaily.csv")
clmdat = clmdato[,c(-1,-3,-4)]            #eliminate unused column

#sort solar radiation data
soldat =  data.frame(
  Date = clmdat$TIMESTAMP,
  SolarW = as.numeric(as.character(clmdat$SlrW_Avg)),   #average solar radiation
  SolarWMx = as.numeric(as.character(clmdat$SlrW_Max))  #maximun solar radiation
  )

# layout the time period
sdato = seq(as.Date("2019/8/21"), 
           as.Date("2019/11/11"),"week")  #from start week 1st day to last week 1st day
sday = format(sdato, format = "%Y-%m-%d") #change form to fit excel data

edato = seq(as.Date("2019/8/27"), 
            as.Date("2019/11/13"),"week") #from start week last day to last week last day

eday = format(edato, format = "%Y-%m-%d") #change form to fit excel data

## Calculate the weekly solar radiation 
# mean calculate function
nmeanc = function(sday,eday){
  dater = soldat[grep(sday,soldat$Date):grep(eday,soldat$Date),] #date sort from week 1st-7th
  wmean=  mean(na.omit(as.numeric(as.character(dater$SolarW))))  #calculate the mean without na
  return(c(wmean))
}

# sd calculate function
nsdc = function(sday,eday){
  dater = soldat[grep(sday,soldat$Date):grep(eday,soldat$Date),] #date sort from week 1st-7th
  wsd=  sd(na.omit(as.numeric(as.character(dater$SolarW))))      #calculate the standard error without na
  return(c(wsd))
}

treatday = matrix(c(sday,eday),ncol=2) #nx2 matrix: (week 1st, week last) 

##calculate by row
SWave =c()
SWsd =c()
for (i in 1:length(sday)) {
  SWave[[i]] = nmeanc(treatday[i,1],treatday[i,2])
  SWsd[[i]] = nsdc(treatday[i,1],treatday[i,2])
}

# the result form data.frame
dat = data.frame(
    Week = ordered(paste(1:length(sday)),levels = c(1:length(sday))),
    SWave,
    SWsd
    )

library(ggplot2)
p = ggplot(data=dat, aes(x=Week, y=SWave, group =1))+
    geom_line(color  = "#B22222")+
    geom_point(color  = "#B22222")+
    xlab("Week(period:2019/8/21-2019/11/11")+
    ylab("Irradiance(unit:W/m^2)")+
   ggtitle("Weekly Solar Radiation")

require(ggthemes)
p + 
  xlab("Week(period:2019/8/21-2019/11/11)")+
  ylab("Irradiance(unit:W/m^2)")+                    #y-axis text
  ggtitle("Weekly Solar Radiation")+                 #Title text
  theme_wsj()                       #Fancy theme


ggsave(filename = "Weekly Solar Radiation",               #save file
       dpi=300, device = "pdf") 
  


    


