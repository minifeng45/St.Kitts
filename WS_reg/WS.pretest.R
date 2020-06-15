#Descripitive statistics: Different situation for the differences of weather station and sensors
source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.data_partition.R")
library(ggplot2)
### Meterological data insert
# Weather stations

WSD.la.guerite = data_partition(Datasource = "WS.La Guerite.dat",
                             start_day = "2019/8/29",
                             end_day = "2020/06/01",
                             Partition_Way = 1) 

WSD.needmust = data_partition(Datasource = "WS.Needmust.dat",
                              start_day = "2019/8/29",
                              end_day = "2020/06/01",
                              Partition_Way = 1) 

# Mounted sensors

HSD.la.guerite.insect.screen.nets = data_partition(Datasource = "HS.La Guerite 1.csv",
                             start_day = "2019/8/29",
                             end_day = "2020/06/01",
                             Partition_Way = 1) 


HSD.la.guerite.shading.wire.nets = data_partition(Datasource = "HS.La Guerite 2.csv",
                                start_day = "2019/8/29",
                                end_day = "2020/06/01",
                                Partition_Way = 1) 

HSD.la.guerite.wire.nets = data_partition(Datasource = "HS.La Guerite 3.csv",
                                          start_day = "2019/8/29",
                                          end_day = "2020/06/01",
                                          Partition_Way = 1)

HSD.needmust = data_partition(Datasource = "HS.Needsmust.csv",
                                 start_day = "2019/8/29",
                                 end_day = "2020/06/01",
                                 Partition_Way = 1) 


## Get whole day data 
# Weather station

WSD.la.guerite.all = rbind(WSD.la.guerite$Day,WSD.la.guerite$Night)

WSD.needmust.all = rbind(WSD.needmust$Day,WSD.needmust$Night)

# Mounted sensor

HSD.la.guerite.shading.wire.nets.all = rbind(HSD.la.guerite.shading.wire.nets$Day,
                                             HSD.la.guerite.shading.wire.nets$Night) %>%
  mutate(AirTC_Avg = (AirTC_Avg-32)*5/9 )

HSD.la.guerite.insect.screen.nets.all = rbind(HSD.la.guerite.insect.screen.nets$Day,
                                              HSD.la.guerite.insect.screen.nets$Night) %>%
  mutate(AirTC_Avg = (AirTC_Avg-32)*5/9 )

HSD.la.guerite.wire.nets.all = rbind(HSD.la.guerite.wire.nets$Day,
                                     HSD.la.guerite.wire.nets$Night) %>%
  mutate(AirTC_Avg = (AirTC_Avg-32)*5/9 )

HSD.needmust.all = rbind(HSD.needmust$Day,HSD.needmust$Night) %>%
  mutate(AirTC_Avg = (AirTC_Avg-32)*5/9 )



## Pretest 1: comparing WSDs' and HSDs' daily temperature
# Joint: WSDs to HSDs for shading.wire.nets

Dataset_allday.la.guerite.shading.wire.nets = WSD.la.guerite.all %>%
  inner_join(HSD.la.guerite.shading.wire.nets.all, by ="TIMESTAMP", suffix = c("_WS","_HS")) %>%
  mutate(month = month(TIMESTAMP), date = days(TIMESTAMP))

# Joint: WSDs to HSDs for insect.screen.nets

Dataset_allday.la.guerite.insect.screen.nets = WSD.la.guerite.all %>%
  inner_join(HSD.la.guerite.insect.screen.nets.all,by ="TIMESTAMP", suffix = c("_WS","_HS")) %>%
  mutate(month = month(TIMESTAMP), date = days(TIMESTAMP))

# Joint: WSDs to HSDs for wire.nets

Dataset_allday.la.guerite.wire.nets = WSD.la.guerite.all %>%
  inner_join(HSD.la.guerite.wire.nets.all,by ="TIMESTAMP", suffix = c("_WS","_HS")) %>%
  mutate(month = month(TIMESTAMP), date = days(TIMESTAMP))


  

# Date selection: 2020/2/2
month.date = c(2,2)

# Function: plot daily temperature fluctuation by Location
Comparison.visualize = function(Dataset,Location = "Location: cultivated house name"){
  # Dateframe form, rbind weather station and sensor data 
  
  WS.HS_air.temp.daily = rbind(
    WS_air.temp = Dataset %>%
      filter(date == month.date[2], month == month.date[1])%>%
      select(TIMESTAMP,AirTC_Avg  = AirTC_Avg_WS) %>%
      mutate(Category = "AWS"),
    HS_air.temp = Dataset %>%
      filter(date == month.date[2], month == month.date[1])%>%
      select(TIMESTAMP,AirTC_Avg = AirTC_Avg_HS) %>%
      mutate(Category = "MS")
    )
  # plot
    
  ggplot(WS.HS_air.temp.daily,aes(x=ymd_hms(TIMESTAMP),y=AirTC_Avg,color= Category))+
      geom_line()+
      labs(title = "Fluctuation of daily temperature",
           subtitle = paste("Date:2020",month.date[1],month.date[2],sep = "/"),
           caption = Location)+
      xlab("Time")+
      ylab("Air temperature")+
      scale_color_manual(values=c('dark blue','dark red'))
}

Comparison.visualize(Dataset_allday.la.guerite.shading.wire.nets,
                     Location = "Location: La Guerite-Shading-wire-nets house")

Comparison.visualize(Dataset_allday.la.guerite.insect.screen.nets,
                     Location = "Location: La Guerite-Insect-screen-nets house")

Comparison.visualize(Dataset_allday.la.guerite.wire.nets,
                     Location = "Location: La Guerite-Wire-nets house")

## Pretest 2: comparing HSDs' daily temperature in different microenvironment
# Joint: all HSD data
Dataset_allday.different.location = HSD.needmust.all %>%
  inner_join(HSD.la.guerite.insect.screen.nets.all,by ="TIMESTAMP", suffix = c("_needmust","_1")) %>%
  inner_join(HSD.la.guerite.shading.wire.nets.all,by ="TIMESTAMP") %>%
  inner_join(HSD.la.guerite.wire.nets.all.all,by ="TIMESTAMP", suffix = c("_2","_3")) %>%
  mutate(month = month(TIMESTAMP), date = days(TIMESTAMP))

# Date selection: 2020/3/10

month.date = c(3,10)

# Dateframe form, rbind mounted sensors from different locations

different_air.temp.daily = rbind(
  Needmust_air.temp = Dataset_allday.different.location %>%
    filter(date == month.date[2], month == month.date[1])%>%
    select(TIMESTAMP,AirTC_Avg  = AirTC_Avg_needmust) %>%
    mutate(Category = "Needmust"),
  
  third_air.temp = Dataset_allday.different.location %>%
    filter(date == month.date[2], month == month.date[1])%>%
    select(TIMESTAMP,AirTC_Avg = AirTC_Avg_3) %>%
    mutate(Category = "Wire-nets house"),
  
  second_air.temp = Dataset_allday.different.location %>%
    filter(date == month.date[2], month == month.date[1])%>%
    select(TIMESTAMP,AirTC_Avg = AirTC_Avg_2) %>%
    mutate(Category = "Shading-wire-nets house"),
  
  
  first_air.temp = Dataset_allday.different.location %>%
    filter(date == month.date[2], month == month.date[1])%>%
    select(TIMESTAMP,AirTC_Avg = AirTC_Avg_1) %>%
    mutate(Category = "Insect-screen-nets house")
)

ggplot(different_air.temp.daily,aes(x=ymd_hms(TIMESTAMP),y=AirTC_Avg,color= Category))+
  geom_line()+
  labs(title = "Fluctuation of daily temperature",
       subtitle = paste("Date:2020",month.date[1],month.date[2],sep = "/"))+
  xlab("Time")+
  ylab("Air temperature")+
  scale_color_manual(values=c('dark blue','dark red',"dark green","orange"))




