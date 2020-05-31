#Descripitive statistics: Different situation for the differences of weather station and sensors
source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.data_partition.R")

WSD = data_partition(Datasource = "La Guerite_five_min.dat",
                             start_day = "2019/8/29",
                             end_day = "2020/04/12",
                             Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both

HSD.greenhouse = data_partition(Datasource = "La Guerite 2020-04-12 15_06_30 -0400.csv",
                             start_day = "2019/8/29",
                             end_day = "2020/04/12",
                             Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both


HSD.greenhouse2 = data_partition(Datasource = "La Guerite 2 2020-04-12 15_07_43 -0400.csv",
                                start_day = "2019/8/29",
                                end_day = "2020/04/12",
                                Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both

HSD.greenhouse3 = data_partition(Datasource = "La Guerite 3 2020-04-12 15_08_52 -0400.csv",
                                 start_day = "2019/8/29",
                                 end_day = "2020/04/12",
                                 Partition_Way = 1) 

HSD.needmust = data_partition(Datasource = "Needsmust.csv",
                                 start_day = "2019/8/29",
                                 end_day = "2020/04/12",
                                 Partition_Way = 1) 




WSD.all = rbind(WSD$Day,WSD$Night)

HSD.greenhouse2.all = rbind(HSD.greenhouse2$Day,HSD.greenhouse2$Night)

HSD.greenhouse.all = rbind(HSD.greenhouse$Day,HSD.greenhouse$Night)

HSD.greenhouse3.all = rbind(HSD.greenhouse3$Day,HSD.greenhouse3$Night)

HSD.needmust.all = rbind(HSD.needmust$Day,HSD.needmust$Night) %>%
  mutate(AirTC_Avg = (AirTC_Avg-32)*5/9 )

Dataset_allday.2 = WSD.all %>%
  inner_join(HSD.greenhouse2.all,by ="TIMESTAMP", suffix = c("_WS","_HS")) %>%
  mutate(month = month(TIMESTAMP), date = days(TIMESTAMP))

Dataset_allday = WSD.all %>%
  inner_join(HSD.greenhouse.all,by ="TIMESTAMP", suffix = c("_WS","_HS")) %>%
  mutate(month = month(TIMESTAMP), date = days(TIMESTAMP))


Dataset_allday.3 = WSD.all %>%
  inner_join(HSD.greenhouse3.all,by ="TIMESTAMP", suffix = c("_WS","_HS")) %>%
  mutate(month = month(TIMESTAMP), date = days(TIMESTAMP))

Dataset_allday.different.location = HSD.needmust.all %>%
  inner_join(HSD.greenhouse.all,by ="TIMESTAMP", suffix = c("_needmust","_1")) %>%
  inner_join(HSD.greenhouse2.all,by ="TIMESTAMP")%>%
  inner_join(HSD.greenhouse3.all,by ="TIMESTAMP", suffix = c("_2","_3")) %>%
  mutate(month = month(TIMESTAMP), date = days(TIMESTAMP))
  



#date selection: 2020/4/1
month.date = c(4,1)

WS_air.temp = Dataset_allday %>%
  filter(date == month.date[2], month == month.date[1])%>%
  select(TIMESTAMP,AirTC_Avg  = AirTC_Avg_WS) %>%
  mutate(Category = "AWS")

HS_air.temp = Dataset_allday %>%
  filter(date == month.date[2], month == month.date[1])%>%
  select(TIMESTAMP,AirTC_Avg = AirTC_Avg_HS) %>%
  mutate(Category = "MS")

WS.HS_air.temp.daily=  rbind(WS_air.temp,HS_air.temp)

library(ggplot2)

ggplot(WS.HS_air.temp.daily,aes(x=ymd_hms(TIMESTAMP),y=AirTC_Avg,color= Category))+
  geom_line()+
  labs(title = "Fluctuation of daily temperature",
       subtitle = paste("Date:2020",month.date[1],month.date[2],sep = "/"),
       caption = "Location: La Guerite-Wire-nests house")+
  xlab("Time")+
  ylab("Air temperature")+
  scale_color_manual(values=c('dark blue','dark red'))



#date selection: 2020/3/10
month.date = c(3,10)

Needmust_air.temp = Dataset_allday.different.location %>%
  filter(date == month.date[2], month == month.date[1])%>%
  select(TIMESTAMP,AirTC_Avg  = AirTC_Avg_needmust) %>%
  mutate(Category = "Needmust")

third_air.temp = Dataset_allday.different.location %>%
  filter(date == month.date[2], month == month.date[1])%>%
  select(TIMESTAMP,AirTC_Avg = AirTC_Avg_3) %>%
  mutate(Category = "Wire-nests house")


second_air.temp = Dataset_allday.different.location %>%
  filter(date == month.date[2], month == month.date[1])%>%
  select(TIMESTAMP,AirTC_Avg = AirTC_Avg_2) %>%
  mutate(Category = "Shading-wire-nets house")


first_air.temp = Dataset_allday.different.location %>%
  filter(date == month.date[2], month == month.date[1])%>%
  select(TIMESTAMP,AirTC_Avg = AirTC_Avg_1) %>%
  mutate(Category = "Insect-screen-nets house")

different_air.temp.daily=  rbind(Needmust_air.temp,third_air.temp,
                                 second_air.temp,first_air.temp)

ggplot(different_air.temp.daily,aes(x=ymd_hms(TIMESTAMP),y=AirTC_Avg,color= Category))+
  geom_line()+
  labs(title = "Fluctuation of daily temperature",
       subtitle = paste("Date:2020",month.date[1],month.date[2],sep = "/"))+
  xlab("Time")+
  ylab("Air temperature")+
  scale_color_manual(values=c('dark blue','dark red',"dark green","orange"))



