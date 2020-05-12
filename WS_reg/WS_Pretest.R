#Descripitive statistics: Different situation for the differences of weather station and sensors


WSD = data_partition(Datasource = "WS.Needsmust.Min.record.dat",
                     start_day = "2019/8/29",
                     end_day = "2020/04/12",
                     Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both

WSD.diffloc = data_partition(Datasource = "La Guerite_five_min.dat",
                             start_day = "2019/8/29",
                             end_day = "2020/04/12",
                             Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both

HSD.greenhouse = data_partition(Datasource = "La Guerite 3 2020-04-12 15_08_52 -0400.csv",
                             start_day = "2019/8/29",
                             end_day = "2020/04/12",
                             Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both


HSD.greenhouse2 = data_partition(Datasource = "La Guerite 2 2020-04-12 15_07_43 -0400.csv",
                                start_day = "2019/8/29",
                                end_day = "2020/04/12",
                                Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both

HSD.greenhouse2$Day$AirTC_Avg = HSD.greenhouse2$Day$AirTC_Avg*9/5 +32

Dataset_day = merge(WSD.diffloc$Day,HSD.greenhouse2$Day, by = "TIMESTAMP")

for (i in 2:ncol(Dataset_day)) {
  if (length(which(is.na(Dataset_day[,i]))) != 0) {
    Dataset_day = Dataset_day[-which(is.na(Dataset_day[,i])),] 
  }
}

pre = Dataset_day$AirTC_Avg.y - Dataset_day$AirTC_Avg.x

pre30up = Dataset_day$AirTC_Avg.y[which(Dataset_day$AirTC_Avg.x>30)] - 
  Dataset_day$AirTC_Avg.x[which(Dataset_day$AirTC_Avg.x>30)]


pre.noon = Dataset_day$AirTC_Avg.y[grep("12:",Dataset_day$TIMESTAMP)] -
                                     Dataset_day$AirTC_Avg.x[grep("12:",Dataset_day$TIMESTAMP)]

pre.une = Dataset_day$AirTC_Avg.y[grep("13:",Dataset_day$TIMESTAMP)] -
  Dataset_day$AirTC_Avg.x[grep("13:",Dataset_day$TIMESTAMP)]

pre.deux = Dataset_day$AirTC_Avg.y[grep("14:",Dataset_day$TIMESTAMP)] -
  Dataset_day$AirTC_Avg.x[grep("14:",Dataset_day$TIMESTAMP)]

pre.sept = Dataset_day$AirTC_Avg.y[grep("07:",Dataset_day$TIMESTAMP)] -
  Dataset_day$AirTC_Avg.x[grep("07:",Dataset_day$TIMESTAMP)]

histogram(pre)
histogram(pre30up,
          main = "When Sensor Temperature > 30
          [Sensor: Insect-screen-nets house | Station: La Guerite Outdoor]",
          xlim = c(-5,10),
          ylim = c(0,40),
          xlab = "Difference(Sensor-Station)")
histogram(pre.noon,
          main = "Everyday 12:00~13:00,
          [Sensor: Insect-screen-nets house | Station: La Guerite Outdoor]",
          xlim = c(-5,10),
          ylim = c(0,40),
          xlab = "Difference(Sensor-Station)")
histogram(pre.une,
          main = "Everyday 13:00~14:00
          [Sensor: Insect-screen-nets house | Station: La Guerite Outdoor]",
          xlim = c(-5,10),
          ylim = c(0,40),
          xlab = "Difference(Sensor-Station)")
histogram(pre.deux,
          main = "Everyday 14:00~15:00
          [Sensor: Insect-screen-nets house | Station: La Guerite Outdoor]",
          xlim = c(-5,10),
          ylim = c(0,40),
          xlab = "Difference(Sensor-Station)")
histogram(pre.sept,
          main = "Everyday 07:00~08:00
          [Sensor: Insect-screen-nets house | Station: La Guerite Outdoor]",
          xlim = c(-5,10),
          ylim = c(0,40),
          xlab = "Difference(Sensor-Station)")

