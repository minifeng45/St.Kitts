# library the package that will be used (Note: if you don't install' em before, please install first)
library(dplyr)
library(e1071)
library(DAAG)
library(DMwR)
library(leaps)
library(arm)
library(caret)
library(lubridate)
library(car)
###source sensor, weather station data


source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.data_partition.R")

setwd("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg")
### Meterological data insert
## Weather stations
#note: prestest proved that only daytime temperature have difference
WSD.needmust = data_partition(Datasource = "WS.Needmust.dat",
                     start_day = "2020/1/29",
                     end_day = "2020/06/01",
                     Partition_Way = 3) 

WSD.la.guerite = data_partition(Datasource = "WS.La Guerite.dat",
                     start_day = "2020/1/29",
                     end_day = "2020/06/01",
                     Partition_Way = 3)

WSD.eco.park = data_partition(Datasource = "WS.Ecopark.dat",
                     start_day = "2020/1/29",
                     end_day = "2020/06/01",
                     Partition_Way = 3) 

WSD.mansion = data_partition(Datasource = "WS.Mansion.dat",
                     start_day = "2020/1/29",
                     end_day = "2020/06/01",
                     Partition_Way = 3) 

# Mounted sensors

HSD.needmust = data_partition(Datasource = "HS.Needsmust.csv",
                     start_day = "2020/1/29",
                     end_day = "2020/06/01",
                     Partition_Way = 3)

HSD.la.guerite.wire.nets = data_partition(Datasource = "HS.La Guerite 3.csv",
                     start_day = "2020/1/29",
                     end_day = "2020/06/01",
                     Partition_Way =3)

HSD.la.guerite.shading.wire.nets = data_partition(Datasource = "HS.La Guerite 2.csv",
                                           start_day = "2020/1/29",
                                           end_day = "2020/06/01",
                                           Partition_Way = 3)

HSD.la.guerite.insect.screen.nets = data_partition(Datasource = "HS.La Guerite 1.csv",
                                                  start_day = "2020/1/29",
                                                  end_day = "2020/06/01",
                                                  Partition_Way = 3)
### Data pre-processing
## Non-meteorological variables remove
# function

non.meterological.remover = function(dataset,type){
  if(type == "WS.La.guerite"){rm = c("RECORD","TIMESasSECOND")}
  if(type == "HS"){rm = c("DewPointTC_Avg","TIMESasSECOND")}
  if(type == "WS"){rm = c("RECORD","BattV_Avg","PTemp_C_Avg","WS_ms_S_WVT","ETos","Rso","BP_mbar_Avg","WindDir_D1_WVT","WindDir_SD1_WVT","HalfBr","TIMESasSECOND")}
  dataset = dataset[,-which(names(dataset) %in% rm)]
  return(dataset)}

## Missing data deletion
# function

missing.data.remover = function(dataset){
  for(i in 2:ncol(dataset)){
    if (length(which(is.na(dataset[,i]))) != 0) {
      dataset = dataset[-which(is.na(dataset[,i])),] 
    } 
  }
  return(dataset)
}

## Processing

C.WSD.la.guerite = WSD.la.guerite%>%
  non.meterological.remover(.,type = "WS.La.guerite")%>%
  missing.data.remover() 
  
C.WSD.needmust = WSD.needmust%>%
  non.meterological.remover(.,type = "WS")%>%
  missing.data.remover() 

# Add: Fahrenheit -> Celsius
C.HSD.la.guerite.wire.nets = HSD.la.guerite.wire.nets %>%
  non.meterological.remover(.,type = "HS")%>%
  missing.data.remover() %>%
  mutate(AirTC_Avg =  ((AirTC_Avg -32) *5)/9) 
  

C.HSD.la.guerite.shading.wire.nets = HSD.la.guerite.shading.wire.nets %>%
  non.meterological.remover(.,type = "HS")%>%
  missing.data.remover()%>%
  mutate(AirTC_Avg =  ((AirTC_Avg -32) *5)/9) 

C.HSD.la.guerite.insect.screen.nets = HSD.la.guerite.insect.screen.nets %>%
  non.meterological.remover(.,type = "HS")%>%
  missing.data.remover()%>%
  mutate(AirTC_Avg =  ((AirTC_Avg -32) *5)/9) 

C.HSD.needmust = HSD.needmust %>%
  non.meterological.remover(.,type = "HS")%>%
  missing.data.remover() %>%
  mutate(AirTC_Avg =  ((AirTC_Avg -32) *5)/9)

### Establishment of Models 

model.1.dataset =C.WSD.la.guerite %>%
  inner_join(C.HSD.la.guerite.shading.wire.nets, by = "TIMESTAMP", suffix = c("_WS","_HS"))

model.2.dataset = C.WSD.la.guerite %>%
  inner_join(C.HSD.la.guerite.insect.screen.nets, by = "TIMESTAMP", suffix = c("_WS","_HS"))

model.3.dataset = C.WSD.needmust %>%
  inner_join(C.HSD.la.guerite.wire.nets, by = "TIMESTAMP", suffix = c("_WS","_HS"))

model.4.dataset = C.WSD.needmust %>%
  inner_join(C.HSD.needmust, by = "TIMESTAMP", suffix = c("_WS","_HS"))

