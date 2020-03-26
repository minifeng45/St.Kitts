### climate minute data cleaner
library(readr)
setwd("~/Desktop/academy/programing/R/St.Kitts/Database")

#greenhouse
LaG1_Rawdata_min = read.csv("La Guerite.csv", skip = 1)
LaG1_Rawdata_min = LaG1_Rawdata_min[,-5:-7]
LaG2_Rawdata_min = read.csv("La Guerite 2.csv", skip = 1)
LaG2_Rawdata_min = LaG2_Rawdata_min[,-5:-7]
LaG3_Rawdata_min = read.csv("La Guerite 3.csv", skip = 1)
LaG3_Rawdata_min = LaG3_Rawdata_min[,-5:-7]
#Needmust farm
Needmust_Rawdata_min = read.csv("Needsmust.csv", skip = 1)
Needmust_Rawdata_min = Needmust_Rawdata_min[,-5:-7]


#for only 1&3
HOBO_Rawdata_min13 = merge(LaG1_Rawdata_min,LaG3_Rawdata_min, by = "Date.Time..GMT..0400")
colnames(HOBO_Rawdata_min13) = c("TIMESTAMP", "LA1T(DegC)","LA1RH(%)","LA1DewPointT(DegC)",
                               "LA3T(DegC)","LA3RH(%)","LA3DewPointT(DegC)")
HOBO_Rawdata_min13$`LA1T(DegC)` = round(((HOBO_Rawdata_min13$`LA1T(DegC)`-32)*5/9),digits = 1)
HOBO_Rawdata_min13$`LA1DewPointT(DegC)` = round(((HOBO_Rawdata_min13$`LA1DewPointT(DegC)`-32)*5/9),digits = 1)
HOBO_Rawdata_min13$`LA3T(DegC)` = round(((HOBO_Rawdata_min13$`LA3T(DegC)`-32)*5/9),digits = 1)
HOBO_Rawdata_min13$`LA3DewPointT(DegC)` = round(((HOBO_Rawdata_min13$`LA3DewPointT(DegC)`-32)*5/9),digits = 1)

#for only 1~3
HOBO_Rawdata_min123 = merge(LaG1_Rawdata_min,LaG2_Rawdata_min, by = "Date.Time..GMT..0400")
HOBO_Rawdata_min123 = merge(HOBO_Rawdata_min123,LaG3_Rawdata_min, by = "Date.Time..GMT..0400")

colnames(HOBO_Rawdata_min123) = c("TIMESTAMP", "LA1T(DegC)","LA1RH(%)","LA1DewPointT(DegC)",
                          "LA2T(DegC)","LA2RH(%)","LA2DewPointT(DegC)",
                          "LA3T(DegC)","LA3RH(%)","LA3DewPointT(DegC)")
HOBO_Rawdata_min123$`LA1T(DegC)` = round(((HOBO_Rawdata_min123$`LA1T(DegC)`-32)*5/9),digits = 1)
HOBO_Rawdata_min123$`LA1DewPointT(DegC)` = round(((HOBO_Rawdata_min123$`LA1DewPointT(DegC)`-32)*5/9),digits = 1)
HOBO_Rawdata_min123$`LA2T(DegC)` = round(((HOBO_Rawdata_min123$`LA2T(DegC)`-32)*5/9),digits = 1)
HOBO_Rawdata_min123$`LA2DewPointT(DegC)` = round(((HOBO_Rawdata_min123$`LA2DewPointT(DegC)`-32)*5/9),digits = 1)
HOBO_Rawdata_min123$`LA3T(DegC)` = round(((HOBO_Rawdata_min123$`LA3T(DegC)`-32)*5/9),digits = 1)
HOBO_Rawdata_min123$`LA3DewPointT(DegC)` = round(((HOBO_Rawdata_min123$`LA3DewPointT(DegC)`-32)*5/9),digits = 1)
rm(LaG1_Rawdata_min,LaG2_Rawdata_min,LaG3_Rawdata_min)

#for needmust
colnames(Needmust_Rawdata_min) = c("TIMESTAMP", "NeedmustT(DegC)","NeedmustRH(%)","NeedmustDewPointT(DegC)")
Needmust_Rawdata_min$`NeedmustT(DegC)` = round(((Needmust_Rawdata_min$`NeedmustT(DegC)`-32)*5/9),digits = 1)
Needmust_Rawdata_min$`NeedmustDewPointT(DegC)` = round(((Needmust_Rawdata_min$`NeedmustDewPointT(DegC)`-32)*5/9),digits = 1)
