library(ggplot2)
library(lubridate)


conn<-dbConnect(MySQL(),dbname="TomatoTrial",user="root",
                password="12345678",host="localhost")


# insert main database from MySQL workbench
Harvest = data.frame()
for (i in dbListTables(conn)) {
  Harvest = rbind(Harvest,dbReadTable(conn,name = i)) %>%
    data.frame()
}

# Sum the harvest by variety, date
Harvest.by.date = Harvest %>%
  group_by(Variety,Date) %>%
  summarise(harvest.weight = sum(Weight))

# Give date information of harvest
Date = Harvest.by.date$Date %>%
  ymd() %>%
  sort() %>%
  unique() %>%
  format(.,"%Y/%m/%d")
Variety.name = unique(Harvest.by.date$Variety)


# Set a eatable dataframe for flourish 
harvest.fill.platform =  matrix(0, 
                             ncol = length(Date), 
                             nrow = length(Variety.name)
)

Dataset = data.frame(Variety.name, harvest.fill.platform)
colnames(Dataset) = c("Variety",Date)

# Fill value into each space
for (i in 1:nrow(Harvest.by.date)){
  Dataset[Dataset$Variety == Harvest.by.date$Variety[i],
          which(colnames(Dataset) == Harvest.by.date[i,]$Date)] = Harvest.by.date$harvest.weight[i]
}
# culumative: today's cases = yesterday's + today's
for (j in 3:(ncol(Dataset))) {
  Dataset[,j] = Dataset[,j]+ Dataset[,j-1]
}

#output the dataframe
setwd("/Users/supermonk00/Desktop/programing/R/St.Kitts/Tomato_Trial")
write.csv(Dataset, "TT.bar.chart.race", row.names = FALSE )

