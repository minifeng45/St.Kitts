library(RMySQL)
library(DBI)
library(dplyr)
library(readxl)
library(lubridate)
conn<-dbConnect(MySQL(),dbname="TomatoTrial",user="root",
                password="12345678",host="localhost")

harvest.times  = length(dbListTables(conn))/4
replicates = 5


Harvest = data.frame()
for (i in 1:length(dbListTables(conn))) {
  Harvest = data.frame(rbind(Harvest,dbReadTable(conn,name =dbListTables(conn)[i])))
}


Rough.summary <- Harvest %>%
  group_by(Variety)%>%
  summarize(total = sum(Weight),
            average =mean(Weight),
            sd.avr = sd(Weight))


plant.number = read_excel("/Users/supermonk00/Desktop/programing/R/St.Kitts/Tomato_Trial/Tomato.Trial_Plant_number.xlsx")


Harvest_before.0502=plant.number %>%
  filter(ymd(Update_date) == ymd("2020-04-27")) %>%
  right_join(Harvest, by=c("Replicate","Variety")) %>%
  filter(ymd(Date) <=  ymd("2020-05-02"))


Harvest_before.0505=plant.number %>%
  filter(ymd(Update_date) == ymd("2020-05-02")) %>%
  right_join(Harvest, by=c("Replicate","Variety")) %>%
  filter(ymd(Date) <=  ymd("2020-05-05"),ymd(Date) >  ymd("2020-05-02"))

Harvest_after.0505=plant.number %>%
  filter(ymd(Update_date) == ymd("2020-05-05")) %>%
  right_join(Harvest, by=c("Replicate","Variety")) %>%
  filter(ymd(Date) >  ymd("2020-05-05"))

Harvesy.final = rbind(Harvest_before.0502,Harvest_before.0505,Harvest_after.0505)

Harvesy.final %>% 
  group_by(Variety)%>%
  summarize(total = sum(Weight/Plant_number),
            fruit.number.per.plant = length(NO.)/harvest.times/replicates,
            avr.fruit.weight = total/(length(NO.)))%>%
  mutate(avr.total.weight = fruit.number.per.plant*avr.fruit.weight)

Harvesy.final %>% 
  group_by(Replicate,Variety) %>%
  arrange(desc(total))

Harvesy.final %>%

tot.dat = Harvesy.final %>% 
  group_by(Date,Variety) %>%
  summarize(total = sum(Weight/Plant_number))

  
