### Tomato trial, Database from mySQL 
library(RMySQL)
library(DBI)
library(dplyr)
library(readxl)
library(lubridate)
conn<-dbConnect(MySQL(),dbname="TomatoTrial",user="root",
                password="12345678",host="localhost")

# dim harvest 
harvest.times  = length(dbListTables(conn))/4
replicates = 5

# insert main database from MySQL workbench
Harvest = data.frame()
for (i in dbListTables(conn)) {
  Harvest = rbind(Harvest,dbReadTable(conn,name = i)) %>%
    data.frame()
}


### summary of harvest, for single plot[Variety,Replicate]
T.rough.summary <- Harvest %>%
  group_by(Variety,Replicate) %>%
  summarize(total = sum(Weight),
            avr.fruit.weight =mean(Weight),
            fruit.number = length(NO.))

### insert related database
plant.number = read_excel("/Users/supermonk00/Desktop/programing/R/St.Kitts/Tomato_Trial/TT_Plant_number.xlsx")
plant.damage.loss = read_excel("/Users/supermonk00/Desktop/programing/R/St.Kitts/Tomato_Trial/TT_Damage.loss.xlsx")


### join weekly plant number information to Harvest database

Harvest.final = rbind(
  Harvest_before.0502=plant.number %>%
    filter(ymd(Update_date) == ymd("2020-04-27")) %>%
    right_join(Harvest, by=c("Replicate","Variety")) %>%
    filter(ymd(Date) <=  ymd("2020-05-02")),
  
  
  Harvest_before.0505=plant.number %>%
    filter(ymd(Update_date) == ymd("2020-05-02")) %>%
    right_join(Harvest, by=c("Replicate","Variety")) %>%
    filter(ymd(Date) <=  ymd("2020-05-05"),ymd(Date) >  ymd("2020-05-02")),
  
  Harvest_before.0603=plant.number %>%
    filter(ymd(Update_date) == ymd("2020-05-05")) %>%
    right_join(Harvest, by=c("Replicate","Variety")) %>%
    filter(ymd(Date) <=  ymd("2020-06-03"),ymd(Date) >  ymd("2020-05-05")),
  
  Harvest_after.0603=plant.number %>%
    filter(ymd(Update_date) == ymd("2020-06-03")) %>%
    right_join(Harvest, by=c("Replicate","Variety")) %>%
    filter(ymd(Date) >  ymd("2020-06-03"))
)


### summary of harvest for RCBD experiment analysis, for single plot[Variety,Replicate]
T.harvest.summarise = Harvest.final %>% 
  filter(Plant_number !=0 )%>%
  group_by(Variety,Replicate)%>%
  summarize(total = sum(Weight/Plant_number),
            avr.fruit.weight = mean(Weight),
            fruit.number.per.plant = length(NO.)/harvest.times/replicates,
            sd.fruit.weight = sd(Weight))%>%
  mutate(avr.total.weight = fruit.number.per.plant*avr.fruit.weight) 


### summary of harvest loss due to pesticide and phyiscal damage.
T.loss.summarise = plant.damage.loss %>%
  group_by(Variety) %>%
  summarise(loss.pest = sum(Pest),
            loss.blackrot = sum(Blackrot),
            loss.rotten = sum(Rotten),
            loss.crack = sum(Crack))
  
### Compare the loss among four varieties
T.loss.compare = Harvest %>%
  group_by(Variety) %>%
  summarize(total = sum(Weight),
            avr.fruit.weight =mean(Weight),
            fruit.number = length(NO.)) %>%
  left_join(T.loss.summarise,by ="Variety") %>%
  mutate(loss.weight = avr.fruit.weight*(loss.pest+loss.blackrot+loss.rotten+loss.crack),
         loss.percent = loss.weight/(loss.weight+total)) %>%
  select(Variety,loss.weight,loss.percent)

### Layout the percent of plant death due to disease
T.diease.rate = plant.number %>%
  filter(ymd(Update_date) == ymd("2020-06-03")) %>%
  group_by(Variety) %>%
  summarise(diease.rate = 1-(sum(Plant_number)/10),
            death  = 10 - sum(Plant_number))
  

### investigate on the rest of the fruit on tomatoes after the last harvest

data_record.the.rest = function(Variety){
  Variety <- read_excel("~/Desktop/Tomato inspection/Greenhouse/Harvest.the.rest.xlsx", 
                        sheet = deparse(substitute(Variety)))
  for (i in 1:(nrow(Variety)-1)){
    Variety$No.[which(is.na(Variety$No.))] = Variety$No.[which(is.na(Variety$No.))-1]+1
    Variety$Replicate[which(is.na(Variety$Replicate))] = Variety$Replicate[which(is.na(Variety$Replicate))-1]
    Variety$Date[i+1] = Variety$Date[1]
    Variety$Variety[i+1] = Variety$Variety[1]
  }
  return(Variety)
}

The.rest= rbind(data_record.the.rest(HA3080),
                data_record.the.rest(Tyranus),
                data_record.the.rest(Farmer933),
                data_record.the.rest(MEIHUI))

T.The.rest  = The.rest %>%
  group_by(Variety) %>%
  summarise(total = sum(Weight),
            fruit.number = length(Date),
            fruit.average = mean(Weight))

Tomato.Trial = list(Rough.summary = T.rough.summary,
                    Summary = T.harvest.summarise,
                    Loss.summary = T.loss.summarise,
                    Loss.comparison = T.loss.compare,
                    Disease.rate = T.diease.rate,
                    Remaing.fruit = T.The.rest)

