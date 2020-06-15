### Tomato trial, Database from mySQL 
library(RMySQL)
library(DBI)
library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(agricolae)
conn<-dbConnect(MySQL(),dbname="TomatoTrial",user="root",
                password="12345678",host="localhost")


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



p.barchart.harvest.by.date = ggplot(Harvest, aes(x = as.POSIXct(Date) ,y = Weight))+
  geom_col(fill = "#FF6347")


p.boxplot.weight = ggplot(Harvest,aes(x = Variety ,y = Weight, fill=Variety))+
  geom_boxplot()+
  scale_fill_brewer(palette="YlOrRd")+
  labs(title = "Boxplot: distribution of average fruit weight",
       y = "Weight(g)")


### T.test for average fruit weight comparing

HA = Harvest %>% filter(Variety == "HA3080") %>% select(Weight)
Ty = Harvest %>% filter(Variety == "Tyranus") %>% select(Weight)
Farmer = Harvest %>% filter(Variety == "Farmer933") %>% select(Weight)
MEIHUI = Harvest %>% filter(Variety == "MEIHUI") %>% select(Weight)

Test.avr_weight.St.Kitts = t.test(HA,Ty)
Test.avr_weight.Taiwan = t.test(Farmer,MEIHUI)

miss.imformation.933 = data.frame(Variety = "Farmer933",
                                  Replicate = 1:5,
                                  Plant_number = 2,
                                  NO. = 0,
                                  Date = "2020/05/20",
                                  Weight = 0) %>%
  as.tbl()

miss.imformation.Ty = data.frame(Variety = "Tyranus",
                                  Replicate = 1:5,
                                  Plant_number = 2,
                                  NO. = 0,
                                  Date = "2020/05/28",
                                  Weight = 0) %>%
  as.tbl()


## Due to the week lacking of harvest in particular varieties, fill in 0 in this dataframe

Dataset.without.missing = rbind(Harvest.final[-4] ,miss.imformation.933 ,miss.imformation.Ty )

### analysis conducting, ANOVA & LSD

analytic.form = Dataset.without.missing %>% 
  filter(Plant_number !=0 )%>%
  mutate(Weight = Weight/Plant_number)%>% #renew the weight as one plant in one plot
  group_by(Variety,Replicate)%>%
  summarize(weight = sum(Weight))

# change Replicate from numeric to factor
analytic.form$Replicate = as.factor(analytic.form$Replicate)


# anova model build
aov.model = lm(weight~Variety+Replicate, data=analytic.form) %>%
  aov() 
  
Test.anova = aov.model %>% summary()


# extra: theoretically, LSD.test is unnecessary
Test.LSD.Replicate = agricolae::LSD.test(aov.model,"Replicate")  
Test.LSD.Variety = agricolae::LSD.test(aov.model,"Variety") 
