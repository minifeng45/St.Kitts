library(dplyr)
library(lubridate)
source("/Users/supermonk00/Desktop/programing/R/St.Kitts/SP_Trial/SP_Data_insert.R")
#set harvest day as level's label
harvest_day = c("11/11","11/18","11/25","12/2",
                "12/9","12/16","12/23","12/31",
                "01/06","01/13","01/21","01/27",
                "02/02","02/11","02/18","02/24",
                "03/02","03/09","03/16","03/23")
#Total harvest data
purple_total = rbind(purple_before_trim,purple_notrim,purple_trim)
red_total = rbind(red_before_trim,red_notrim,red_trim)

pivot.purple = purple_total %>%
  group_by(Date) %>%
  summarise(weight = sum(`Weight(g)`),
            mu = mean(`Weight(g)`),
            sd = sd(`Weight(g)`),
            number = length(`Weight(g)`))%>%
  arrange(factor(Date,levels =harvest_day)) 

pivot.red =red_total %>%
  group_by(Date) %>%
  summarise(weight = sum(`Weight(g)`),
            mu = mean(`Weight(g)`),
            sd = sd(`Weight(g)`),
            number = length(`Weight(g)`))%>%
  arrange(factor(Date,levels =harvest_day)) 

Harvest = pivot.purple %>%
  left_join(pivot.red,by = "Date",suffix = c("_purple","_red"))


#TrimTrial harvest data

Harvest_treatment_p = rbind(
purple_trim %>%
  group_by(Date) %>%
  summarise(Weight = sum(`Weight(g)`),
            mu = mean(`Weight(g)`),
            sd = sd(`Weight(g)`),
            number = length(`Weight(g)`))%>%
  mutate(Treatment = "Trim")%>%
  arrange(factor(Date,levels =harvest_day)) ,
purple_notrim %>%
  group_by(Date) %>%
  summarise(Weight = sum(`Weight(g)`),
            mu = mean(`Weight(g)`),
            sd = sd(`Weight(g)`),
            number = length(`Weight(g)`))%>%
  mutate(Treatment = "Notrim")%>%
  arrange(factor(Date,levels =harvest_day)) 
)

Harvest_treatment_r = rbind(
  red_trim %>%
    group_by(Date) %>%
    summarise(Weight = sum(`Weight(g)`),
              mu = mean(`Weight(g)`),
              sd = sd(`Weight(g)`),
              number = length(`Weight(g)`))%>%
    mutate(Treatment = "Trim")%>%
    arrange(factor(Date,levels =harvest_day)) ,
  red_notrim %>%
    group_by(Date) %>%
    summarise(Weight = sum(`Weight(g)`),
              mu = mean(`Weight(g)`),
              sd = sd(`Weight(g)`),
              number = length(`Weight(g)`))%>%
    mutate(Treatment = "Notrim")%>%
    arrange(factor(Date,levels =harvest_day)) 
)

