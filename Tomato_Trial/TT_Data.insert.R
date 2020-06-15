####Tomato trial data insetion ####

library(readxl)
library(RMySQL)
library(DBI)
library(dplyr)

# using mysql as data desposition 
conn<-dbConnect(MySQL(),dbname="TomatoTrial",user="root",
                password="12345678",host="localhost")
dbSendQuery(conn,'SET NAMES utf8')

# As using localhost, set in local is needed
dbGetQuery(conn,"SET GLOBAL local_infile=1")

###FUCNTION: insert excel file
data_record = function(Variety){
  Variety <- read_excel("~/Desktop/Tomato inspection/Greenhouse/Harvest.xlsx", 
                        sheet = deparse(substitute(Variety)))
## fill the empty entries
  for (i in 1:(nrow(Variety)-1)){
    Variety$No.[which(is.na(Variety$No.))] = Variety$No.[which(is.na(Variety$No.))-1] +1
    Variety$Replicate[which(is.na(Variety$Replicate))] = Variety$Replicate[which(is.na(Variety$Replicate))-1]
    Variety$Date[i+1] = Variety$Date[1]
    Variety$Variety[i+1] = Variety$Variety[1]
  }

## set Date column's value as Date
  Variety$Date = lubridate::ymd(Variety$Date)%>% 
    gsub("-","/",.)
  
## code is for following table name setting
  code = lubridate::ymd(Variety$Date[1]) %>% 
    gsub("-","/",.) %>% 
    gsub("/","",.)
  
  return(list(Variety,code))
}

### upload 4 varieties data to mySQL
for (i in 1:4) {
  if (i ==1) {
    data = data_record(HA3080)
    DBI::dbWriteTable(conn, paste("HA3080",data[[2]],sep = "_"), data[[1]], overwrite = T)
    print(DBI::dbWriteTable(conn, paste("HA3080",data[[2]],sep = "_"), data[[1]], overwrite = T))
  }
  if (i ==2){
    data = data_record(Tyranus)
    DBI::dbWriteTable(conn, paste("Tyranus",data[[2]],sep = "_"), data[[1]], overwrite = T)
    print(DBI::dbWriteTable(conn, paste("Tyranus",data[[2]],sep = "_"), data[[1]], overwrite = T))
  }
  if (i ==3){
    data = data_record(Farmer933)
    DBI::dbWriteTable(conn, paste("Farmer933",data[[2]],sep = "_"), data[[1]], overwrite = T)
    print(DBI::dbWriteTable(conn, paste("Farmer933",data[[2]],sep = "_"), data[[1]], overwrite = T))
  }
  if (i ==4){
    data = data_record(MEIHUI)
    DBI::dbWriteTable(conn, paste("MEIHUI",data[[2]],sep = "_"), data[[1]], overwrite = T)
    print(DBI::dbWriteTable(conn, paste("MEIHUI",data[[2]],sep = "_"), data[[1]], overwrite = T))
  }
  }

dbDisconnect(conn)

