# determine inspection date
# layout the time period
dato = seq(as.Date("2019/10/7"), 
           as.Date("2020/3/23"),"day") #from start day to end day
day = format(dato, format = "%Y-%m-%d") #change form to fit excel data
rm(dato)
