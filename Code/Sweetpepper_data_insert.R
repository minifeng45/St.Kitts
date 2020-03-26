library(readxl)
###################################################
#################PARAMATER CHANGE##################
###################################################
harvest_day = c("11/11","11/18","11/25","12/2",
                "12/9","12/16","12/23","12/31",
                "01/06","01/13","01/21","01/27",
                "02/02","02/11","02/18","02/24",
                "03/02","03/09","03/16","03/23")

#insert data before TRIM treatment
purple_before_trim = read_excel("~/Desktop/工作/外交替/聖克資料/甜椒田間試驗數據.xlsx", 
                      sheet = "Purple")
red_before_trim = read_excel("~/Desktop/工作/外交替/聖克資料/甜椒田間試驗數據.xlsx", 
                                sheet = "Red")
#insert data after TRIM treatment
purple_trim = read_excel("~/Desktop/工作/外交替/聖克資料/甜椒田間試驗數據.xlsx", 
                                sheet = "Purple(trim)")
purple_notrim = read_excel("~/Desktop/工作/外交替/聖克資料/甜椒田間試驗數據.xlsx", 
                         sheet = "Purple(notrim)")
red_trim = read_excel("~/Desktop/工作/外交替/聖克資料/甜椒田間試驗數據.xlsx", 
                         sheet = "Red(trim)")
red_notrim = read_excel("~/Desktop/工作/外交替/聖克資料/甜椒田間試驗數據.xlsx", 
                         sheet = "Red(notrim)")



