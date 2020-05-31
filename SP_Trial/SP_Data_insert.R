library(readxl)
###################################################
#################PARAMATER CHANGE##################
###################################################

#insert data before TRIM treatment
purple_before_trim = read_excel("~/Desktop/programing/R/St.Kitts/SP_Trial/甜椒田間試驗數據.xlsx", 
                      sheet = "Purple")
red_before_trim = read_excel("~/Desktop/programing/R/St.Kitts/SP_Trial/甜椒田間試驗數據.xlsx", 
                                sheet = "Red")
#insert data after TRIM treatment
purple_trim = read_excel("~/Desktop/programing/R/St.Kitts/SP_Trial/甜椒田間試驗數據.xlsx", 
                                sheet = "Purple(trim)")
purple_notrim = read_excel("~/Desktop/programing/R/St.Kitts/SP_Trial/甜椒田間試驗數據.xlsx", 
                         sheet = "Purple(notrim)")
red_trim = read_excel("~/Desktop/programing/R/St.Kitts/SP_Trial/甜椒田間試驗數據.xlsx", 
                         sheet = "Red(trim)")
red_notrim = read_excel("~/Desktop/programing/R/St.Kitts/SP_Trial/甜椒田間試驗數據.xlsx", 
                         sheet = "Red(notrim)")



