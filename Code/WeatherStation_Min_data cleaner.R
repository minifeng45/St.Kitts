### climate minute data cleaner
## transfer the data value into numeric form, remove unit's row
library(readr)
setwd("~/Desktop/academy/programing/R/St.Kitts/Database")
Rawdata_min = read_csv("CR300Series_Needsmust_Minutely_record.dat",
        col_types = cols(AirTC_Avg = col_number(),
                         BP_mbar_Avg = col_number(),
                         BattV_Avg = col_number(),
                         EC_Avg = col_number(),
                         ETos = col_number(),
                         HalfBr = col_number(),
                         PTemp_C_Avg = col_number(),
                         RECORD = col_number(),
                         RH = col_number(),
                         Rso = col_number(),
                         SlrMJ_Tot = col_number(),
                         SlrW = col_number(),
                         T_Avg = col_number(),
                         VWC_Avg = col_number(),
                         WS_ms_Avg = col_number(),
                         WS_ms_S_WVT = col_number(),
                         WindDir_D1_WVT = col_number(),
                         WindDir_SD1_WVT = col_number()),
        skip = 1)
Rawdata_min = Rawdata_min[-1:-2,]

