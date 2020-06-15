####FUNCTION: Climate minute data cleaner####
## transfer the data value into numeric form, remove unit's row
library(readr)
setwd("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg")

Dataclean = function(
  filename = ""
){
  assertive.clause_dat = !length(grep(".dat",filename))  
  if (!assertive.clause_dat)  {
    Rawdata_min = read_csv(filename,
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
                                            SlrkW_Avg = col_number(),
                                            Rain_mm_Tot = col_number(),
                                            WindDir_SD1_WVT = col_number()),
                           skip = 1)
    
    Rawdata_min = Rawdata_min[-1:-2,]
    
    print("Record.period")
    print(paste("start.time",Rawdata_min$TIMESTAMP[1]))
    print(paste("end.time",Rawdata_min$TIMESTAMP[nrow(Rawdata_min)]))
    return(Rawdata_min)
  }
  assertive.clause_csv = !length(grep(".csv",filename))  
  if (!assertive.clause_csv){
    LaG = read.csv(filename, skip = 1)
    LaG = LaG[,-5:-7]
    colnames(LaG) = c("TIMESTAMP", "AirTC_Avg","RH","DewPointTC_Avg")
    print(paste("start.time",LaG$TIMESTAMP[1]))
    print(paste("end.time",LaG$TIMESTAMP[nrow(LaG)]))
    return(LaG)
  }
}