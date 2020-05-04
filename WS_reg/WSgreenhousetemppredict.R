# library the package that will be used (Note: if you don't install' em before, please install first)
library(dplyr)
library(e1071)
library(DAAG)
library(DMwR)
library(leaps)
library(arm)
library(caret)
###source sensor, weather station data


setwd("/Users/supermonk00/desktop/academy/programing/R/St.Kitts/Code")
source("(Function) Data_partition.R")


### find regression: in day & in night

WSD = data_partition(Datasource = "CR300Series-direct-Needsmust_Minutely_record.dat",
                     start_day = "2019/8/29",
                     end_day = "2020/04/12",
                     Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both

WSD.diffloc = data_partition(Datasource = "La Guerite_five_min.dat",
                     start_day = "2019/8/29",
                     end_day = "2020/04/12",
                     Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both

WSD.100m = data_partition(Datasource = "CR300Series-direct-Eco-park_Minutely_Record.dat",
                     start_day = "2019/8/29",
                     end_day = "2020/04/12",
                     Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both

WSD.200m = data_partition(Datasource = "CR300Series-direct-Mansion_Minutely_Record.dat",
                     start_day = "2019/8/29",
                     end_day = "2020/04/12",
                     Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both

HSD = data_partition(Datasource = "Needsmust.csv",
                     start_day = "2019/8/29",
                     end_day = "2020/04/12",
                     Partition_Way = 1)

HSD.diffloc = data_partition(Datasource = "La Guerite 3 2020-04-12 15_08_52 -0400.csv",
                     start_day = "2019/8/29",
                     end_day = "2020/04/12",
                     Partition_Way = 1)
HSD.diffloc$Day$AirTC_Avg = HSD.diffloc$Day$AirTC_Avg*9/5+32



# day/night data merge 
Dataset_day = merge(WSD$Day,HSD$Day, by = "TIMESTAMP") 
colnames(WSD.200m$Day) = c("TIMESTAMP","RECORD.z","BattV_Avg.z",
                           "PTemp_C_Avg.z","AirTC_Avg.z","RH.z",
                           "BP_mbar_Avg.z","VWC_Avg.z","EC_Avg.z",
                           "T_Avg.z","ETos.z","Rso.z","SlrW.z",
                           "SlrMJ_Tot.z","WS_ms_Avg.z","WS_ms_S_WVT.z",
                           "WindDir_D1_WVT.z","WindDir_SD1_WVT.z")
colnames(HSD.diffloc$Day) = c("TIMESTAMP","AirTC_Avg.y2","RH.y2","DewPointTC_Avg")
Dataset_day = merge(Dataset_day,WSD.200m$Day,by = "TIMESTAMP")
Dataset_day = merge(Dataset_day,HSD.diffloc$Day,by = "TIMESTAMP")

Data_whole_night = merge(WSD$Night,HSD$Night, by = "TIMESTAMP")


# erase impossible data
rm = c("RECORD","BattV_Avg","PTemp_C_Avg","WS_ms_S_WVT","WindDir_D1_WVT","WindDir_SD1_WVT","HalfBr")
Dataset_day = Dataset_day[,-which(names(Dataset_day) %in% rm)]
Dataset_night = Dataset_day[,-which(names(Dataset_day) %in% rm)]

#pre-processing: fill in NA

for (i in 2:35) {
  if (length(which(is.na(Dataset_day[,i]))) != 0) {
    Dataset_day = Dataset_day[-which(is.na(Dataset_day[,i])),] 
  }
}

for (i in 2:15) {
  if (length(which(is.na(Dataset_night[,i]))) != 0) {
    Dataset_night = Dataset_night[-which(is.na(Dataset_night[,i])),] 
  }
}


#### PCA for variable relationship####

PCAweather = prcomp(x = Dataset_day[,c(2:35)], center = T ,scale. = T)
summary(PCAweather)

library(devtools)
library(ggbiplot)
ggbiplot(PCAweather,alpha = 0.01)


#### bestsubset regression ####
regfit.full = regsubsets(Dataset_day[,"AirTC_Avg.y"]~.,Dataset_day[,2:12],nvmax =11 , nbest = 3)
reg.summary = summary(regfit.full)
# confirm plot: check adjr2,cp,bic

plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted RSq", type = "l")
plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
plot(reg.summary$bic, xlab = "Number of variables", ylab = "bic", type = "l")


plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
plot(regfit.full, scale = "adjr2")


#chose bic as criteria

md1 = coef(regfit.full,which(order(reg.summary$bic) == 1))
md2 = coef(regfit.full,which(order(reg.summary$bic) == 2))
md3 = coef(regfit.full,which(order(reg.summary$bic) == 3))



# cross validation
set.seed(15112)
train = Dataset_day[sample(nrow(Dataset_day),nrow(Dataset_day)*0.8),]
test = Dataset_day[-(sample(nrow(Dataset_day),nrow(Dataset_day)*0.8)),]


train.control <- trainControl(method = "cv", number = 10)
model1 <- train(Dataset_day[,names(md1)[-1]], Dataset_day[,"AirTC_Avg.y"], method = "lm",
                trControl = train.control)

model2 <- train(Dataset_day[,names(md2)[-1]], Dataset_day[,"AirTC_Avg.y"], method = "lm",
                trControl = train.control)

model3 <- train(Dataset_day[,names(md3)[-1]], Dataset_day[,"AirTC_Avg.y"], method = "lm",
                trControl = train.control)

# choose the win model

basic_mat = model.matrix(Dataset_day[,"AirTC_Avg.y"]~.,Dataset_day[,names(md1)[-1]])
outlocate_mat = model.matrix(Dataset_day[,"AirTC_Avg.y"]~.,Dataset_day[,c("AirTC_Avg.z","RH.z",
                                                                          "BP_mbar_Avg.z","VWC_Avg",
                                                                          "EC_Avg.z","T_Avg.z",
                                                                          "ETos.z","Rso.z","SlrW.z",
                                                                          "SlrMJ_Tot.z")])
predict = basic_mat %*% md1
predict.out = outlocate_mat %*% md1

residual.pre = Dataset_day$AirTC_Avg.y-predict
residual.pre.out = Dataset_day$AirTC_Avg.y-predict.out

count <- 1:length(residual.pre)
count.out <- 1:length(residual.pre.out)
#----------------------------------------------------------------------#
# fit data points with LOESS + cross validation
#----------------------------------------------------------------------#
library(bootstrap)
loess_wrapper_extrapolate <- function (x, y, span.vals = seq(0.25, 1, by = 0.05), folds = 10){
  # Do model selection using mean absolute error, which is more robust than squared error.
  mean.abs.error <- numeric(length(span.vals))
  
  # Quantify error for each span, using CV
  loess.model <- function(x, y, span){
    loess(y ~ x, span = span, control=loess.control(surface="direct"))
  }
  
  loess.predict <- function(fit, newdata) {
    predict(fit, newdata = newdata)
  }
  
  span.index <- 0
  for (each.span in span.vals) {
    span.index <- span.index + 1
    y.hat.cv <- crossval(x, y, theta.fit = loess.model, theta.predict = loess.predict, span = each.span, ngroup = folds)$cv.fit
    non.empty.indices <- !is.na(y.hat.cv)
    mean.abs.error[span.index] <- mean(abs(y[non.empty.indices] - y.hat.cv[non.empty.indices]))
  }
  # find the span which minimizes error
  best.span <- span.vals[which.min(mean.abs.error)]
  
  # fit and return the best model
  best.model <- loess(y ~ x, span = best.span, control=loess.control(surface="direct"))
  infomlist = list(best.model,mean.abs.error)
  return(infomlist)
}


lo <- loess(residual.pre~count,span = 0.5)
lo2 <- loess(residual.pre~count,span = 0.25)
lo3 <- loess(residual.pre~count,span = 0.1)



lo.cross = loess_wrapper_extrapolate(count,residual.pre)
lo.cross.out = loess_wrapper_extrapolate(count.out,residual.pre.out)
plot(count,residual.pre.out,pch = ".")
lines(predict(lo), col='red', lwd=2)
lines(predict(lo2), col='blue', lwd=2)
lines(predict(lo3), col='green', lwd=2)
lines(predict(lo.cross.out[[1]]), col='purple', lwd=3)

mean.line = predict(lo.cross.out[[1]], count, se = TRUE)


# correct residual

out = predict.out+mean.line$fit



predictors = as.numeric(out)
Reality = Dataset_day[,"AirTC_Avg.z"]


dat = data_frame(predict.out, Reality)
library(ggplot2)
ggplot(data = dat, aes(x = predict.out, y = Reality))+
  geom_point()

predict.interval = c(1:5)
for (i in predict.interval) {
  probility = length(which((Reality-predict)<i))/length(Reality)
  probility.cor = length(which((Reality-predictors)<i))/length(Reality)
  print(probility)
  print(probility.cor)
}


