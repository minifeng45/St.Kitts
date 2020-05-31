# library the package that will be used (Note: if you don't install' em before, please install first)
library(dplyr)
library(e1071)
library(DAAG)
library(DMwR)
library(leaps)
library(arm)
library(caret)
###source sensor, weather station data


source("/Users/supermonk00/Desktop/programing/R/St.Kitts/WS_reg/WS.data_partition.R")

### find regression: in day & in night

WSD = data_partition(Datasource = "La Guerite_five_min.dat",
                     start_day = "2019/8/29",
                     end_day = "2020/04/12",
                     Partition_Way = 1) # 1 for day/night, 2 for monthly, 3 for both
HSD = data_partition(Datasource = "La Guerite 2 2020-04-12 15_07_43 -0400.csv",
                     start_day = "2019/8/29",
                     end_day = "2020/04/12",
                     Partition_Way = 1)

# remove useless data
ls<-ls()
rm(list=ls[which(ls!='HSD' & ls !='WSD')])
rm(ls)


# day/night data merge 
Data_whole_day = merge(WSD$Day,HSD$Day, by = "TIMESTAMP")
Data_whole_night = merge(WSD$Night,HSD$Night, by = "TIMESTAMP")


# erase impossible data
rm = c("RECORD","BattV_Avg","PTemp_C_Avg","WS_ms_S_WVT","WindDir_D1_WVT","WindDir_SD1_WVT","HalfBr")
Dataset_day = Data_whole_day[,-which(names(Data_whole_day) %in% rm)]
Dataset_night = Data_whole_night[,-which(names(Data_whole_night) %in% rm)]

#pre-processing: fill in NA

for (i in 2:15) {
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

PCAweather = prcomp(x = Dataset_day[,c(2:12)], center = T ,scale. = T)
summary(PCAweather)

library(devtools)
library(ggbiplot)
ggbiplot(PCAweather,alpha = 0.01)


#### bestsubset regression ####
regfit.full = regsubsets(Dataset_day[,"AirTC_Avg.y"]~.,Dataset_day[,2:9],nvmax =11 , nbest = 3)
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

predict = basic_mat %*% md1

ord = order(basic_mat[,2])
sort.predict = basic_mat[ord,] %*% md1



residual.pre = Dataset_day$AirTC_Avg.y-predict
count <- 1:length(residual.pre)

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
plot(count,residual.pre,pch = ".")
lines(predict(lo), col='red', lwd=2)
lines(predict(lo2), col='blue', lwd=2)
lines(predict(lo3), col='green', lwd=2)
lines(predict(lo.cross[[1]]), col='purple', lwd=3)

mean.line = predict(lo.cross[[1]], count, se = TRUE)


# correct residual

out = predict+mean.line$fit



predictors = as.numeric(predict+mean.line$fit)
Reality = Dataset_day[,"AirTC_Avg.y"]
set.seed(15112)
sample(Reality,5000)
sample(sort.predict,5000)
plot(Reality[ord]-predictors[ord])
plot(Reality[ord]-sort.predict)

dat = data_frame(predictors, Reality)
library(ggplot2)
ggplot(data = dat, aes(x = predictors, y = Reality))+
  geom_point()


