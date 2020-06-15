# Establishment of Model1: Shading-wire-nets house nowcast
# Poly choose : 5 degree
model1.bestsubset = regsubsets(model.1.dataset$AirTC_Avg_HS~.,
                               data.frame(cbind(
                                 model.1.dataset$AirTC_Avg_WS,
                                 model.1.dataset$AirTC_Avg_WS^2,
                                 model.1.dataset$AirTC_Avg_WS^3,
                                 model.1.dataset$AirTC_Avg_WS^4,
                                 model.1.dataset$AirTC_Avg_WS^5,
                                 model.1.dataset$RH_WS,
                                 model.1.dataset$RH_WS^2,
                                 model.1.dataset$RH_WS^3,
                                 model.1.dataset$RH_WS^4,
                                 model.1.dataset$RH_WS^5,
                                 model.1.dataset$SlrkW_Avg,
                                 model.1.dataset$SlrkW_Avg^2,
                                 model.1.dataset$SlrkW_Avg^3,
                                 model.1.dataset$SlrkW_Avg^4,
                                 model.1.dataset$SlrkW_Avg^5,
                                 model.1.dataset$SlrMJ_Tot,
                                 model.1.dataset$SlrMJ_Tot^2,
                                 model.1.dataset$SlrMJ_Tot^3,
                                 model.1.dataset$SlrMJ_Tot^4,
                                 model.1.dataset$SlrMJ_Tot^5,
                                 model.1.dataset$Rain_mm_Tot,
                                 model.1.dataset$Rain_mm_Tot^2,
                                 model.1.dataset$Rain_mm_Tot^3,
                                 model.1.dataset$Rain_mm_Tot^4,
                                 model.1.dataset$Rain_mm_Tot^5,
                                 model.1.dataset$VWC_Avg,
                                 model.1.dataset$VWC_Avg^2,
                                 model.1.dataset$VWC_Avg^3,
                                 model.1.dataset$VWC_Avg^4,
                                 model.1.dataset$VWC_Avg^5,
                                 model.1.dataset$EC_Avg,
                                 model.1.dataset$EC_Avg^2,
                                 model.1.dataset$EC_Avg^3,
                                 model.1.dataset$EC_Avg^4,
                                 model.1.dataset$EC_Avg^5,
                                 model.1.dataset$T_Avg,
                                 model.1.dataset$T_Avg^2,
                                 model.1.dataset$T_Avg^3,
                                 model.1.dataset$T_Avg^4,
                                 model.1.dataset$T_Avg^5)),nvmax =40 , nbest = 3,method = "forward")
reg.summary = summary(model1.bestsubset)
plot(model1.bestsubset, scale = "bic",  xlab = "regressor")
plot(model1.bestsubset, scale = "Cp", xlab = "regressor")
md1 = coef(model1.bestsubset,which(rank(reg.summary$bic) == 1))
md2 = coef(model1.bestsubset,which(rank(reg.summary$bic) == 2))
md3 = coef(model1.bestsubset,which(rank(reg.summary$bic) == 3))

# cross validation
set.seed(15112)
train = model.1.dataset[sample(nrow(model.1.dataset),nrow(model.1.dataset)*0.8),]
test = model.1.dataset[-(sample(nrow(model.1.dataset),nrow(model.1.dataset)*0.8)),]


train.control <- trainControl(method = "cv", number = 10)


model1 <- train(data.frame(cbind(model.1.dataset$AirTC_Avg_WS,
                                 model.1.dataset$AirTC_Avg_WS^2,
                                 model.1.dataset$AirTC_Avg_WS^3,
                                 model.1.dataset$AirTC_Avg_WS^4,
                                 model.1.dataset$AirTC_Avg_WS^5,
                                 model.1.dataset$RH_WS,
                                 model.1.dataset$RH_WS^2,
                                 model.1.dataset$RH_WS^5,
                                 model.1.dataset$SlrkW_Avg^2,
                                 model.1.dataset$SlrkW_Avg^5,
                                 model.1.dataset$SlrMJ_Tot,
                                 model.1.dataset$SlrMJ_Tot^3,
                                 model.1.dataset$SlrMJ_Tot^5,
                                 model.1.dataset$VWC_Avg,
                                 model.1.dataset$VWC_Avg^4,
                                 model.1.dataset$VWC_Avg^5,
                                 model.1.dataset$EC_Avg,
                                 model.1.dataset$EC_Avg^2,
                                 model.1.dataset$EC_Avg^3,
                                 model.1.dataset$EC_Avg^4,
                                 model.1.dataset$EC_Avg^5,
                                 model.1.dataset$T_Avg,
                                 model.1.dataset$T_Avg^2,
                                 model.1.dataset$T_Avg^3,
                                 model.1.dataset$T_Avg^4,
                                 model.1.dataset$T_Avg^5)),
                model.1.dataset$AirTC_Avg_HS, method = "lm",
                trControl = train.control)

model2 <- train(data.frame(cbind(model.1.dataset$AirTC_Avg_WS,
                                 model.1.dataset$AirTC_Avg_WS^2,
                                 model.1.dataset$AirTC_Avg_WS^3,
                                 model.1.dataset$AirTC_Avg_WS^4,
                                 model.1.dataset$AirTC_Avg_WS^5,
                                 model.1.dataset$RH_WS,
                                 model.1.dataset$RH_WS^2,
                                 model.1.dataset$RH_WS^5,
                                 model.1.dataset$SlrkW_Avg^2,
                                 model.1.dataset$SlrkW_Avg^5,
                                 model.1.dataset$SlrMJ_Tot,
                                 model.1.dataset$SlrMJ_Tot^3,
                                 model.1.dataset$SlrMJ_Tot^5,
                                 model.1.dataset$VWC_Avg,
                                 model.1.dataset$VWC_Avg^3,
                                 model.1.dataset$VWC_Avg^4,
                                 model.1.dataset$VWC_Avg^5,
                                 model.1.dataset$EC_Avg,
                                 model.1.dataset$EC_Avg^2,
                                 model.1.dataset$EC_Avg^3,
                                 model.1.dataset$EC_Avg^5,
                                 model.1.dataset$T_Avg,
                                 model.1.dataset$T_Avg^2,
                                 model.1.dataset$T_Avg^3,
                                 model.1.dataset$T_Avg^4,
                                 model.1.dataset$T_Avg^5)),
                model.1.dataset$AirTC_Avg_HS, method = "lm",
                trControl = train.control)

model3 <- train(data.frame(cbind(model.1.dataset$AirTC_Avg_WS,
                                 model.1.dataset$AirTC_Avg_WS^2,
                                 model.1.dataset$AirTC_Avg_WS^3,
                                 model.1.dataset$AirTC_Avg_WS^4,
                                 model.1.dataset$AirTC_Avg_WS^5,
                                 model.1.dataset$RH_WS,
                                 model.1.dataset$RH_WS^2,
                                 model.1.dataset$RH_WS^5,
                                 model.1.dataset$SlrkW_Avg^2,
                                 model.1.dataset$SlrkW_Avg^5,
                                 model.1.dataset$SlrMJ_Tot,
                                 model.1.dataset$SlrMJ_Tot^3,
                                 model.1.dataset$SlrMJ_Tot^5,
                                 model.1.dataset$VWC_Avg,
                                 model.1.dataset$VWC_Avg^2,
                                 model.1.dataset$VWC_Avg^4,
                                 model.1.dataset$VWC_Avg^5,
                                 model.1.dataset$EC_Avg,
                                 model.1.dataset$EC_Avg^2,
                                 model.1.dataset$EC_Avg^3,
                                 model.1.dataset$EC_Avg^5,
                                 model.1.dataset$T_Avg,
                                 model.1.dataset$T_Avg^2,
                                 model.1.dataset$T_Avg^3,
                                 model.1.dataset$T_Avg^4,
                                 model.1.dataset$T_Avg^5)),
                model.1.dataset$AirTC_Avg_HS, method = "lm",
                trControl = train.control)
rbind(model1$results,model2$results,model3$results)

# vif test

vif(lm(model.1.dataset$AirTC_Avg_HS~model.1.dataset$AirTC_Avg_WS+
       model.1.dataset$AirTC_Avg_WS^2+
       model.1.dataset$AirTC_Avg_WS^3+
       model.1.dataset$AirTC_Avg_WS^4+
       model.1.dataset$AirTC_Avg_WS^5+
       model.1.dataset$RH_WS+
       model.1.dataset$RH_WS^2+
       model.1.dataset$RH_WS^5+
       model.1.dataset$SlrkW_Avg^2+
       model.1.dataset$SlrkW_Avg^5+
       model.1.dataset$SlrMJ_Tot+
       model.1.dataset$SlrMJ_Tot^3+
       model.1.dataset$SlrMJ_Tot^5+
       model.1.dataset$VWC_Avg+
       model.1.dataset$VWC_Avg^3+
       model.1.dataset$VWC_Avg^4+
       model.1.dataset$VWC_Avg^5+
       model.1.dataset$EC_Avg+
       model.1.dataset$EC_Avg^2+
       model.1.dataset$EC_Avg^3+
       model.1.dataset$EC_Avg^5+
       model.1.dataset$T_Avg+
       model.1.dataset$T_Avg^2+
       model.1.dataset$T_Avg^3+
       model.1.dataset$T_Avg^4+
       model.1.dataset$T_Avg^5))
### LASSO Reg
library(glmnet)
library(caret)
set.seed(15112)
train = model.1.dataset[sample(nrow(model.1.dataset),nrow(model.1.dataset)*0.8),]
test = model.1.dataset[-(sample(nrow(model.1.dataset),nrow(model.1.dataset)*0.8)),]

x_train = as.matrix(cbind(train$AirTC_Avg_WS,
                          train$AirTC_Avg_WS^2,
                          train$AirTC_Avg_WS^3,
                          train$AirTC_Avg_WS^4,
                          train$AirTC_Avg_WS^5,
                          train$RH_WS,
                          train$RH_WS^2,
                          train$RH_WS^5,
                          train$SlrkW_Avg^2,
                          train$SlrkW_Avg^5,
                          train$SlrMJ_Tot,
                          train$SlrMJ_Tot^3,
                          train$SlrMJ_Tot^5,
                          train$VWC_Avg,
                          train$VWC_Avg^3,
                          train$VWC_Avg^4,
                          train$VWC_Avg^5,
                          train$EC_Avg,
                          train$EC_Avg^2,
                          train$EC_Avg^3,
                          train$EC_Avg^5,
                          train$T_Avg,
                          train$T_Avg^2,
                          train$T_Avg^3,
                          train$T_Avg^4,
                          train$T_Avg^5))

y_train = train$AirTC_Avg_HS

x_test = as.matrix(cbind(test$AirTC_Avg_WS,
                         test$AirTC_Avg_WS^2,
                         test$AirTC_Avg_WS^3,
                         test$AirTC_Avg_WS^4,
                         test$AirTC_Avg_WS^5,
                         test$RH_WS,
                         test$RH_WS^2,
                         test$RH_WS^5,
                         test$SlrkW_Avg^2,
                         test$SlrkW_Avg^5,
                         test$SlrMJ_Tot,
                         test$SlrMJ_Tot^3,
                         test$SlrMJ_Tot^5,
                         test$VWC_Avg,
                         test$VWC_Avg^3,
                         test$VWC_Avg^4,
                         test$VWC_Avg^5,
                         test$EC_Avg,
                         test$EC_Avg^2,
                         test$EC_Avg^3,
                         test$EC_Avg^5,
                         test$T_Avg,
                         test$T_Avg^2,
                         test$T_Avg^3,
                         test$T_Avg^4,
                         test$T_Avg^5))
y_test = test$AirTC_Avg_HS

lambdas <- 10^seq(2, -3, by = -.1)

# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

# Best 
lambda_best <- lasso_reg$lambda.min 
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

eval_results = function(true, predicted, df) {
  SSE = sum((predicted - true)^2)
  SST = sum((true - mean(true))^2)
  R_square = 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  MAE = sum(abs(predicted - true))/nrow(df)
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square,
    MAE = MAE
  )
  
}

predictions_train <- predict(lasso_model, s = lambda_best, newx = x_train)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)


coef = rbind(lasso_model$a0,lasso_model$beta) %>%
  as.matrix()

basic_mat = model.matrix(model.1.dataset$AirTC_Avg_HS~.,
                         data.frame(cbind(model.1.dataset$AirTC_Avg_WS,
                                          model.1.dataset$AirTC_Avg_WS^2,
                                          model.1.dataset$AirTC_Avg_WS^3,
                                          model.1.dataset$AirTC_Avg_WS^4,
                                          model.1.dataset$AirTC_Avg_WS^5,
                                          model.1.dataset$RH_WS,
                                          model.1.dataset$RH_WS^2,
                                          model.1.dataset$RH_WS^5,
                                          model.1.dataset$SlrkW_Avg^2,
                                          model.1.dataset$SlrkW_Avg^5,
                                          model.1.dataset$SlrMJ_Tot,
                                          model.1.dataset$SlrMJ_Tot^3,
                                          model.1.dataset$SlrMJ_Tot^5,
                                          model.1.dataset$VWC_Avg,
                                          model.1.dataset$VWC_Avg^3,
                                          model.1.dataset$VWC_Avg^4,
                                          model.1.dataset$VWC_Avg^5,
                                          model.1.dataset$EC_Avg,
                                          model.1.dataset$EC_Avg^2,
                                          model.1.dataset$EC_Avg^3,
                                          model.1.dataset$EC_Avg^5,
                                          model.1.dataset$T_Avg,
                                          model.1.dataset$T_Avg^2,
                                          model.1.dataset$T_Avg^3,
                                          model.1.dataset$T_Avg^4,
                                          model.1.dataset$T_Avg^5)))

### Predictive power evaluation

predict  = basic_mat %*% coef %>%
  as.numeric()
actual =  model.1.dataset$AirTC_Avg_HS

eval_results(actual, predict, model.1.dataset)


### Descriptive statistics
library(ggplot2)
plot.dataset = rbind(
  data.frame(Time = as.POSIXct(model.1.dataset$TIMESTAMP), Temperature = model.1.dataset$AirTC_Avg_WS, Category = "AWS"),
  data.frame(Time = as.POSIXct(model.1.dataset$TIMESTAMP), Temperature = actual, Category = "MS:Actual"),
  data.frame(Time = as.POSIXct(model.1.dataset$TIMESTAMP), Temperature = predict, Category = "Predict")
)

plot.dataset.0202 = plot.dataset %>%
  filter(month(Time) == 2, day(Time) == 2)

p.model.shade = ggplot(plot.dataset.0202, aes(x = Time, y = Temperature, color = Category))+
  geom_line() + 
  labs(title = "Fluctuation of daily temperature", 
       subtitle = "Date:2020/2/2",y = "Air temperature",
       caption = "Model:shading-wire-nets house nowcast")+
  scale_color_manual(values=c('dark blue','dark red','dark green'))



plot.dataset.scatter.shade = data.frame(predict= predict,
                                  actual = actual)

           
p.scatter.actual.vs.predict.shade  = ggplot(plot.dataset.scatter.shade, aes(x = actual , y =predict),size = 0.5)+
  geom_point(alpha = 0.3,size = 0.7,color="dark green")+
  labs(x = "Actual values", y = "Predictive values",
       title = "Air temperature: Actual vs. Predictive values",
       caption = "Model:shading-wire-nets house nowcast")+
  coord_cartesian(xlim = c(22, 44), ylim = c(22, 44))
  


#discussion: SLR vs. ML
SLR = lm(model.1.dataset$AirTC_Avg_HS~model.1.dataset$AirTC_Avg_WS)
new = data.frame(model.1.dataset$AirTC_Avg_WS)
SLR.predict = predict(SLR,new) %>%
  as.numeric()
eval_results(actual, SLR.predict, model.1.dataset)

