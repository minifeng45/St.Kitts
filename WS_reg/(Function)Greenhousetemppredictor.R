temporary =3

temppredict = function(temporary = NULL){
  #Raw.predictor
  basic_value = model.matrix(Dataset_day[temporary,"AirTC_Avg.y"]~.,Dataset_day[temporary,names(md1)[-1]])
  predictor = basic_value %*% md1
  
  mean.line = predict(lo.cross[[1]], count[temporary], se = TRUE)
  
  out = predict[temporary]+mean.line$fit
  return(cbind(Dataset_day[temporary,"AirTC_Avg.y"],out))
  }
  