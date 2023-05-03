rmse <- function(y, ychap, digits = 0) {
  round(sqrt(mean((y - ychap)^2, na.rm = TRUE)), digits = digits)
}


mape<- function(dataact,dataforecast)
{
  return(mean(abs((dataact-dataforecast)/dataact)) * 100)
}


logregobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  preds <- 1/(1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  return(list(grad = grad, hess = hess))
}
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  err <- as.numeric(sum(labels != (preds > 0)))/length(labels)
  return(list(metric = "error", value = err))
}
