rmse <- function(y, ychap, digits=0){
  return( round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits) )
}

mape <- function(y, ychap, digits=1){
  return ( round((100/length(y)) * sum(abs((y - ychap) / y)), digits=digits) )
}



#(1/n) * Σ(|Original – Predicted| / |Original|) * 100