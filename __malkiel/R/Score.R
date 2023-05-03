rmse <- function(y, ychap, digits=0){
  return( round(sqrt(mean((y-ychap)^2, na.rm=TRUE)), digits=digits) )
}

mape <- function(y, ychap, digits=1){
  return ( round((100/length(y)) * sum(abs((y - ychap) / y)), digits=digits) )
}

buildBlock <- function (Nblock, data0) 
{
  borne_block <- seq(1, nrow(data0), length = Nblock + 1) %>% 
    floor
  block_list <- list()
  l <- length(borne_block)
  for (i in c(2:(l - 1))) {
    block_list[[i - 1]] <- c(borne_block[i - 1]:(borne_block[i] - 
                                                   1))
  }
  block_list[[l - 1]] <- c(borne_block[l - 1]:(borne_block[l]))
  return(block_list)
}

forecastBlock<-function(block, formula, data)
{
  g<- mgcv::gam(formula, data=data[-block,] )
  forecast<-predict(g, newdata=data[block,])
  return(forecast)
}

forecastBlockSat <-function(block, formula, data, datasat)
{
  g <- mgcv::gam(formula, data=data[-block,] )
  forecast <-predict(g, newdata=datasat[block,])
  return(forecast)
}


rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE))/1000,digits=2))
}

mape<-function(y,ychap)
{
  return(round(100*mean(abs(y-ychap)/abs(y)),digits=2))
}

mae<-function(y,ychap)
{
  return(round(mean(abs(y-ychap))/1000,digits=2))
}



ar_variables <- function(use,offset,indices) {
  vars <- matrix(0,length(use)-offset,length(indices))
  for (i in 1:length(indices))
    vars[,i] <- use[(offset+1-indices[i]):(length(use)-indices[i])]
  vars
}


ridge <- function(X,y,lambda=1) {
  P <- diag(d) * lambda
  theta <- matrix(0,d,1)
  for (t in 1:nrow(X)) {
    P <- P - tcrossprod(P %*% X[t,]) / (1 + (t(X[t,]) %*% P %*% X[t,])[1])
    theta <- theta + P %*% X[t,] * (y[t] - (t(theta) %*% X[t,])[1])
  }
  theta
}
predict_offline_discriminate <- function(X,y,train_set,discr) {
  prev <- numeric(length(y))
  d <- dim(X)[2]
  for (val in unique(discr)) {
    select <- train_set[which(discr[train_set] == val)]
    theta <- ridge(X[select,],y[select])
    prev[discr==val] <- X[discr==val,] %*% theta
  }
  prev
}

predict_online <- function(parameters_init,X,y,Q=0,sigma=1,delay=1) {
  parameters <- parameters_init
  d <- dim(X)[2]
  yhat <- numeric(length(y))
  theta <- parameters_init$theta
  P <- parameters_init$P
  for (t in 1:length(y)) {
    if (t > delay) {
      P <- P - tcrossprod(P %*% X[t-delay,]) / (sigma^2 + (t(X[t-delay,]) %*% P %*% X[t-delay,])[1])
      theta <- theta + P %*% X[t-delay,] * (y[t-delay] - (t(theta) %*% X[t-delay,])[1]) / sigma^2
      P <- P + Q
    }
    yhat[t] <- crossprod(theta, X[t,])[1]
  }
  yhat
}
predict_online_discriminate <- function(X,y,discr,l=NULL,theta1=NULL,P1=NULL,
                                        delay=1) {
  prev <- numeric(length(y))
  for (val in unique(discr)) {
    i <- which(l$values == val)
    select <- which(discr == val)
    if (is.null(l))
      prev[select] <- predict_online(list('theta'=theta1,'P'=P1),
                                     X[select,], y[select], delay=delay)
    else
      prev[select] <- predict_online(list('theta'=l$theta1_arr[i,],
                                          'P'=l$P1_arr[i,,]), X[select,], y[select],
                                     Q=l$Q_arr[i,,], sigma=l$sigma_arr[i], delay=delay)
  }
  prev
}
predict_online_variances <- function(parameters_init,X,y,Q=0,sigma=1,delay=1) {
  parameters <- parameters_init
  d <- dim(X)[2]
  yvar <- numeric(length(y))
  theta <- parameters_init$theta
  P <- parameters_init$P
  for (t in 1:length(y)) {
    if (t > delay) {
      P <- P - tcrossprod(P %*% X[t-delay,]) / (sigma^2 + (t(X[t-delay,]) %*% P %*% X[t-delay,])[1])
      theta <- theta + P %*% X[t-delay,] * (y[t-delay] - (t(theta) %*% X[t-delay,])[1]) / sigma^2
      P <- P + Q
    }
    yvar[t] <- sigma^2 + crossprod(X[t,], P %*% X[t,])[1]
  }
  yvar
}

plot.weights <- function(agg)
{
  w.order <- order(apply(agg$weights,2,mean),decreasing = TRUE)
  K <- ncol(agg$experts)
  col <- rev(RColorBrewer::brewer.pal(n = max(min(K,11),4),name = "Spectral"))[1:min(K,11)]
  my.colors  <- col
  col <- numeric(K)
  if (K <= length(my.colors)) {
    col[w.order] <- my.colors[1:K]
  } else {
    col[w.order] <- c(my.colors, rep(my.colors[length(my.colors)],K-length(my.colors)))
  }
  
  matplot(agg$weights, type='l',col=col, lty=1,lwd=2)
  sel <- which(tail(agg$weights,1)>0)
  legend("top", col= col[sel], colnames(agg$experts)[sel],bty='n', lty=1, ncol=3, lwd=2)
  
}

