pred_regression <- function(df.thread_cluster){
  X <- t(as.matrix(df.thread_cluster[,1:6]))
  X <- rbind(rep(1, dim(X)[2]),X)
  y <- as.matrix(df.thread_cluster[,7])
  
  par(mfrow=c(1,1))
  alphaI <- diag(dim(X)[1])*0.01
  b_mle <- (solve(X%*%t(X)+alphaI)%*%X)%*%y
  preds <- t(X)%*%b_mle
  plot(preds, ylim=c(0,max(y)))
  lines(y, col='red')
  title(paste("Predictions with linear regression. SSE:", (1/length(y))*sqrt(sum((preds-y)^2))))
}