# Not use, but standard method
tsoutliers <- function(x)
{
  # Standard method based on IQR
  # http://stats.stackexchange.com/questions/1142/simple-algorithm-for-online-outlier-detection-of-a-generic-time-series
  x <- as.ts(x)
  tt <- 1:length(x)
  resid <- residuals(loess(x ~ tt))
  resid.q <- quantile(resid,prob=c(0.25,0.75))
  iqr <- diff(resid.q)
  limits <- resid.q + 1.5*iqr*c(-1,1)
  score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
  return(score)
}

outliers.iqr <- function(x){ 
  #deltas <- c(1,1,1,1,1,1,10,1,1,1,1,10,1,1,1)
  # ok
  
  #deltas <- c(27175,  1613,  2075,  9607,  1037,  497)
  # only detects the first one :(
  
  sequence <- deltas
  q.25.75 <- quantile(sequence, c(0.25, 0.75)) 
  iqr <- diff(q.25.75)
  limits <- q.25.75 + 1.5*iqr*c(-1,1)
  score.outlier.sup <- pmax((sequence - limits[2])/iqr,0)
  return(score.outlier.sup)
}

outliers.bertrand.seq <- function(x){
  # Outlier if it is 1.5 times higher than the maximum previous value 
  # in the window from the last outlier until now
  x <- c(1,1,1,1,1,1,10,1,1,1,1,10,1,1,1)
  x <- c(27175,  1613,  2075,  9607,  1037,  497)
  # does not detect the first and therefore sets the maximum too high :(
  
  outliers <- c()
  scores<- rep(NA, length(x))
  window.init <- 1
  pos <- 2
  while(pos<length(x)){
    sequence <- x[window.init:(pos-1)]
    score <- x[pos]/(1.5*max(sequence))
    scores[pos] <- score
    
    cat("\ninit position:", window.init)
    cat("\nlast position: ", pos-1)
    cat("\nposition:", pos)
    cat("\nin-window sequence: ", sequence)
    cat("\nnew sample:", x[pos])
    cat("\nmax delta:", 1.5*max(sequence))
    cat("\nscore: ", score)
    if(score>1){
      cat('\n*******outlier')
      outliers <- c(outliers, pos)
      window.init <- pos+1
      pos <- window.init + 1
    }
    else{
      pos <- pos+1
    }
    readline("next...")
    
  }
  par(mfrow=c(2,1))
  plot(x)
  plot(scores)
  return(scores)
}


outliers.bertrand <- function(x){
  # Outlier if it is 1.5 times higher than the maximum previous value 
  # in the window from the last outlier until now
  x <- c(1,1,1,1,1,1,10,1,1,1,1,10,1,1,1)
  
  x <- c(1,1,1,1,1,1,10,1,1,1,1,100,1,1,1)
  # :( 100 makes the 10 to look like normal 
  x <- c(27175,  1613,  2075,  9607,  1037,  497)
  
  outliers <- c()
  
  score <- x/max(x)
  scores[pos] <- score
  
  cat("\ninit position:", window.init)
  cat("\nlast position: ", pos-1)
  cat("\nposition:", pos)
  cat("\nin-window sequence: ", sequence)
  cat("\nnew sample:", x[pos])
  cat("\nmax delta:", 1.5*max(sequence))
  cat("\nscore: ", score)

  par(mfrow=c(2,1))
  plot(x)
  plot(scores)
  return(scores)
}



############################################
# Miercoles. Con bayes factor
#############################################
outlier.bayes <- function(deltas){
  
  deltas <- c(1,1,1,1,1,1,10,1,1,1,1,10,1,1,1)
  deltas <- c(27175,  1613,  2075,  9607,  1037,  497)
  deltas <- c(0,  0,  2075,  0,  0,  0)
  deltas <- c(26678,  1116,  1578,  9110,   540,     0)
  deltas <- c(0, 0.041832, 0.0591, 0.3414, 0.0202, 0)
  
  outliers <- c()
  bayes.factors <- rep(NA, length(deltas))
  alpha <- 1/mean(deltas)
  beta <- 1
  
  window.init <- 1
  pos <- 2
  while(pos<length(deltas)){
    sequence <- deltas[window.init:(pos-1)]
    delta.new <- deltas[pos]
    cat("\ninit position:", window.init)
    cat("\nlast position: ", pos-1)
    cat("\nposition:", pos)
    cat("\nin-window sequence: ", sequence)
    cat("\nnew sample:", delta.new)
    m <- mean(sequence)
    n <- length(sequence)
    p <- log(alpha+n) +
      (alpha+n)*log(beta + n*m) -
      (alpha+n+1)*log(beta + n*m + delta.new)
    p.prior <- log(alpha) +
      (alpha)*log(beta) -
      (alpha+1)*log(beta + delta.new)
    
    
    # if outlier, store it and re-start the sequence in the next point
    bayes.factors[pos] <- exp(p.prior-p)
    cat("\nprobability from prior: ", p.prior)
    cat("\nprobability from window: ", p)
    cat("\nbayes factor ", exp(p.prior-p))
    if(exp(p.prior-p)>2){
      cat("\noutlier, better explained by the base prior")
      outliers <- c(outliers, i)
      window.init <- pos+1
      pos <- window.init + 1
    }
    else{
      pos <- pos+1
    }
    #readline("next...")
  }
  par(mfrow=c(2,1))
  plot(deltas)
  plot(bayes.factors)
}

dates <- c(1408917425, 1408944600, 1408946213, 1408948288, 1408957895, 1408958932, 1408959429)
deltas <- diff(dates)

#deltas/median(deltas)

scores <- tsoutliers(dates)


outlier.bayes(deltas)


outliers.iqr
outliers.bertrand.seq 
outliers.bertrand
outlier.bayes 