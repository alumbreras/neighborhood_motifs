posterior <- function(point=1, alpha=1, beta=1){
    m <- 27175
    n <- 1
    
    log(alpha+n) +
    (alpha+n)*log(beta + n*m) -
    (alpha+n+1)*log(beta + n*m + point)
}

x <- seq(1,10000, by=0.1)
y <- exp(posterior(point=x, alpha=1, beta=1))
plot(x, y, pch=19, cex=0.2, type='b')
title(paste('alpha:', alpha, 'beta:', beta))

posterior(point=1613, alpha=7000, beta=1)


alpha <- 10
beta <- 1
nsamples <- 100
x <- rep(NA,nsamples)
for(i in 1:nsamples){
  lam <- rgamma(1, shape=alpha, rate=beta)
  x[i] <- rexp(1, rate=lam)
}