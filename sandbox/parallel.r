# 

process_words <- function(words){
  sum(length(words))
  cbind(words)
}

words <- c("a", "b", "c", "d", "e", "f", "g", "h")


ncores <- detectCores() - 2
cl<-makeCluster(ncores, outfile="", port=11439)
registerDoParallel(cl)

res <- foreach(i=1:3)%dopar%{
  process_words(words[i:(i+2)])
}
stopCluster(cl)
