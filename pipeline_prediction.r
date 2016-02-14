library(dplyr)
source('R/plot_thread_cluster.r')

# Some plots and setting up the matrix to make predictions
#############################################################
# Plot individual relation between participation by cluster vs length
plot.whiskers_cluster_length(df.posts, df.users, df.threads)

# Plot cluster census by thread
plot.by_thread_cluster(df.posts, df.users)

# Create a dataset where features are the % of users within every role in the first posts
# of a thread, and output is the length of a thread
df.thread_cluster <- by_thread_cluster_nfirst(df.posts, nfirst=10) %>%
                     merge(df.users, by=c('user')) %>%
                     acast(thread~cluster) %>%
                     apply(1, function(x) x/sum(x)) %>%
                     t %>%
                     as.data.frame %>%
                     merge(df.threads, by.x="row.names", by.y="thread") %>% # add length
                     arrange(length) %>% # sort by length 
                     select(-Row.names) #drop thread names

# Plot it with length column normalized to stay in the same color range than the rest  
df.thread_cluster %>% 
  mutate(length=length/max(length)) %>%
  as.matrix %>%
  heatmap(Colv=NA, Rowv=NA, labRow=NA, ylab="Threads", xlab="Roles", 
          main = "Participations by role (10 first posts)")  
  
dev.copy(png, paste0('2016-01-15-thread_role_composition_nfirst.png'), width=600)
dev.off()


##########################################
# Length prediction
###########################################
source('R/pred_regression.r')
# pred_regression(df.thread_cluster)

# Binary lengths 
df.thread_cluster <- mutate(df.thread_cluster, length =factor(length > quantile(length,0.5)))


# SVM
######################
library('e1071')
if(FALSE){
  # Feature selection
  features <- 1:(ncol(df.thread_cluster)-1)
  idx <- sample(length(y)) 
  cut <- floor(length(idx)*0.7)
  idx.train <- idx[1:cut]
  idx.test <- idx[cut:length(idx)]
  y.train <- df.thread_cluster$length[idx.train]
  y.test <- df.thread_cluster$length[idx.test]
  X.train <- as.matrix(df.thread_cluster[idx.train,features])
  X.test <- as.matrix(df.thread_cluster[idx.test,features])
  
  bag <- vector()
  accuracy <- 0
  remaining.features <- features
  while(length(remaining.features)>0){
    cat("\n\n :::::::round:::::::")
    cat("\n", remaining.features)
    best.candidate.accuracy <- 0
    best.candidate <- remaining.features[i]
    for(i in 1:length(remaining.features)){
      cat('\n candidate', remaining.features[i])
      candidate <- remaining.features[i]
      current <- c(bag, candidate)
      preds <- predict(svm(X.train[,current], y.train))
      candidate.accuracy <- sum(y.train==preds)/length(y.train)
      cat("- acc:", candidate.accuracy)
      if (candidate.accuracy > best.candidate.accuracy){
        best.candidate.accuracy <- candidate.accuracy
        best.candidate <- candidate
        cat('**')
      }
    }
    bag <- c(bag, best.candidate)
    accuracy <- c(accuracy, best.candidate.accuracy)
    remaining.features <- remaining.features[remaining.features!=best.candidate]
    cat('\n selected features:', bag)
    cat('\n accuracy:', best.candidate.accuracy)
  }
  accuracy <- accuracy[-1]
  best <- which.max(accuracy)
  features <- bag[1:best]
  cat("Final selected features:", features)
  cat('Final accuracy:', max(accuracy))
  plot(accuracy, type='b', xaxt = "n")
  title("Forward feature selection")
  axis(1, at=1:length(features), labels=bag)
  
}

#######################
# Bootstrap for testing
#######################
accuracies <- vector()
features <- 1:(ncol(df.thread_cluster)-1)
for(i in 1:1000){
  # re-sample train/test
  idx <- sample(nrow(df.thread_cluster)) 
  cut <- floor(length(idx)*0.7)
  idx.train <- idx[1:cut]
  idx.test <- idx[cut:length(idx)]
  y.train <- df.thread_cluster$length[idx.train]
  y.test <- df.thread_cluster$length[idx.test]
  X.train <- as.matrix(df.thread_cluster[idx.train,features])
  X.test <- as.matrix(df.thread_cluster[idx.test,features])
  
  # Train model
  model <- svm(X.train, y.train, kernel='radial')
  
  # Accuracy in training
  preds <- predict(model)
  table(preds, y.train)
  cat("\nAccuracy:", sum(y.train==preds)/length(y.train))
  
  # Accuracy in test
  preds <- predict(model, X.test)
  table(preds, y.test)
  accuracies[i] <- sum(y.test==preds)/length(y.test)
  cat("\nAccuracy:", accuracies[i])
}
cat("Accuracy:", mean(accuracies), "sd:", sd(accuracies))

