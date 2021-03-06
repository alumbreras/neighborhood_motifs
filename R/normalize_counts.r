normalize_counts <- function(features){
  # Normalize the matrix with the user counts
  # Summary  matrix
  cat("\nMotifs counts:", colSums(features))
  
  # Normalize user activity (make the analysis independent of number of posts)
  features <- t(apply(features, 1, function(x) x/sum(x)))
  
  #as.data.frame(t(apply(user.motifs, 1, function(x) x/sum(x))))
  cat("\nMotifs % of active users:", colSums(features))
  
  # Center and scale data 
  # warning: it can create NA if feature count is 0
  features <- scale(features)
  cat("\nMotifs % of active users (z-score):", colSums(features))
  
  # Some NA because no active user has these neighborhoods. 
  #user.motifs[is.na(user.motifs)] <- 0
  #features[,is.na(colSums(features))] <- 0
  return(features)
}