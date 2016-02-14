normalize_counts <- function(df.user.motifs){
  # Normalize the matrix with the user counts
  # Summary  matrix
  df.user.motifs <- df.user.motifs[,colSums(df.user.motifs)>0]
  cat("\nMotifs counts:", colSums(df.user.motifs))
  
  df.user.motifs <- df.user.motifs[rowSums(df.user.motifs)>0,]
  # Normalize user activity (make the analysis independent of number of posts)
  df.user.motifs <- as.data.frame(t(apply(df.user.motifs, 1, function(x) x/sum(x))))
  cat("\nMotifs % of active users:", colSums(df.user.motifs))
  
  # Center and scale data
  df.user.motifs <- as.data.frame(scale(df.user.motifs))
  cat("\nMotifs % of active users (z-score):", colSums(df.user.motifs))
  
  return(df.user.motifs)
}