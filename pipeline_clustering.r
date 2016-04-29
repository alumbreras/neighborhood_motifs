
library(ggplot2)
library(ggbiplot)
library(gplots)
library(dplyr)
library(reshape2)
library(igraph)
library(ggbiplot)

source('R/load_participations.r')
source('R/normalize_counts.r')
source('R/clustering.r')

MIN_POSTS <- 100 # number of post to consider a user as active
###################################################
# Load data
###################################################
load('./R_objects/dfposts_podemos.Rda')
df.posts <- df.posts
df.posts$date <- as.numeric(df.posts$date)
df.posts <- data.frame(df.posts) %>% arrange(date)
df.posts <- df.posts[1:75000,] # Paper
df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"
df.posts$forum <- 'podemos'

# Print dates range
start.date <- as.POSIXct(min(as.numeric(df.posts$date)), origin = "1970-01-01") 
end.date <- as.POSIXct(max(as.numeric(df.posts$date)), origin = "1970-01-01")
print(paste("Start date:", start.date))
print(paste("End date:", end.date))

cat('Number of threads: ', nrow(df.threads))
cat('Number of users: ', nrow(df.users))
cat('Number of active users', nrow(filter(df.users, posts>MIN_POSTS)))

#########################################
# Load neighbourhoods previously extracted
#########################################
load("./R_objects/res_time_75000_podemos.Rda")
df.post.motif  <- res$posts.motifs
motifs <- res$motifs

# Check they are sorted by frequency
n <- as.numeric(table(df.post.motif$motif))
all(n == cummin(n))

# Add motif info to posts dataframe
df.posts <- merge(df.posts, df.post.motif, all=FALSE, sort=FALSE)

# Re-index according to an external global dictionary
res <- merge.dictionaries(motifs.global, motifs)
df.posts$motif <- res$mapping[df.posts$motif]
motifs <- res$dict


# Because the counting is based on threads, there might be some posts that were 
# recovered in df.post.motif but that were cut in df.posts[1:N]
# so re-sort again motifs by frequency
# Sort by frequency (and relabel: 1 for the most frequent and so forth)
# make NA appear in table so that they are convetred to 0
idx <- order(tabulate(df.posts$motif), decreasing = TRUE)
df.posts$motif <- match(df.posts$motif, idx)
motifs <- motifs[idx]


# Check they are STILL sorted by frequency (they should be)
n <- as.numeric(table(df.posts$motif))
all(n == cummin(n))

######################################################
# Count motifs in which each user appears
######################################################
user.motifs <- acast(df.posts, user~motif)

# Set up a user-features matrix (features are z-scores w.r.t a motif)
######################################################################
active.mask <- rowSums(user.motifs) > MIN_POSTS
cat("Active users: ", sum(active.mask))

# normalize with respect to active users
features <- normalize_counts(user.motifs[active.mask,])

# or normalize with respect to all users
#features <- normalize_counts(user.motifs)
#features <- features[active.mask,]

# after removing the non-active, some motifs dissapear.
# remove also motifs that appeared less than 10 times
idx.motifs.none <- as.vector((which(is.na(colSums(features)))))
idx.motifs.low <- as.vector((which(colSums(user.motifs[active.mask,])<50)))
idx.motifs.delete <- c(idx.motifs.none, idx.motifs.low)

idx.motifs <- as.numeric(colnames(features))[-idx.motifs.delete]
features <- features[,-idx.motifs.delete]

###############################################
# Clustering
###############################################
par(mfrow=c(1,1))
z <- cluster(features, 3)

# Update users df with their cluster
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.users)[2] <- "posts"

df.users <-  data.frame(user=rownames(features), cluster=z) %>%
  merge(df.users, all=TRUE)
df.users[is.na(df.users$cluster),]$cluster <- 0 # non-active users go to cluster 0

# Assign colors to clusters by size so that we use the same colors all along
cluster.colors <- palette()[2:length(palette())] # 2: to avoid black

# PCA and whisker plots
#################################################### 
#TODO: features should contain also inactive users?
library(scales)
plot.clusters(features, 
              clusters = z,
              sizes    = 0.5*log(rowSums(user.motifs[active.mask,])), 
              colors   = alpha(cluster.colors[z], 0.5))


####################################################
# Which are the top neighbourhoods of each cluster?
####################################################

centers <- kmeans(features, 3)$centers
plot(centers[1,])
plot(centers[2,])
plot(centers[3,])
relevant.features <- c() 
mypalette <- c("black", "yellow", "orange", "red", "white")
for (k in 1:3){
  par(mfrow=c(1,1))
  fav.motifs <- order(abs(centers[k,]),  decreasing=TRUE)
  plot(centers[k,fav.motifs])
  cat('\nrelevants: ', fav.motifs)
  
  # The most prefered and the most repulsed
  limits <- quantile(centers[k,],c(0.05,0.95))
  upper.motifs <- sort(centers[k,][centers[k,]>limits[2]], decreasing=TRUE) %>% names %>% as.numeric
  lower.motifs <- sort(centers[k,][centers[k,]<limits[1]], decreasing=TRUE) %>% names %>% as.numeric
  
  relevant.features <- c(relevant.features, upper.motifs)
  relevant.features <- c(relevant.features, lower.motifs)
  cat('\nCluster: ', k)
  cat('\n Relevant features -- upper ', upper.motifs)
  cat('\n Relevant features -- lower ', lower.motifs)
  
  par(mfrow=c(3,5))
  for(j in 1:length(upper.motifs)){
    i <- upper.motifs[j]
    gmotif <- as.undirected(motifs[[i]])
    la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
    plot(gmotif,
         layout = la,
         vertex.color=mypalette[V(motifs[[i]])$color],
         vertex.label = "",
         edge.arrow.size=0.6)
    title(paste(i),sub=sum(df.post.motif$motif==i))  
  }
  
  par(mfrow=c(3,5))
  for(j in 1:length(lower.motifs)){
    i <- lower.motifs[j]
    gmotif <- as.undirected(motifs[[i]])
    la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
    plot(gmotif,
         layout = la,
         vertex.color=mypalette[V(motifs[[i]])$color],
         vertex.label = "",
         edge.arrow.size=0.6)
    title(paste(i),sub=sum(df.post.motif$motif==i)) 
  }
}


## Plot which are those favorite clusters

points <- as.data.frame(features[, relevant.features])
points$cluster <- factor(z)
points <- melt(points, id='cluster')
p <- ggplot(points, aes(x=variable, y=value)) + 
  scale_fill_manual(values = cluster.colors) + 
  geom_boxplot(aes(fill = cluster), position = position_dodge(width = 0.75))
p <- p + theme_bw() +
  theme(text = element_text(size = 15),
        legend.key = element_blank()) #+
  #ggtitle("Clusters means and variances")
print(p)