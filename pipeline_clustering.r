# Extract list of triads where a user participates.
#
# author: Alberto Lumbreras
#
#https://www.reddit.com/r/europe/CasualConversation
#https://www.reddit.com/r/europe/
#https://www.reddit.com/r/datascience
#https://www.reddit.com/r/science/
#https://www.reddit.com/r/france
#https://www.reddit.com/r/catalunya
#https://www.reddit.com/r/es

library(ggplot2)
library(ggbiplot)
library(gplots)

library(dplyr)
library(reshape2)
library(igraph)
library(ggbiplot)
#library(RSQLite)
#library(GGally)

source('R/load_participations.r')
source('R/count_motifs.r')
source('R/normalize_counts.r')
source('R/clustering.r')

MIN_POSTS <- 30 # number of post to consider a user as active
###################################################
# Load data
###################################################
df.posts <- load_posts(database='reddit', forum='podemos')
df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"

# Compute neighborhood around every post
#res <-count_motifs_by_post(df.threads$thread, database='reddit')

df.post.motif  <- res$posts.motifs
motifs <- res$motifs

# Sort by frequency (and relabel: 1 for the most frequent and so forth)
idx <- order(table(df.post.motif$motif), decreasing = TRUE)
df.post.motif$motif <- match(df.post.motif$motif, idx)
motifs <- motifs[idx]

# Add motif info to posts dataframe
df.posts <- merge(df.posts, df.post.motif, all.x=TRUE, sort=FALSE)

# Check that df.posts gets the motifs relabeled
head(arrange(df.post.motif, postid),10)
head(arrange(df.posts, postid),10)

####################################################
# Plot found neighborhoods
####################################################
mypalette <- c("black", "red", "white")
par(mfrow=c(3,5))
for(i in 1:length(motifs)){
  gmotif <- as.undirected(motifs[[i]])
  la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
  plot(gmotif,
       layout = la,
       vertex.color=mypalette[V(motifs[[i]])$color],
       vertex.label = "",
       edge.arrow.size=0.6)
  title(paste(i),sub=sum(df.post.motif$motif==i))  
}

dev.copy(png, paste0('2016-01-15-motifs_4_4.png'))
dev.off()

######################################################
# Count motifs in which each user appears
######################################################
user.motifs <- acast(df.posts, user~motif)
user.motifs <- user.motifs[rownames(user.motifs) != 'root',] 

# Set up a user-features matrix (features are z-scores w.r.t a motif)
######################################################################
active.mask <- rowSums(user.motifs) > MIN_POSTS

features <- normalize_counts(user.motifs[active.mask,])

# after removing the non-active, some motifs dissapear.
idx.remove <- as.vector((which(is.na(colSums(features)))))
features <- features[,-idx.remove]

###############################################
# Clustering
###############################################
par(mfrow=c(1,1))
z <- cluster(features)

# Update users df with their cluster
df.users <-  data.frame(user=rownames(features), cluster=z) %>%
             merge(df.users, all=TRUE)
df.users[is.na(df.users$cluster),]$cluster <- 0 # non-active users go to cluster 0

# Assign colors to clusters by size so that we use the same colors all along
cluster.colors <- palette()[2:length(palette())] # 2: to avoid black

# PCA and whisker plots
#################################################### 
#TODO: features should contain also inactive users?
plot.clusters(features, 
              clusters = z,
              sizes    = 0.5*log(rowSums(user.motifs[active.mask,])), 
              colors   = alpha(cluster.colors[z], 0.5))
