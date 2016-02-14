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

library(reshape2)
library(igraph)

#library(RSQLite)
#library(dplyr)
#library(GGally)

source('R/load_participations.r')
source('R/count_motifs.r')
source('R/normalize_counts.r')
source('R/clustering.r')

###################################################
# Load data
###################################################
df.posts <- load_posts(database='reddit', forum='podemos')
df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"

######################################################
# Count motifs in which each user appears
######################################################
# deprecated
#res <- count_motifs(df.threads$thread, database='reddit')
#df.user.motifs <- as.data.frame(res$user.motifs)
#df.post.motif <- res$posts.motifs
#motifs <- res$motifs

# more elegant
res <-count_motifs_by_post(df.threads$thread, database='reddit')
df.post.motif  <- res$posts.motifs
df.posts <- merge(df.posts, df.post.motif, all.x=TRUE, sort=FALSE)
df.user.motifs <- acast(df.posts, user~motif)
df.user.motifs <- df.user.motifs[rownames(df.user.motifs)!='root',] # remove root
motifs <- res$motifs

# sort by frequency
idx <- order(colSums(df.user.motifs), decreasing = TRUE)
df.user.motifs <- df.user.motifs[,idx]
motifs <- motifs[idx]

# Put names to the features
#colnames(df.user.motifs) <- c("spot", "lonely answer",  "chain init answer 1", "chain init answer 2", 
#                              "common answer 1", "common answer 2", "common answer 3", "star init answer")

# Plot motifs with names
mypalette <- c("black", "red", "white")
par(mfrow=c(3,3))
for(i in 1:length(motifs)){
  gmotif <- as.undirected(motifs[[i]])
  la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
  plot(gmotif,
       layout = la,
       vertex.color=mypalette[V(motifs[[i]])$color],
       vertex.label = "",
       edge.arrow.size=0.6)
  title(paste(colnames(df.user.motifs)[i]),sub=colSums(df.user.motifs)[i])  
}

dev.copy(png, paste0('2016-01-15-motifs_4_4.png'))
dev.off()

# make names shorter
#colnames(df.user.motifs) <- c("spot", "LA", "ChIA1", "ChIA2", "CA1", "CA2", "CA3", "SIA")

# Merge similar motifs

# Set up a user-features matrix (features are z-scores w.r.t a motif)
######################################################################
#df.user.motifs <- as.data.frame(res$users.motifs) # res$users.motifs
active.mask <- rowSums(df.user.motifs)>10
df.features <- normalize_counts(df.user.motifs)
df.features <- df.features[active.mask,] #only active users

###############################################
# Clustering
###############################################
z <- cluster(df.features)

# Update users df with their cluster
tmp <- data.frame(user=rownames(df.features), cluster=z)
df.users <- merge(df.users, tmp, all.x=TRUE)
df.users[is.na(df.users$cluster),]$cluster <- 0

# Assign colors to clusters by size so that we use the same colors all along
cluster.colors <- palette()[2:length(palette())] # 2: to avoid black

# PCA and whisker plots
#################################################### 
plot.clusters(df.features, 
              clusters = z,
              sizes    = 0.5*log(rowSums(df.user.motifs[active.mask,])), 
              colors   = alpha(cluster.colors[z], 0.5))
