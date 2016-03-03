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
library(parallel)
library(doParallel)
library(foreach)
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

MIN_POSTS <- 100 # number of post to consider a user as active
###################################################
# Load data
###################################################
#df.posts <- load_posts(database='reddit', forum='podemos')
#save(df.posts,file="dfposts.Rda")
load('dfposts.Rda')
df.posts <- data.frame(df.posts)

df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"

# Compute neighborhood around every post
chunks <- split(df.threads$thread, ceiling(seq_along(df.threads$thread)/8000))

##############################"
# sequential
#par(mfrow=c(1,1))
#res.seq <- count_motifs_by_post(as.vector(unlist(chunks[1025])), database='reddit')
#res <- res.seq
#plot.motif.counts(res.seq)

# parallel
ncores <- detectCores() - 2
cl<-makeCluster(ncores, outfile="", port=11439)
registerDoParallel(cl)
pck <- c('RSQLite')
res.parallel <- foreach(i=1:4, .packages = pck)%dopar%{
          source('R/extract_from_db.r')
          count_motifs_by_post(chunks[[i]], database='reddit')
}
stopCluster(cl)
res.parallel <- merge.motif.counts(res.parallel)
res <- res.parallel
plot.motif.counts(res)
dev.copy(png, paste0('2016-01-15-motifs_4_4.png'))
dev.off()

#########################################




df.post.motif  <- res$posts.motifs
motifs <- res$motifs

# Sort by frequency (and relabel: 1 for the most frequent and so forth)
#idx <- order(table(df.post.motif$motif), decreasing = TRUE)
#df.post.motif$motif <- match(df.post.motif$motif, idx)
#motifs <- motifs[idx]

# Add motif info to posts dataframe
df.posts <- merge(df.posts, df.post.motif, all.x=TRUE, sort=FALSE)

# Check that df.posts gets the motifs relabeled
#head(arrange(df.post.motif, postid),10)
#head(arrange(df.posts, postid),10)


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
