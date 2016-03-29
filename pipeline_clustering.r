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
load('dfposts.Rda') # 836119 posts, 47803 threads
df.posts <- data.frame(df.posts) %>% arrange(date)
df.posts <- df.posts[1:300000,]
df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"

# Print dates range
start.date <- as.POSIXct(min(as.numeric(df.posts$date)), origin = "1970-01-01") 
end.date <- as.POSIXct(max(as.numeric(df.posts$date)), origin = "1970-01-01")


# Plot general overview of forum
n <- hist(df.threads$length, breaks=0:max(df.threads$length))$counts
plot(1:max(df.threads$length), n, log='xy', 
     ylab='Number of threads', xlab='Length')
title(main='Threads length')

plot(cumsum(table(df.threads$length)), pch=19, cex=0.5,
     ylab='Length (cum)', xlab='Thread')
title('Threads length (cumulative)')

n <- hist(df.users$posts, breaks=0:max(df.users$posts))$counts
plot(1:max(df.users$posts), n, log='xy', 
     ylab='Number of users', xlab='Posts')
title(main='Users posts')

plot(cumsum(table(df.users$posts)), pch=19, cex=0.5,
     ylab='Posts (cum)', xlab='User')
title('Users posts (cumulative)')

# Compute neighborhood around every post
chunks <- split(df.threads$thread, ceiling(seq_along(df.threads$thread)/1000))
length(chunks)
##############################"
# sequential
#par(mfrow=c(1,1))
res.seq <- count_motifs_by_post(as.vector(unlist(chunks[1])), 
                                database='reddit',
                                neighbourhood='order')
threads <- chunks[[1]]
res.seq.dyn <- count_motifs_by_post(threads[1:50], 
                                    database='reddit',
                                    neighbourhood='time')

#res <- res.seq
#plot.motif.counts(res.seq)

# parallel
ncores <- detectCores() - 2
cl<-makeCluster(ncores, outfile="", port=11439)
registerDoParallel(cl)
pck <- c('RSQLite', 'data.table')
res.parallel <- foreach(i=1:length(chunks), .packages = pck)%dopar%{
  source('R/extract_from_db.r')
  count_motifs_by_post(chunks[[i]], 
                       database='reddit',
                       neighbourhood='order')
}
stopCluster(cl)
res <- merge.motif.counts(res.parallel)
save(res,file="res_3_4_dyn.Rda")

# Plot found motifs and their frequency
plot.motif.counts(res)
#dev.copy(png, paste0('2016-01-15-motifs_4_4_order.png'))
dev.copy(png, paste0('2016-01-15-motifs_4_dyn.png'))

dev.off()


#########################################



df.post.motif  <- res$posts.motifs
motifs <- res$motifs

# Sort by frequency (and relabel: 1 for the most frequent and so forth)
#idx <- order(table(df.post.motif$motif), decreasing = TRUE)
#df.post.motif$motif <- match(df.post.motif$motif, idx)
#motifs <- motifs[idx]

# Add motif info to posts dataframe
#df.posts <- merge(df.posts, df.post.motif, all.x=TRUE, sort=FALSE)
df.posts <- merge(df.posts, df.post.motif, all.x=FALSE, sort=FALSE)


# Check that df.posts gets the motifs relabeled
#head(arrange(df.post.motif, postid),10)
#head(arrange(df.posts, postid),10)


######################################################
# Count motifs in which each user appears
######################################################
user.motifs <- acast(df.posts, user~motif)
#user.motifs <- user.motifs[rownames(user.motifs) != 'root',] 

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
