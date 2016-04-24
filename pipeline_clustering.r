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
#load('dfposts.Rda') # 836119 posts, 47803 threads

#df.posts <- load_posts(database='reddit', forum='gameofthrones')
#save(df.posts,file="dfposts.Rda")
load('./R_objects/dfposts_podemos.Rda')
df.posts$date <- as.numeric(df.posts$date)
df.posts <- data.frame(df.posts) %>% arrange(date)
#df.posts <- df.posts[1:75000,] # Paper
df.posts <- df.posts[17100:17109,] # Debug

df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"

# Print dates range
start.date <- as.POSIXct(min(as.numeric(df.posts$date)), origin = "1970-01-01") 
end.date <- as.POSIXct(max(as.numeric(df.posts$date)), origin = "1970-01-01")
print(paste("Start date:", start.date))
print(paste("End date:", end.date))

cat('Number of threads: ', nrow(df.threads))
cat('Number of users: ', nrow(df.users))
cat('Number of active users', nrow(filter(df.users, posts>MIN_POSTS)))


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

#########################################################
# Compute neighborhood around every post
#########################################################
# Only long threads
#df.threads <- filter(df.threads, length>10)

chunks <- split(df.threads$thread, ceiling(seq_along(df.threads$thread)/70))
length(chunks)

chunks <- "t3_2bmb4v"
# Profiling
library(profvis)
prof <- profvis({
  res.seq <- count_motifs_by_post(as.vector(unlist(chunks))[1], 
                                  database='reddit',
                                  neighbourhood='struct')
})

# sequential
res.seq <- count_motifs_by_post(as.vector(unlist(chunks)), 
                                database='reddit',
                                neighbourhood='struct')

# parallel
ncores <- detectCores() - 5
cl<-makeCluster(ncores, outfile="", port=11439)
registerDoParallel(cl)
pck <- c('RSQLite', 'data.table', 'changepoint')
res.parallel <- foreach(i=1:length(chunks), .packages = pck)%dopar%{
  source('R/extract_from_db.r')
  #withTimeout(  count_motifs_by_post(chunks[[i]], 
  #                                   database='reddit',
  #                                   neighbourhood='time'),
  #                                  120, onTimeout='warning')
  count_motifs_by_post(chunks[[i]], 
                       database='reddit',
                       neighbourhood='struct', chunk.id=i)
}
stopCluster(cl)
res <- merge.motif.counts(res.parallel)
save(res, file='./R_objects/res_struct_35000_podemos.Rda') 
#save(res, file='./R_objects/res_order_2_4_75000_4chan.Rda') 

#save(res,file="res_time_75000_4chan.Rda")
#load("res_time_75000.Rda")

#save(res,file="res_order_2_4_75000_gameofthrones.Rda")
#load("res_order_2_4_75000_gameofthrones.Rda")

#load('res_2_4_order_75000.Rda') 

# Plot found motifs and their frequency
plot.motif.counts(res)

#dev.copy(png, paste0('2016-01-15-motifs_4_4_order.png'))
dev.copy(png, 'neighbourhoods_time.png')
dev.off()


#########################################
#df.posts.bck <- df.posts

df.posts <-df.posts.bck
df.post.motif  <- res$posts.motifs
motifs <- res$motifs

# Check they are sorted by frequency
n <- as.numeric(table(df.post.motif$motif))
all(n == cummin(n))

# Add motif info to posts dataframe
df.posts <- merge(df.posts, df.post.motif, all=FALSE, sort=FALSE)

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

###############################################
# Census
###############################################
par(mfrow=c(1,1))
n <- hist(df.posts$motif, breaks=0:max(df.posts$motif))$counts
plot(1:max(df.posts$motif), n, log='y', 
     ylab='Frequency', xlab='Neighbourhood',
     pch=19, cex=0.1)
title(main='Neighbourhoods frequency')

plot(cumsum(table(df.posts$motif)), pch=19, cex=0.5,
     ylab='Frequency (cum)', xlab='Neighbourhood')
title('Neighbourhoods frequency (cumulative)')

# How many neighbourhoods we need to account for 90% (for instance) of occurrences
names(table(df.posts$motif))[cumsum(table(df.posts$motif))<63000]

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
        legend.key = element_blank()) +
  ggtitle("Clusters means and variances")
print(p)