# Compare the nieghbourhood census of two or more forums
# author: Alberto Lumbreras

library(dplyr)
library(reshape2)
library(igraph)
source('R/merge_indices.r')
if(FALSE){
  df.posts <- load_posts(database='reddit', forum='podemos')
  save(df.posts,file="dfposts_podemos.Rda")
  
  df.posts <- load_posts(database='reddit', forum='gameofthrones')
  save(df.posts,file="dfposts_gameofthrones.Rda")
  
  df.posts <- load_posts(database='reddit', forum='sex')
  save(df.posts,file="dfposts_sex.Rda")
  
  df.posts <- load_posts(database='reddit', forum='4chan')
  save(df.posts,file="dfposts_4chan.Rda")
  
  df.posts <- load_posts(database='reddit', forum='complexsystems')
  save(df.posts,file="dfposts_complexsystems.Rda")
  
  df.posts <- load_posts(database='reddit', forum='datascience')
  save(df.posts,file="dfposts_datascience.Rda")
}

########################################################
# Load posts from each forum
########################################################
load('./R_objects/dfposts_gameofthrones.Rda')
df.posts.got <- df.posts
df.posts.got$date <- as.numeric(df.posts.got$date)
df.posts.got <- data.frame(df.posts.got) %>% arrange(date)
df.posts.got <- df.posts.got[1:75000,] # Paper
df.threads.got <- plyr::count(df.posts.got, "thread")
df.users.got <- plyr::count(df.posts.got, 'user')                                                                                                                                   
names(df.threads.got)[2] <- "length"
names(df.users.got)[2] <- "posts"
df.posts.got$forum <- 'gameofthrones'

load('./R_objects/dfposts_podemos.Rda')
df.posts.pod <- df.posts
df.posts.pod$date <- as.numeric(df.posts.pod$date)
df.posts.pod <- data.frame(df.posts.pod) %>% arrange(date)
df.posts.pod <- df.posts.pod[1:75000,] # Paper
df.threads.pod <- plyr::count(df.posts.pod, "thread")
df.users.pod <- plyr::count(df.posts.pod, 'user')                                                                                                                                   
names(df.threads.pod)[2] <- "length"
names(df.users.pod)[2] <- "posts"
df.posts.pod$forum <- 'podemos'

# load('./R_objects/dfposts_4chan.Rda')
# df.posts.4ch <- df.posts
# df.posts.4ch$date <- as.numeric(df.posts.4ch$date)
# df.posts.4ch <- data.frame(df.posts.4ch) %>% arrange(date)
# df.posts.4ch <- df.posts.4ch[1:75000,] # Paper
# df.threads.4ch <- plyr::count(df.posts.4ch, "thread")
# df.users.4ch <- plyr::count(df.posts.4ch, 'user')                                                                                                                                   
# names(df.threads.4ch)[2] <- "length"
# names(df.users.4ch)[2] <- "posts"
# df.posts.4ch$forum <- '4chan'

########################################################
# Load neighbourhoods
########################################################
load("./R_objects/res_struct_75000_gameofthrones.Rda")
res.got.struct <- res

# Add motif info to posts dataframe and sort by frequency
#df.post.motif  <- res.got.order$posts.motifs
#motifs <- res$motifs
df.posts.got <- merge(df.posts.got, res.got.struct$posts.motifs, all=FALSE, sort=FALSE)
idx <- order(tabulate(df.posts.got$motif), decreasing = TRUE) # get order by frequency
df.posts.got$motif <- match(df.posts.got$motif, idx) # re-arrange pointers to motifs
motifs.got.struct <- res.got.struct$motifs[idx] # re-sort motifs
colnames(df.posts.got)[which(names(df.posts.got) == "motif")] <- "motif.struct"

# Check they are sorted by frequency (they should be)
n <- as.numeric(table(df.posts.got$motif.struct))
all(n == cummin(n))

#################################
load("./R_objects/res_order_75000_gameofthrones.Rda")

res.got.order <- res

# Add motif info to posts dataframe and sort by frequency
df.posts.got <- merge(df.posts.got, res.got.order$posts.motifs, all=FALSE, sort=FALSE)
idx <- order(tabulate(df.posts.got$motif), decreasing = TRUE) # get order by frequency
df.posts.got$motif <- match(df.posts.got$motif, idx) # re-arrange pointers to motifs
motifs.got.order <- res.got.order$motifs[idx] # re-sort motifs
colnames(df.posts.got)[which(names(df.posts.got) == "motif")] <- "motif.order"

# Check the motifs ids are sorted by frequency (they should be)
n <- as.numeric(table(df.posts.got$motif.order))
all(n == cummin(n))

load("./R_objects/res_time_75000_gameofthrones.Rda")
res.got.time <- res

# Add motif info to posts dataframe and sort by frequency
df.posts.got <- merge(df.posts.got, res.got.time$posts.motifs, all=FALSE, sort=FALSE)
idx <- order(tabulate(df.posts.got$motif), decreasing = TRUE) # get order by frequency
df.posts.got$motif <- match(df.posts.got$motif, idx) # re-arrange pointers to motifs
motifs.got.time <- res.got.time$motifs[idx] # re-sort motifs
colnames(df.posts.got)[which(names(df.posts.got) == "motif")] <- "motif.time"

# Check they are sorted by frequency (they should be)
n <- as.numeric(table(df.posts.got$motif.time))
all(n == cummin(n))

#####################
load("./R_objects/res_struct_75000_podemos.Rda")
res.pod.struct <- res

# Add motif info to posts dataframe and sort by frequency
df.posts.pod <- merge(df.posts.pod, res.pod.struct$posts.motifs, all=FALSE, sort=FALSE)
idx <- order(tabulate(df.posts.pod$motif), decreasing = TRUE) # get order by frequency
df.posts.pod$motif <- match(df.posts.pod$motif, idx) # re-arrange pointers to motifs
motifs.pod.struct <- res.pod.struct$motifs[idx] # re-sort motifs
colnames(df.posts.pod)[which(names(df.posts.pod) == "motif")] <- "motif.struct"

# Check they are sorted by frequency (they should be)
n <- as.numeric(table(df.posts.pod$motif.struct))
all(n == cummin(n))

load("./R_objects/res_order_75000_podemos.Rda")
res.pod.order <- res

# Add motif info to posts dataframe and sort by frequency
df.posts.pod <- merge(df.posts.pod, res.pod.order$posts.motifs, all=FALSE, sort=FALSE)
idx <- order(tabulate(df.posts.pod$motif), decreasing = TRUE) # get order by frequency
df.posts.pod$motif <- match(df.posts.pod$motif, idx) # re-arrange pointers to motifs
motifs.pod.order <- res.pod.order$motifs[idx] # re-sort motifs
colnames(df.posts.pod)[which(names(df.posts.pod) == "motif")] <- "motif.order"

# Check the motifs ids are sorted by frequency (they should be)
n <- as.numeric(table(df.posts.pod$motif.order))
all(n == cummin(n))

load("./R_objects/res_time_75000_podemos.Rda")
res.pod.time <- res

# Add motif info to posts dataframe and sort by frequency
df.posts.pod <- merge(df.posts.pod, res.pod.time$posts.motifs, all=FALSE, sort=FALSE)
idx <- order(tabulate(df.posts.pod$motif), decreasing = TRUE) # get order by frequency
df.posts.pod$motif <- match(df.posts.pod$motif, idx) # re-arrange pointers to motifs
motifs.pod.time <- res.pod.time$motifs[idx] # re-sort motifs
colnames(df.posts.pod)[which(names(df.posts.pod) == "motif")] <- "motif.time"

# Check they are sorted by frequency (they should be)
n <- as.numeric(table(df.posts.pod$motif.time))
all(n == cummin(n))

##############
# load("./R_objects/res_struct_75000_4chan.Rda")
# res.4ch.struct <- res
# 
# # Add motif info to posts dataframe and sort by frequency
# #df.post.motif  <- res.4ch.order$posts.motifs
# #motifs <- res$motifs
# df.posts.4ch <- merge(df.posts.4ch, res.4ch.struct$posts.motifs, all=FALSE, sort=FALSE)
# idx <- order(tabulate(df.posts.4ch$motif), decreasing = TRUE) # get order by frequency
# df.posts.4ch$motif <- match(df.posts.4ch$motif, idx) # re-arrange pointers to motifs
# motifs.4ch.struct <- res.4ch.struct$motifs[idx] # re-sort motifs
# colnames(df.posts.4ch)[which(names(df.posts.4ch) == "motif")] <- "motif.struct"
# 
# # Check they are sorted by frequency (they should be)
# n <- as.numeric(table(df.posts.4ch$motif.struct))
# all(n == cummin(n))
# 
# #################################
# load("./R_objects/res_order_75000_4chan.Rda")
# 
# res.4ch.order <- res
# 
# # Add motif info to posts dataframe and sort by frequency
# df.posts.4ch <- merge(df.posts.4ch, res.4ch.order$posts.motifs, all=FALSE, sort=FALSE)
# idx <- order(tabulate(df.posts.4ch$motif), decreasing = TRUE) # get order by frequency
# df.posts.4ch$motif <- match(df.posts.4ch$motif, idx) # re-arrange pointers to motifs
# motifs.4ch.order <- res.4ch.order$motifs[idx] # re-sort motifs
# colnames(df.posts.4ch)[which(names(df.posts.4ch) == "motif")] <- "motif.order"
# 
# # Check the motifs ids are sorted by frequency (they should be)
# n <- as.numeric(table(df.posts.4ch$motif.order))
# all(n == cummin(n))
# 
# #################################
# load("./R_objects/res_time_75000_4chan.Rda")
# res.4ch.time <- res
# 
# # Add motif info to posts dataframe and sort by frequency
# df.posts.4ch <- merge(df.posts.4ch, res.4ch.time$posts.motifs, all=FALSE, sort=FALSE)
# idx <- order(tabulate(df.posts.4ch$motif), decreasing = TRUE) # get order by frequency
# df.posts.4ch$motif <- match(df.posts.4ch$motif, idx) # re-arrange pointers to motifs
# motifs.4ch.time <- res.4ch.time$motifs[idx] # re-sort motifs
# colnames(df.posts.4ch)[which(names(df.posts.4ch) == "motif")] <- "motif.time"
# 
# # Check they are sorted by frequency (they should be)
# n <- as.numeric(table(df.posts.4ch$motif.time))
# all(n == cummin(n))

# A single dataset for all
df.posts <- rbind(df.posts.got, df.posts.pod)
df.posts <- select(df.posts, -parent, -user, -date, -thread)

################################################################################################
# Plot census distribution
################################################################################################
par(mfrow=c(1,1))
n0 <- hist(df.posts.got$motif.struct, breaks=0:max(df.posts.got$motif.struct), plot=FALSE)$counts
n1 <- hist(df.posts.got$motif.order, breaks=0:max(df.posts.got$motif.order), plot=FALSE)$counts
n2 <- hist(df.posts.got$motif.time, breaks=0:max(df.posts.got$motif.time), plot=FALSE)$counts

plot(1:max(df.posts.got$motif.order), n1, 
       pch=19, cex=0.1, col='red',
       ylim=c(1, 20000), xlim=c(0, max(df.posts.got$motif.struct)), log='y',
       ylab='Frequency', xlab='Neighbourhood')

points(1:max(df.posts.got$motif.time), n2,
       pch=19, cex=0.1, col='black')

points(1:max(df.posts.got$motif.struct), n0,
       pch=19, cex=0.1, col='blue')

#legend('topright', c('time-based', 'order-based'), col=1:2, pch=19)
legend(2005,15, c('time-based', 'order-based', 'struct-based'), col=c('red', 'black', 'blue'), pch=19)
title(main='Census distribution (Game of Thrones)')

#In-picture plot
par(new=TRUE)
par(oma=c(0,3,5,3))
par(mfcol=c(2,2), mfg=c(1,2))
par(mar=c(4,4,1,1))

n.max <- 100
plot(1:n.max, n1[1:n.max], ylim=c(1, 20000), log='y',
     ylab=NA, xlab=NA,
     pch=19, cex=0.1, col='red')

points(1:n.max, n2[1:n.max], 
       pch=19, cex=0.1, col='black')

points(1:n.max, n0[1:n.max], 
       pch=19, cex=0.1, col='blue')

# Cumulative
par(mfrow=c(1,1))
plot(cumsum(n2), pch=19, cex=0.5, ylab='Frequency (cum)', xlab='Neighbourhoods', xlim=c(0,2800))
points(1:max(df.posts.got$motif.order), cumsum(n1), pch=19, cex=0.1, col='red')
points(1:max(df.posts.got$motif.struct), cumsum(n0), pch=19, cex=0.1, col='blue')
#legend('topright', c('time-based', 'order-based'), col=1:2, pch=19)
legend(2305,60000, c('time-based', 'order-based', 'struct-based'), col=c('red', 'black', 'blue'), pch=19)
title('Cumulative census distribution (Game of Thrones)')

#In-picture plot
par(new=TRUE)
par(oma=c(5,3,0,3))
par(mfcol=c(2,2), mfg=c(2,2))
par(mar=c(4,4,1,1))

n.max <- 100
plot(cumsum(n1[1:n.max]),  ylim=c(1, 70000), pch=19, cex=0.5, ylab=NA, xlab=NA, col='red')
points(1:n.max, cumsum(n2[1:n.max]), pch=19, cex=0.1, col='black')
points(1:n.max, cumsum(n0[1:n.max]), pch=19, cex=0.1, col='blue')

#########################################################
# How many neighbourhoods explain the 90% of the census?
#########################################################
total <- sum(n0)
th <-total*0.95
sum(cumsum(n0) < th)

total <- sum(n1)
th <-total*0.95
sum(cumsum(n1) < th)

total <- sum(n2)
th <-total*0.95
sum(cumsum(n2) < th)

#######################################################################
# Create a common neighbourhood dictionary index
#######################################################################
df.posts.global <- df.posts
motifs.global <- motifs.got.time

# Game of Thrones (17h57)
mask <- df.posts.global$forum == 'gameofthrones'
res <- merge.dictionaries(motifs.global, motifs.got.order)
df.posts.global[mask,]$motif.order <- res$mapping[df.posts.global[mask,]$motif.order]
motifs.global <- res$dict

# 10 mins
res <- merge.dictionaries(motifs.global, motifs.got.struct)
df.posts.global[mask,]$motif.struct <- res$mapping[df.posts.global[mask,]$motif.struct]
motifs.global <- res$dict

# Podemos
mask <- df.posts.global$forum == 'podemos'
res <- merge.dictionaries(motifs.global, motifs.pod.time)
df.posts.global[mask,]$motif.time <- res$mapping[df.posts.global[mask,]$motif.time]
motifs.global <- res$dict

res <- merge.dictionaries(motifs.global, motifs.pod.order)
df.posts.global[mask,]$motif.order <- res$mapping[df.posts.global[mask,]$motif.order]
motifs.global <- res$dict

res <- merge.dictionaries(motifs.global, motifs.pod.struct)
df.posts.global[mask,]$motif.struct <- res$mapping[df.posts.global[mask,]$motif.struct]
motifs.global <- res$dict

#save(df.posts.global, file="df.posts.global.Rda")
#save(motifs.global, file="motifs.global.Rda")

load("df.posts.global.Rda")
load("motifs.global.Rda")


# 4chan
#mask <- df.posts.global$forum == '4chan'

#res <- merge.dictionaries(motifs.global, motifs.4ch.time)
#df.posts.global[mask,]$motif.time <- res$mapping[df.posts.global[mask,]$motif.time]
#motifs.global <- res$dict

#res <- merge.dictionaries(motifs.global, motifs.4ch.order)
#df.posts.global[mask,]$motif.order <- res$mapping[df.posts.global[mask,]$motif.order]
#motifs.global <- res$dict

#res <- merge.dictionaries(motifs.global, motifs.4ch.struct)
#df.posts.global[mask,]$motif.struct <- res$mapping[df.posts.global[mask,]$motif.struct]
#motifs.global <- res$dict

# Now we have df.posts.motif.global that indicate the motif (neighbourhood) of each post
# and motifs.global where we store the motifs

# Re-index by total frequency
together <- c(df.posts.global$motif.order, df.posts.global$motif.time, df.posts.global$motif.struct)
idx <- order(tabulate(together), decreasing = TRUE) # get order by frequency
df.posts.global$motif.order <- match(df.posts.global$motif.order, idx) # re-arrange pointers to motifs
df.posts.global$motif.time <- match(df.posts.global$motif.time, idx) # re-arrange pointers to motifs
df.posts.global$motif.struct <- match(df.posts.global$motif.struct, idx) # re-arrange pointers to motifs
motifs.global <- motifs.global[idx] # re-sort motifs

#######################################################
# Confusion between neighbourhood types 
#######################################################
par(mfrow=c(1,1))

## Scatter plots
# confusion time-order (not confusion matrix because it does not sum)
plot(df.posts.global$motif.time, df.posts.global$motif.order, cex=0.1, pch=19,
     xlab='time-based neighbourhood', ylab='order-based neighbourhood', xlim=c(0,500), ylim=c(0,500))

# confusion time-structure
plot(df.posts.global$motif.struct, df.posts.global$motif.time, cex=0.1, pch=19,
     xlab='struct-based neighbourhood', ylab='time-based neighbourhood', xlim=c(0,500), ylim=c(0,500))

# and closer
plot(df.posts.global$motif.time, df.posts.global$motif.order, cex=0.1, pch=19,
     xlab='time-based neighbourhood', ylab='order-based neighbourhood', xlim=c(0,100), ylim=c(0,100))

# closer confusion time-structure
plot(df.posts.global$motif.struct, df.posts.global$motif.time, cex=0.1, pch=19,
     xlab='struct-based neighbourhood', ylab='time-based neighbourhood', xlim=c(0,100), ylim=c(0,100))

# 

# Looking only at most frequents in time
# idx <- order(tabulate(df.posts.global$motif.time), decreasing = TRUE) # get order by frequency
# df.posts.filtered <- filter(df.posts.global, motif.time %in% idx[1:100])

# Looking only at most frequents in order
# idx <- order(tabulate(df.posts.global$motif.order), decreasing = TRUE) # get order by frequency
# df.posts.filtered <- filter(df.posts.global, motif.order %in% idx[1:100])

# par(mfrow=c(1,1))
# Plot confusion matrix (not confusion matrix because it does not sum)
# plot(df.posts.filtered$motif.time, df.posts.filtered$motif.order, cex=0.1, pch=19,
#     xlab='time-based neighbourhood', ylab='order-based neighbourhood', xlim=c(0,200), ylim=c(0,200))
# and closer
# plot(df.posts.filtered$motif.time, df.posts.filtered$motif.order, cex=0.1, pch=19,
#     xlab='time-based neighbourhood', ylab='order-based neighbourhood', xlim=c(0,100), ylim=c(0,100))


# Confusion matrix
# slower
#df.test <- head(df.posts.global,100)
#df.test <- df.posts.global
#m1 <- acast(df.test, motif.order~postid, fun.aggregate = length)
#m2 <- acast(df.test, motif.time~postid, fun.aggregate = length)
#confusion <- m1%*%t(m2) 
confusion.order_time <- acast(df.posts.global, motif.order~motif.time, fun.aggregate = length) # faster?
confusion.time_struct <- acast(df.posts.global, motif.time~motif.struct, fun.aggregate = length) # faster?
#save(confusion.order_time, file="confusion.order_time.Rda")
#save(confusion.time_struct, file="confusion.time_struct.Rda")
load("confusion.order_time.Rda")
load("confusion.time_struct.Rda")

# % of agreement
df.agree <- filter(df.posts.global, motif.order==motif.time, motif.time==motif.struct)
nrow(df.agree)/nrow(df.posts.global)
# 30.75%

df.agree <- filter(df.posts.global, motif.order==motif.time)
nrow(df.agree)/nrow(df.posts.global)
# 35.93

df.agree <- filter(df.posts.global, motif.time==motif.struct)
nrow(df.agree)/nrow(df.posts.global)
# 40%

df.agree <- filter(df.posts.global, motif.order==motif.struct)
nrow(df.agree)/nrow(df.posts.global)
# 41.48%

# Which order-based neighgbourhoods have a bigger diversity of time-based ounterparts?
# use the confusion matrices, it will be easier
n.sources <- apply(confusion.order_time, 1, function(x) length(unique(x)))
sort(n.sources[1:100], decreasing = TRUE)

n.sources <- apply(confusion.time_struct, 1, function(x) length(unique(x)))
sort(n.sources[1:100], decreasing = TRUE)
#######################################################################
# Census of the 3 types of neighbourhood in the same forum
#######################################################################
# Get the first 25 motifs of struct, order and time based
# Plot the census population in this base 
# if a motif has not isomorphisms in another XX-based, the coount in XX-base in 0

df.posts <- df.posts.global[df.posts.global$forum=='podemos',]
df.posts <- filter(df.posts.global, forum=='gameofthrones')
#df.posts <- filter(df.posts.global, forum=='podemos')

ntop <- 50
top.struct <- order(tabulate(df.posts$motif.struct), decreasing = TRUE)[1:ntop]
top.order <- order(tabulate(df.posts$motif.order), decreasing = TRUE)[1:ntop]
top.time <- order(tabulate(df.posts$motif.time), decreasing = TRUE)[1:ntop]
top <- union(union(top.order, top.time), top.struct) %>% sort
#top <- 1:max(top) # fill the gaps

counts.struct <- df.posts$motif.struct[df.posts$motif.struct %in% top]
counts.order <- df.posts$motif.order[df.posts$motif.order %in% top]
counts.time <- df.posts$motif.time[df.posts$motif.time %in% top]
census.struct <- sapply(top, function(x) sum(counts.struct==x))
census.order <- sapply(top, function(x) sum(counts.order==x))
census.time <- sapply(top, function(x) sum(counts.time==x))

par(mfrow=c(1,1))
plot(1:length(top), census.order, pch=19, col='red',
     type='l',xlab='Neighborhood', ylab='Frequency', axes=FALSE)
lines(1:length(top), census.time, pch=19, col='black')
lines(1:length(top), census.struct, pch=19, col='blue')
axis(1, at=1:length(top), labels=top, cex=0.2) 
axis(2) 
legend(length(top)-15,12000, c('order-based', 'time_based', 'struct-based'), col=c('red', 'black', 'blue'), pch=c(18,20))
title('Neighbourhood census in Game of Thrones')


plot.trees(motifs.global[top], top)

# maybe its clearer with comparing each type of neighbourhood in two forums
# and analyizing which one is more sensitive to changes
##########################

# struct
##########
motifs.podemos <- filter(df.posts.global, forum=='podemos')$motif.struct
motifs.gameofthrones <- filter(df.posts.global, forum=='gameofthrones')$motif.struct 
motifs <- sort(union(motifs.podemos, motifs.gameofthrones))[1:50]

census.struct.podemos <- sapply(motifs, function(x) sum(motifs.podemos==x))
census.struct.gameofthrones <- sapply(motifs, function(x) sum(motifs.gameofthrones==x))

plot(1:length(motifs), census.struct.podemos, col='blue', 
     type = 'l', axes=FALSE, xlab='Neighborhood', ylab='Frequency')
lines(1:length(motifs), census.struct.gameofthrones, col='red')
axis(1, at=1:length(motifs), labels = motifs)
axis(2)
legend(length(motifs)-15,6000, c('podemos', 'gameofthrones'), col=c('blue', 'red'), pch=c(18,20))
title('Structure-based census')

# order
##########
motifs.podemos <- filter(df.posts.global, forum=='podemos')$motif.order
motifs.gameofthrones <- filter(df.posts.global, forum=='gameofthrones')$motif.order 
motifs <- sort(union(motifs.podemos, motifs.gameofthrones))[1:50]

census.struct.podemos <- sapply(motifs, function(x) sum(motifs.podemos==x))
census.struct.gameofthrones <- sapply(motifs, function(x) sum(motifs.gameofthrones==x))

plot(1:length(motifs), census.struct.podemos, col='blue', 
     type = 'l', axes=FALSE, xlab='Neighborhood', ylab='Frequency')
lines(1:length(motifs), census.struct.gameofthrones, col='red')
axis(1, at=1:length(motifs), labels = motifs)
axis(2)
legend(length(motifs)-15,6000, c('podemos', 'gameofthrones'), col=c('blue', 'red'), pch=c(18,20))
title('Order-based census')

# time
##########
motifs.podemos <- filter(df.posts.global, forum=='podemos')$motif.time
motifs.gameofthrones <- filter(df.posts.global, forum=='gameofthrones')$motif.time 
motifs <- sort(union(motifs.podemos, motifs.gameofthrones))[1:50]

census.struct.podemos <- sapply(motifs, function(x) sum(motifs.podemos==x))
census.struct.gameofthrones <- sapply(motifs, function(x) sum(motifs.gameofthrones==x))

plot(1:length(motifs), census.struct.podemos, col='blue', 
     type = 'l', axes=FALSE, xlab='Neighborhood', ylab='Frequency')
lines(1:length(motifs), census.struct.gameofthrones, col='red')
axis(1, at=1:length(motifs), labels = motifs)
axis(2)
legend(length(motifs)-15,6000, c('podemos', 'gameofthrones'), col=c('blue', 'red'), pch=c(18,20))
title('Time-based census')
############################################################################
############################################################################
############################################################################
#############################################################
# Get first motifs of first dataset as reference and plot 
# and compare occurences between the two à la Adamic 
# Podemos vs Game of Thrones
############################################################
library(igraph)

load('./R_objects/dfposts_podemos.Rda')
df.posts$date <- as.numeric(df.posts$date)
df.posts <- data.frame(df.posts) %>% arrange(date)
df.posts <- df.posts[1:75000,] # Paper

df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"

#load("./R_objects/res_time_75000_podemos.Rda")
load("./R_objects/res_order_2_4_75000_podemos.Rda")

# Add motif info to posts dataframe and sort by frequency
df.post.motif  <- res$posts.motifs
motifs <- res$motifs
df.posts <- merge(df.posts, df.post.motif, all=FALSE, sort=FALSE)
idx <- order(tabulate(df.posts$motif), decreasing = TRUE)
df.posts$motif <- match(df.posts$motif, idx)
motifs <- motifs[idx]

# unique name
df.posts.1 <- df.posts
motifs.1 <- motifs

#################
load('./R_objects/dfposts_gameofthrones.Rda')
df.posts$date <- as.numeric(df.posts$date)
df.posts <- data.frame(df.posts) %>% arrange(date)
df.posts <- df.posts[1:75000,] # Paper

df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"

#load("./R_objects/res_time_75000_gameofthrones.Rda")
load("./R_objects/res_order_2_4_75000_gameofthrones.Rda")

# Add motif info to posts dataframe and sort by frequency
df.post.motif  <- res$posts.motifs
motifs <- res$motifs
df.posts <- merge(df.posts, df.post.motif, all=FALSE, sort=FALSE)
idx <- order(tabulate(df.posts$motif), decreasing = TRUE)
df.posts$motif <- match(df.posts$motif, idx)
motifs <- motifs[idx]

# unique name
df.posts.2 <- df.posts
motifs.2 <- motifs

counters.1 <- rep(0,50)
counters.2 <- rep(0,50)
for(i in 1:50){
  cat('\n ', i)
  counters.1[i] <- sum(df.posts.1$motif==i)
  for(j in 1:length(motifs.2)){
    
    # the vf2 does not like graphs of different size
    if(vcount(motifs.1[[i]]) != vcount(motifs.2[[j]])){      
      next
    }          
    
    if(is_isomorphic_to(motifs.1[[i]], motifs.2[[j]], method='vf2')){        
      counters.2[i] <- sum(df.posts.2$motif==j)  
      break
    }  
  }
}

par(mfrow=c(1,1))
plot(counters.1, pch=18, type='o', xlab='Neighborhood', ylab='Frequency', col='black')
lines(counters.2, pch=20, type='o', col='red')
#par(mar=c(0, 0, 0, 0))
altura <- 10
legend(40,20300, c('Podemos', 'Game of Thrones'), col=1:2, pch=c(18,20))
#title('Neighbourhood census (time-based)')
title('Neighbourhood census (order-based)')

###################################################################
# Size vs coverage
###################################################################
sizes.1 <- rep(0,length(motifs.1))
freqs.1 <- rep(0,length(motifs.1))
for(i in 1:length(motifs.1)){
  sizes.1[i] <- vcount(motifs.1[[i]])
  freqs.1[i] <- sum(df.posts.1$motif==i)  
}
sizes.2 <- rep(0,length(motifs.2))
freqs.2 <- rep(0,length(motifs.2))
for(i in 1:length(motifs.2)){
  sizes.2[i] <- vcount(motifs.2[[i]])
  freqs.2[i] <- sum(df.posts.2$motif==i)  
}

plot(sizes.1-0.1, freqs.1, pch=20, cex=0.7, xlab="size", ylab='frequency')
points(sizes.2+0.1, freqs.2,  col='red', pch=18, cex=0.7)
legend(24,3500, c('Time-based', 'Order-based'), col=1:2, pch=c(20,18))

df.1 <- data.frame(size=sizes.1, freq=freqs.1)
df.2 <- data.frame(size=sizes.2, freq=freqs.2)

coverages.1 <- rep(0,100)
for (i in 1:100){
  coverages.1[i] <- sum(df.1[df.1$size==i,]$freq)
}

coverages.2 <- rep(0,100)
for (i in 1:100){
  coverages.2[i] <- sum(df.2[df.2$size==i,]$freq)
}

plot(coverages.1, type='b', xlim=c(0,11), ylim=c(0,max(coverages.2)), xlab='neighbourhood size', ylab='frequency')
lines(coverages.2, type='b', col='red')
title('Neighbourhoods size distribution')

#################################################################################
# What are the equivalents between structural, order and time-based neighbourhood
# (or posts with time-based neighbourhood i what is its order-based neighbourhood?)
#################################################################################

# Load a dataframe of posts, motifs.struct, motifs.order, motifs.time
load("./R_objects/res_order_2_4_75000_gameofthrones.Rda")
df.posts.motif.order  <- res$posts.motifs
motifs.order <- res$motifs

load("./R_objects/res_time_75000_gameofthrones.Rda")
df.posts.motif.time <- res$posts.motifs
motifs.time <- res$motifs

load("./R_objects/res_struct_75000_gameofthrones.Rda")

# Create a common list of motifs for the three kind of neighbourhoods
motifs.global <- motifs.time
last.pos <- length(motifs.global)
mapping <- vector()
for(i in 1:length(motifs.order)){
  dupl <- FALSE
  for(j in 1:length(motifs.time)){
    if(vcount(motifs.order[[i]]) != vcount(motifs.time[[j]])){
      next
    }
    if(is_isomorphic_to(motifs.order[[i]], motifs.time[[j]], method='vf2')){
      dupl <- TRUE
      mapping[i] <- j
      cat("\n", i, " -> ", j)
      break
    }
  }
  # if motif.2 not found among motifs.1, give him its own position in 1
  if(!dupl){
    new.pos <- last.pos + 1
    motifs.global[[new.pos]] <- motifs.order[[i]] # copy motif graph
    mapping[i] <- new.pos
    cat("\nnew ", i, " -> ", new.pos)
    last.pos <- last.pos + 1
  }
}
df.posts.motif.order$motif <- mapping[df.posts.motif.order$motif]
df.posts.motif.global <- data.frame(postid = df.posts.motif.time$postid,
                                    motif.time = df.posts.motif.time$motif,
                                    motif.order = df.posts.motif.order$motif)

# Plot matrix
plot(df.posts.motif.global$motif.time, df.posts.motif.global$motif.order, cex=0.1, pch=19,
     xlab='time-based neighbourhood', ylab='order-based neighbourhood', xlim=c(0,500), ylim=c(0,500))
# and closer
plot(df.posts.motif.global$motif.time, df.posts.motif.global$motif.order, cex=0.1, pch=19,
     xlab='time-based neighbourhood', ylab='order-based neighbourhood', xlim=c(0,100), ylim=c(0,100))


# other ways
ma <- matrix(0, length(motifs.global), length(motifs.global))
for (i in 1:nrow(df.posts.motif.global)){
  a <- df.posts.motif.global$motif.time[i]
  b <- df.posts.motif.global$motif.order[i]
  ma[a,b] <- ma[a,b] + 1
}
# how many are in the diagonal?
sum(diag(ma))/69484

apply(ma,2, function(x) sum(x>0))
top.atractors <- order(apply(ma,2, function(x) sum(x>0)), decreasing = TRUE)[1:10]
apply(ma,2, function(x) sum(x>0))[top.atractors]

# we dont see nothing here :(
image(ma[1:100,1:100], col = rev(grey(seq(0, 1, length = max(ma)))))
# neither
contour(1:max(ma), 1:max(ma), ma)

#pse
levelplot(ma)

# Which are the most frequent matchings?
x <- which(ma>=sort(ma, decreasing = T)[100], arr.ind = T)
x.order <- order(ma[x], decreasing = T)
max.positions <- x[x.order, ]
freqs <- rowSums(ma[max.positions[,1],])
df.confusions <- data.frame(motif.time = max.positions[,1], 
                            motif.order = max.positions[,2], 
                            counts = ma[max.positions],
                            freq.motif.time = freqs)

# Where do match the most popular time-based neighbourhoods?
most.frequent.time <- 1:20
confounded.by <- apply(ma[most.frequent.time,], 1, which.max) #most frequent order-based counterparts
positions <- cbind(most.frequent.time, confounded.by)
freqs <- rowSums(ma[most.frequent.time,])
df.confusions <- data.frame(motif.time = most.frequent.time, 
                            motif.order = confounded.by, 
                            counts = ma[positions],
                            freq.motif.time = freqs)
####################################################
# Plot selection of neighborhoods
####################################################
mypalette <- c("black", "yellow", "orange", "red", "white")
par(mfcol=c(2,5))
selected.motifs <- top
for(i in 1:nrow(df.confusions)){
  n  <- df.confusions[i,1] # time
  m  <- df.confusions[i,2] # order
  gmotif <- as.undirected(motifs.global[[n]])
  la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
  plot(gmotif,
       layout = la,
       vertex.color=mypalette[V(motifs.global[[n]])$color],
       vertex.label = "",
       edge.arrow.size=0.6)
  title(n)
  
  gmotif <- as.undirected(motifs.global[[m]])
  la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
  plot(gmotif,
       layout = la,
       vertex.color=mypalette[V(motifs.global[[m]])$color],
       vertex.label = "",
       edge.arrow.size=0.6)
  title(m)
}