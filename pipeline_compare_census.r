# Compare the nieghbourhood census of two or more forums
# author: Alberto Lumbreras

library(dplyr)
library(igraph)
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

# Load posts dataframe (df.posts)
load('./R_objects/dfposts_gameofthrones.Rda')
#load('./R_objects/dfposts_podemos.Rda')
df.posts$date <- as.numeric(df.posts$date)
df.posts <- data.frame(df.posts) %>% arrange(date)
df.posts <- df.posts[1:75000,] # Paper

df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"


# Load the results of the neighbourhoud detection (res)
load("./R_objects/res_time_75000_gameofthrones.Rda")
#load("./R_objects/res_time_75000_podemos.Rda")

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

# Check they are sorted by frequency (they should be)
n <- as.numeric(table(df.posts.1$motif))
all(n == cummin(n))


###############################################################
# Load another kind of motifs
##############################################################
# Load posts dataframe (df.posts)
#load('./R_objects/dfposts_podemos.Rda')

load('./R_objects/dfposts_gameofthrones.Rda')
#load('./R_objects/dfposts_podemos.Rda')
df.posts$date <- as.numeric(df.posts$date)
df.posts <- data.frame(df.posts) %>% arrange(date)
df.posts <- df.posts[1:75000,] # Paper

df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"

# Load the results of the neighbourhoud detection (res)
load("./R_objects/res_order_2_4_75000_gameofthrones.Rda")
#load("./R_objects/res_order_2_4_75000_podemos.Rda")

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

# Check they are sorted by frequency (they should be)
n <- as.numeric(table(df.posts.2$motif))
all(n == cummin(n))



########################################
# Plot census distribution
########################################
par(mfrow=c(1,1))
n1 <- hist(df.posts.1$motif, breaks=0:max(df.posts.1$motif), plot=FALSE)$counts
n2 <- hist(df.posts.2$motif, breaks=0:max(df.posts.2$motif), plot=FALSE)$counts

plot(1:max(df.posts.1$motif), n1, ylim=c(1, 20000), log='y',
     ylab='Frequency', xlab='Neighbourhood',
     pch=19, cex=0.1)

points(1:max(df.posts.2$motif), n2, 
     pch=19, cex=0.1, col='red')
#legend('topright', c('time-based', 'order-based'), col=1:2, pch=19)
legend(1805,5, c('time-based', 'order-based'), col=1:2, pch=19)
title(main='Census distribution (Game of Thrones)')

#In-picture plot
par(new=TRUE)
par(oma=c(0,3,5,3))
par(mfcol=c(2,2), mfg=c(1,2))
par(mar=c(4,4,1,1))

n.max <- 100
plot(1:n.max, n1[1:n.max], ylim=c(1, 20000), log='y',
     ylab=NA, xlab=NA,
     pch=19, cex=0.1)

points(1:n.max, n2[1:n.max], 
       pch=19, cex=0.1, col='red')


# Cumulative
par(mfrow=c(1,1))
plot(cumsum(n1), pch=19, cex=0.5, ylab='Frequency (cum)', xlab='Neighbourhoods')
points(1:max(df.posts.2$motif), cumsum(n2), pch=19, cex=0.1, col='red')
#legend('topright', c('time-based', 'order-based'), col=1:2, pch=19)
legend(1805,65000, c('time-based', 'order-based'), col=1:2, pch=19)
title('Cumulative census distribution (Game of Thrones)')

#In-picture plot
par(new=TRUE)
par(oma=c(5,3,0,3))
par(mfcol=c(2,2), mfg=c(2,2))
par(mar=c(4,4,1,1))

n.max <- 100
plot(cumsum(n1[1:n.max]),  ylim=c(1, 70000), pch=19, cex=0.5, ylab=NA, xlab=NA)
points(1:n.max, cumsum(n2[1:n.max]), pch=19, cex=0.1, col='red')

######################
# How many neighbourhoods explain the 90% of the census?
#########################################################
total <- sum(n1)
th <-total*0.95
sum(cumsum(n1) < th)

total <- sum(n2)
th <-total*0.95
sum(cumsum(n2) < th)


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
selected.motifs <- top.atractors
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