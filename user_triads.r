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
####################################################
#  TODO: I can use a color for the ego user so that its position in the egonet is taken into account by the iso algorithm
library(RSQLite)
library(dplyr)
library(ggplot2)
library(gplots)
library(igraph)
library(GGally)
library(ggbiplot)
library(reshape2)

source('./R/extract_from_db.r')

# Load data
dataset <- "reddit"
forum <- 'podemos'
con <- dbConnect(dbDriver("SQLite"), 
                 dbname = paste0("./data/", dataset, ".db"))

# Threads in forum
thread.ids <- dbGetQuery(con,  paste0("select threadid from threads where forum='", forum, "'"))

# Dataframe of users and threads where they participated
df.user_thread <- data.frame()
for(i in 1:nrow(thread.ids)){ 
  thread.users <- dbGetQuery(con,  paste0("select user from posts where thread='", thread.ids[i,], "'"))
  thread.users$thread <- thread.ids[i,]
  #thread.users$length <- nrow(thread.users)
  df.user_thread <- rbind(df.user_thread, thread.users)
}

df.threads <- count(df.user_thread, "thread")
names(df.threads)[2] <- "length"
df.users <- count(df.user_thread, 'user')
names(df.users)[2] <- "posts"

# assign and id to each user
users <- df.users$user
users.ids <- new.env()
for(i in 1:length(users)){
  users.ids[[users[i]]] <- i
}


######################################################
# Count motifs in which each user appears
######################################################

# Now we can create a list with user entries in order to count.
users.motifs <- matrix(0, nrow=length(users), ncol=100)
rownames(users.motifs) <- users

# For every thread, recontruct its graph. Then count 
# if neighborhood at distance 1 and binary tree, max nodes is 4
# if neighborhood at distance 1 and binary tree, max nodes is 10
# 4   10   22   30   46   62   93  123...

# the maximum egonet will be a binary tree
rad <- 4
max.neighbors <- 4
motifs <- list()
plotted.trees <- 1
for(i in 1:nrow(thread.ids)){
  cat('\n', i, '/', nrow(thread.ids))
  
  #Extract the egos of every post
  g <- database.to.graph(thread.ids[i,], con, dataset)
  gp <- g$gp
  egos <- make_ego_graph(gp, rad, nodes=V(gp))
  
  if (FALSE){
    if(vcount(gp)>50){
      la <- layout_with_fr(gp)
      plot(as.undirected(gp),
           layout = la, 
           vertex.label = "",
           vertex.color = "black",
           vertex.size = 1.5 + 1.5 * log( graph.strength(gp), 3 ), 
           edge.width = 1.5)  
      dev.copy(png, paste0('2015-01-15-tree', plotted.trees, '.png'))
      dev.off()
      plotted.trees <- plotted.trees +1
    }
  }
  
  for(j in 1:length(egos)){
     # Reduce the ego to a maximum of N neighbors (the N closest in time)
     # and where the neighborhood is fully connected.
     # Set a different color (only) to the ego post
     user.name <- V(gp)[j]$user
     if (user.name == "root") 
       next
     user.id <- users.ids[[user.name]]
     post.id <- V(gp)[j]$name
     eg <- egos[[j]]
     neighbors <- order((abs(as.numeric(V(eg)$date)-as.numeric(V(gp)[j]$date))))
     reduced.neighbors <- neighbors[1:min(length(neighbors), max.neighbors)]
     eg <- induced.subgraph(eg, reduced.neighbors)
     u <- V(eg)[V(eg)$name==post.id]
     V(eg)$color <- "black"
     V(eg)[u]$color <- "red"
     V(eg)[V(eg)$user=="root"]$color="white"
     
     # Drop nodes gat are not connected to the post
     eg <- delete_vertices(eg, which(distances(eg, u)==Inf))
     
     # See if it matches any seen motif
     is.new <- TRUE
     if(length(motifs)>0){
       for(motif.id in 1:length(motifs)){
         if(is_isomorphic_to(eg, motifs[[motif.id]])){
           users.motifs[user.id, motif.id] <- users.motifs[user.id, motif.id]+1
           is.new <- FALSE
           break
     }}}
     
     # If motif is new, add it to the list
     if(is.new){
       motif.id <- length(motifs)+1
       motifs[[motif.id]] <- eg
     }
  }
}


######################################################################
# Set up a user-features matrix (features are z-scores w.r.t a motif)
######################################################################
# Summary  matrix
users.motifs <- users.motifs[,colSums(users.motifs)>0]
cat("\nMotifs counts:", colSums(users.motifs))

# Consider only active users
active.users.motifs <- users.motifs[rowSums(users.motifs)>10,]
cat("\nMotifs counts of active users:", colSums(active.users.motifs))

# Normalize user activity (make the analysis independent of number of posts)
active.users.motifs <- t(apply(active.users.motifs, 1, function(x) x/sum(x)))
cat("\nMotifs % of active users:", colSums(active.users.motifs))

# Center and scale data
scaled <- scale(active.users.motifs)
cat("\nMotifs % of active users (z-score):", colSums(scaled))

######################
# Plot found motifs
######################
par(mfrow=c(3,3))
for(i in 1:length(motifs)){
  plot(motifs[[i]],
       vertex.label = "",
       edge.arrow.size=0.2)
}
# Put names to the features
colnames(scaled) <- c("successful answer", "spot", "common answer 3", 
                      "lonely answer", "chain init answer", "common answer 2", 
                      "chain init comment", "chain common bifurcation")

# Plot sorted and with names
idx <- order(colSums(users.motifs), decreasing = TRUE) # sort by frequency
par(mfrow=c(2,4))
for(i in 1:length(motifs)){
  plot(motifs[[idx[i]]],
       vertex.label = "",
       edge.arrow.size=0.6)
  title(paste(colnames(scaled)[idx[i]]),sub=colSums(users.motifs)[idx[i]])  
}

dev.copy(png, paste0('2016-01-15-motifs_4_4.png'))
dev.off()

# make names shorter
df.scaled <- as.data.frame(scaled)
colnames(df.scaled) <- c("SA", "spot", "CA3", "LA", "CIA", "CA2", "CIC", "CCB")


####################################
# Clustering
####################################
wss <- (nrow(df.scaled)-1)*sum(apply(df.scaled,2,var))
for (i in 2:25){
  wss[i] <- sum(kmeans(df.scaled, i)$withinss)
}
plot(1:25, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
fit <- kmeans(df.scaled, 5)

# Update users df with their cluster
tmp <- data.frame(user=rownames(df.scaled), cluster=fit$cluster)
df.users <- merge(df.users, tmp, all.x=TRUE)

# Assign colors to clusters by size so that we use the same colors all along
cluster.colors <- palette()[2:length(palette())] # 2: to avoid black

# Complementary
library(mclust)
BIC <-  mclustBIC(df.scaled)
plot(BIC)
fit2 <-Mclust(df.scaled)
summary(fit2, parameters=TRUE)

# Try a hierarchical clustering (todo)

# Plot cluster profiles (means and variances)
cluster.means <- aggregate(df.scaled, by = list(fit$cluster), FUN = mean)
names(cluster.means)[1] <- c("cluster")
melted.means <- melt(cluster.means, id = "cluster", value.name = "mean")

cluster.devs <- aggregate(df.scaled, by = list(fit$cluster), FUN = sd)
names(cluster.devs)[1] <- c("cluster")
melted.devs <- melt(cluster.devs, id = "cluster",  value.name = "var")

cluster.profiles <- merge(melted.means, melted.devs, by=c("cluster", "variable"))
cluster.profiles$cluster <- as.factor(cluster.profiles$cluster)

ggplot(cluster.profiles, aes(x = variable, 
                             y = mean, 
                             group = cluster, 
                             colour = cluster)) + 
        scale_colour_manual(values = cluster.colors) +
        geom_line() + 
        geom_point(size=4, shape=21, fill="white")+
        geom_errorbar(aes(ymin=mean-var/2, ymax=mean+var/2))+
        theme(legend.key = element_blank())

# Colored scatter matrix
########################
if(FALSE){
  # df.scaled$colors <- cluster.colors[fit$cluster]
  p1 <- ggpairs(data    = df.scaled,
                columns = 1:8,
                upper   = list(continuous = "points"),
                lower   = list(continuous = "points"),
                diag    = list(continuous = "density"),
                colour  = 'colors')
  print(p1)
}

###############################################
# PCA projection of users (colored)
##############################################
# non-colored
par(mfrow=c(1,1))
pca <- princomp(df.scaled)
plot(pca)
biplot(pca, xlabs = rep(".", nrow(df.scaled)))

# colored (size proportional to number of posts)
####################################################
par(mfrow=c(1,1))
nposts <- rowSums(users.motifs[rowSums(users.motifs)>10,])

plot(pca$scores[,1], pca$scores[,2], 
     col  = alpha(cluster.colors[fit$cluster], 0.5), 
     cex  = 0.5*log(nposts), 
     pch  = 19,
     xlab = "Dimension 1", ylab = "Dimension 2")
text(pca$scores[,1], pca$scores[,2], 
     labels = rownames(df.scaled), 
     cex    = 0.7)
title("Individual factor map (PCA)")
dev.copy(png, paste0('2016-01-15-PCA.png'), width = 800, height = 800)
dev.off()

# Colored PCA (another library)
###################################################
#df.scaled$cluster <- as.factor(fit$cluster)

p1 <- ggbiplot(pca, 
               obs.scale = 1, 
               var.scale    = 1, 
               groups       = factor(fit$cluster), 
               ellipse      = TRUE, 
               circle       = TRUE,
               labels       = rownames(df.scaled),
               labels.size  = 3.5,
               varname.size = 4) 
p1 <- p1 + scale_color_manual(values = alpha(cluster.colors,0.75))
p1 <- p1 + theme_bw() +
           theme(text            = element_text(size = 15),
                 aspect.ratio    = 1,
                 legend.position = "none") +
            ggtitle("Individual factor map (PCA)")
print(p1) 
dev.copy(png, paste0('2016-01-15-PCA2.png'), width=800, height=800)
dev.off()

# Boxplots
#########################

# Clusters profile
points <- df.scaled[1:(ncol(df.scaled))]
points$cluster <- factor(fit$cluster)
points <- melt(points, id='cluster')
p <- ggplot(points, aes(x=variable, y=value)) + 
     scale_fill_manual(values = cluster.colors) + 
     geom_boxplot(aes(fill = cluster), position = position_dodge(width = 0.75))
p <- p + theme_bw() +
        theme(text = element_text(size = 15),
              legend.key = element_blank()) +
     ggtitle("Clusters means and variances")
print(p)
dev.copy(png, paste0('2016-01-15-whiskers.png'), width=800)
dev.off()

# Individual relation between role participation and final length
#roles_length <- df.user_thread[,c(4,3)]
# Relation between role participation and length
merged1 <- merge(df.user_thread, df.users)
merged2 <- merge(merged1, df.threads)
df.participations <- merged2

# no role -> cluster 0
df.participations[is.na(df.participations$cluster),]$cluster <- 0
df.participations$cluster <- factor(df.participations$cluster)
df.roles_length <- df.participations[c(3,5)]

plot(df.roles_length)

p <- ggplot(df.roles_length, aes(x=cluster, y=length)) + 
  scale_fill_manual(values = c("white", cluster.colors)) + # no role -> white color
  geom_boxplot(aes(fill=cluster))
p <- p + theme_bw() +
  theme(text = element_text(size = 15),
        legend.key = element_blank()) +
    ggtitle("Participations vs threads length")
print(p)
dev.copy(png, paste0('2016-01-15-whiskers_roles_vs_length.png'), width=800)
dev.off()


###########################################
# Cluster threads by role composition 
# (role 0 is no role. Threads with no active users will have some of these)
###########################################
by_thread_cluster <- acast(df.participations, thread~cluster)
by_thread_cluster.perc <- t(apply(by_thread_cluster, 1, function(x) x/sum(x)))

heatmap(by_thread_cluster.perc, labRow=NA, ylab="Threads", xlab="Roles")
dev.copy(png, paste0('2016-01-15-thread_role_composition.png'), width=600)
dev.off()

