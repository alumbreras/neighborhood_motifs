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
source('./R/extract_from_db.r')

# Load data
dataset <- "reddit"
forum <- 'podemos'
con <- dbConnect(dbDriver("SQLite"), 
                 dbname = paste0("./data/", dataset, ".db"))

# Threads in forum
thread.ids <- dbGetQuery(con,  paste0("select threadid from threads where forum='", forum, "'"))

# Dataframe of users and threads where they participated
df <- data.frame()
for(i in 1:nrow(thread.ids)){ 
  thread.users <- dbGetQuery(con,  paste0("select user from posts where thread='", thread.ids[i,], "'"))
  thread.users$thread <- thread.ids[i,]
  rbind(df)
  df <- rbind(df, thread.users)
}
users <- unique(df$user)

# assign and id to each user
users.ids <- new.env()
for(i in 1:length(users)){
  users.ids[[users[i]]] <- i
}

# Now we can create a list with user entries in order to count.
users.motifs <- matrix(0, nrow=length(users), ncol=100)
rownames(users.motifs) <- users

# For every thread, recontruct its graph. Then count 
# if neighborhood at distance 1 and binary tree, max nodes is 4
# if neighborhood at distance 1 and binary tree, max nodes is 10
# 4   10   22   30   46   62   93  123...

# the maximum egonet will be a binary tree
rad <- 1
max.neighbors <- 4
motifs <- list()
for(i in 1:nrow(thread.ids)){
  cat('\n', i, '/', nrow(thread.ids))
  #Extract the egos of every post
  g <- database.to.graph(thread.ids[i,], con, dataset)
  gp <- g$gp
  egos <- make_ego_graph(gp, rad, nodes=V(gp))
  
  for(j in 1:length(egos)){
     # Reduce the ego to a maximum of N neighbors (the N closest in time)
     user.id <- users.ids[[V(gp)[j]$user]]
     eg <- egos[[j]]
     neighbors <- order((abs(as.numeric(V(eg)$date)-as.numeric(V(gp)[j]$date))))
     reduced.neighbors <- neighbors[1:min(length(neighbors), max.neighbors)]
     eg <- induced.subgraph(eg, reduced.neighbors)
     
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

# Summary  matrix
users.motifs <- users.motifs[,colSums(users.motifs)>0]

# Consider only active users
active.users.motifs <- users.motifs[rowSums(users.motifs)>10,]

# Normalize
average.user <- colMeans(active.users.motifs) 

# plot found motifs
par(mfrow=c(3,3))
for(i in 1:length(motifs)){
  plot(motifs[[i]])
}

