# Shows the different conversations (neighbourhoods) in which a user participated
# 
# author: Alberto Lumbreras
library(RSQLite)

plot.user.participations <- function(thread, user, con, database='reddit'){
  
  g <- database.to.graph(thread, con, database)
  gp <- g$gp
  mask <- V(gp)$user==user
  posts <- V(gp)[mask]
  
  for(i in 1:length(posts)){
    eg <- neighborhood.temporal.order(gp, posts[i], 2, 4)
    g <- graph.union(gp,eg)
    
    
    # Color nodes
    root.post <- V(gp)[which(degree(gp, mode='out')==0)]$name
    user.name <- V(gp)[posts[i]]$user
    post.id <- V(gp)[posts[i]]$name
    
    # See if it matches any seen motif
    u <- V(gp)[V(gp)$name==post.id] # identify the ego vertex
    uu <- V(gp)[V(gp)$user==user.name] # posts writen by ego author
    
    # note the <= in case two siblings have the same date
    V(gp)$color[V(gp)$date<=V(gp)[u]$date] <- 1 
    V(gp)$color[V(gp)$date>V(gp)[u]$date] <- 2 
    if(degree(gp, v=u, mode='out')==1){
      p.user.name <- neighbors(gp, u, mode='out')$user
      V(gp)[V(gp)$user==p.user.name]$color <- 3 # posts writen by parent of ego
    }
    V(gp)[uu]$color <- 4
    V(gp)[u]$color <- 5
    V(gp)[V(eg)$name==root.post]$color <- 6
    V(gp)[! V(gp)$name %in% V(eg)$name]$color <- 7
    
    mypalette <- c("grey", "black", "yellow", "orange", "red", "white", 'green')
    
    # Plot
    gmotif <- as.undirected(gp)
    la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
    plot(gmotif,
         layout = la,
         vertex.color=mypalette[V(gmotif)$color],
         vertex.label = order(V(gp)$date),
         vertex.label.cex = 1,
         vertex.size= ifelse(V(gp)$color==7, 1, 3),
         edge.arrow.size=0.6)
    title(paste(user))
  }
}


con <- dbConnect(dbDriver("SQLite"), dbname = paste0("./data/", database, ".db"))
database <- 'reddit'

for (u in 1:length(cluster1)){
  user <- as.character(cluster1[u])
  query <- paste0("SELECT DISTINCT(t.threadid)  FROM  posts p, threads t WHERE t.forum LIKE 'podemos' AND p.thread = t.threadid AND p.user = '", user, "'")
  threads <- dbGetQuery(con, query)$threadid
  
  for(i in 1:length(threads)){
    plot.user.participations(threads[i], user, con)
    
    answer <- readline("Continue with user? ([y]/n/exit) ")
    if (answer=='yes' || answer == '')
    {
      next
    }
    else if (answer == 'n'){
     break 
    }
    else if (answer == 'exit'){
      stop("Bye!")
    }
  }
}



