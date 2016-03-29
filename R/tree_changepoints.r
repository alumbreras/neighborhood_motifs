# Functions to extract the neighborhood of a post in a tree
# Function to detect the changepoints of a tree w.r.t time
# between posts
# author: Alberto Lumbreras
##############################################################

#t3_2eh5kz    141
#t3_2dyzfs     36
#t3_2dz84u     11
#t3_2dz884     81
#t3_2dz88i     84
#t3_2e049q      7
#t3_2e04x4     11
#t3_2e05bb     14

changepoints <- function(gp, vertical=FALSE, horizontal=FALSE){
  # Detect the posts that took a time too long to the previous post
  # - to its parent (vertical changepoint)
  # - to its siblings (horizontal changepoints)
  if(!is.numeric(V(gp)$date)){
    stop('Dates are not numeric')
  }
  
  breakpoints.v <- c()
  breakpoints.h <- c()
  
  # Vertical changepoints (from root to every leaf)
  ##################################################
  if(vertical==TRUE){
    leafs <- which(ego_size(gp, 1, mode='in', mindist=1)==0)
    root <-  which(ego_size(gp, 1, mode='out', mindist=1)==0)
    for(j in 1:length(leafs)){
      #j<-17
      # Path and its deltas 
      path <- unlist(shortest_paths(gp, root, to=leafs[j], mode='all')$vpath)
      if(length(path)<5){
        next
      }
      dates <- as.numeric(V(gp)[path]$date)
      deltas <-  diff(dates) 
      scores <- deltas/quantile(deltas, 0.5)
      breakpoints.v <- c(breakpoints.v, path[which(scores>2)+1]) # deltas had an offset w.r.t dates
      #scores <- c(NA, scores)
      #probs[path] <- scores
      
      # Debug
      #par(mfrow=c(2,1))
      #plot(dates, cex=1, type='b')
      #cols <- rep('black', length(dates))
      #cols[which(scores>2)] <- 'red'
      #plot(scores, type='b', col=cols)
    }
    breakpoints.v <- names(breakpoints.v)
    
  }
  
  # Horizontal changepoints (between siblings)
  ############################################
  if(horizontal==TRUE){
    candidates.h <- c()
    dist.matrix <- distances(gp, mode = 'in')
    for (i in 1:nrow(dist.matrix)){
      children.mask <- dist.matrix[i,]==1
      if(sum(children.mask)>3){
        children <- colnames(dist.matrix)[which(children.mask)]
        
        # sort children by date
        children <- children[order(V(gp)[children]$date)]
        candidates.h <- c(candidates.h, children)
        dates <- as.numeric(V(gp)[children]$date)
        
        # this both cases should give the same result
        if(is.unsorted(dates)){
          stop("Posts are not sorted by date")
        }
        else{
          deltas <-  diff(dates) 
          scores <- deltas/quantile(deltas, 0.5)
          breakpoints.h <- c(breakpoints.h, children[which(scores>2)+1]) # deltas had an offset w.r.t dates
        }
      }
    }
  }
  # Breakpoint only if both in vertical 
  # and horizontal
  ########################################
  if(vertical && horizontal){
    # include posts that are breakpoints in vertical and horizontal
    # and posts that are vertical breakpoints but have no chance to be horizontal breakpoints
    # because they have not enough siblings
    breakpoints <- union(intersect(breakpoints.v, breakpoints.h),
                         setdiff(breakpoints.v, candidates.h))
  }
  if(vertical && !horizontal){
    breakpoints <- breakpoints.v
  }
  if(!vertical && horizontal){
    breakpoints <- breakpoints.h
  }
  
  list(breakpoints.vh = V(gp)[breakpoints],
       breakpoints.v = V(gp)[breakpoints.v],
       breakpoints.h = V(gp)[breakpoints.h])
}


neighborhood.temporal <- function(gp, j, rad, breakpoints.v, breakpoints.h){

  #################################################
  # temporal neighborhood based on dynamic shift detection
  dates <- as.numeric(V(gp)$date)
  user.name <- V(gp)[j]$user
  post.id <- V(gp)[j]$name
  root <- V(gp)[which(degree(gp, mode='out')==0)]
  
  # structural neighbourhood
  ###########################
  eg <- make_ego_graph(gp, rad, nodes=j)[[1]] 
  
  # temporal neighbourhood
  ##########################
  u <- V(eg)[V(eg)$name==post.id]
  ego.root <- V(eg)[which(degree(eg, mode='out')==0)]
  ascendants <- ego(eg,100, nodes=u, mode='out', mindist=0)[[1]] #include ego
  descendants <- ego(eg,100, nodes=u, mode='in', mindist=1)[[1]]
  parent <- neighbors(eg, u, mode='out')
  siblings <- neighbors(eg, parent, mode='in')
  older.siblings <- siblings[siblings$date < u$date]
  
  # delete nodes only at the end to avoid re-indexing the graph
  # in the middle of the operations
  to.delete <- c()
  
  if (length(breakpoints.h)>0){
    for (k in 1:length(breakpoints.h)){
      bp <- breakpoints.h[k]
      
      # if breakpoint not in neighborhood we ignore it
      # else locate it in the neighborhood
      if (! bp$name %in% V(eg)$name){
        next
      }
      else{
        bp <- V(eg)[V(eg)$name==bp$name]
      }
      bp.date <- bp$date

      if(bp %in% union(older.siblings, ascendants)){
        # if antecedent or older brother of ego, delete older brothers
        # (= delete previous dynamics)
        bp.parent <- neighbors(eg, bp, mode='out')
        siblings <- neighbors(eg, bp.parent, mode='in')
        bp.older.siblings <- siblings[siblings$date < bp$date]
        to.delete <- c(to.delete, bp.older.siblings)
      }
      else{
        # else, delete younger brothers and breakpoint
        # (= delete next dynamics)
        bp.parent <- neighbors(eg, bp, mode='out')
        siblings <- neighbors(eg, bp.parent, mode='in')
        bp.younger.siblings <- siblings[siblings$date > bp.date]
        to.delete <- c(to.delete, union(bp.younger.siblings, bp))
      }
    }
  }
  
  if (length(breakpoints.v)>0){
    for(k in 1:length(breakpoints.v)){
      bp <- breakpoints.v[k]
      
      # if breakpoint not in neighborhood we ignore it
      if (! bp$name %in% V(eg)$name){
        next
      }
      else{
        bp <- V(eg)[V(eg)$name==bp$name]
      }
      bp.date <- bp$date
      
      if(bp %in% ascendants){
        # if antecedent, delete even older ascendents
        # (= delete previous dynamic)
        p <- neighbors(eg, bp, mode='out')
        to.delete <- c(to.delete, p)
      }
      else{
        # else, delete breakpoint and its descendents
        # (= detele next dynamics)
        to.delete <- c(to.delete, bp)
      }
    }
  }
  eg <- delete.vertices(eg, to.delete)    
  # Drop nodes that are not connected to the post
  #Cannot use a vertex sequence from another graph.
  # distances(eg, u)
  u <- V(eg)[V(eg)$name==post.id]
  eg <- delete_vertices(eg, which(distances(eg, u)==Inf))

  par(mfrow=c(1,1))
  gp_ <- as.undirected(gp)
  la = layout_as_tree(gp_, mode='out', root=which.min(V(gp_)$date))
  V(gp_)$size <- 1
  V(gp_)$color <- 'black'
  V(gp_)[V(eg)$name]$color <- 'darkgreen'
  V(gp_)[V(eg)$name]$size <- 2
  V(gp_)[j]$size <- 5
  V(gp_)[j]$color <- 'green'
  V(gp_)[breakpoints.v]$color <- 'orange'
  V(gp_)[breakpoints.h]$color <- 'yellow'
  plot(gp_,
       layout = la, 
       #vertex.label = order(V(gp)),
       vertex.label = NA,
       edge.width = 0.2, 
       edge.arrow.size=0.02,
       asp=9/16,
       margin=-0.15)
  title("temp")
  
  eg

}


neighborhood.temporal.order <- function(gp, j, rad, max.neighbors){
  #
  # Temporal neighbourhood based on closer posts (in time)
  #
  dates <- as.numeric(V(gp)$date)
  user.name <- V(gp)[j]$user
  post.id <- V(gp)[j]$name
  root <- V(gp)[which(degree(gp, mode='out')==0)]
  
  # structural neighbourhood
  ###########################
  eg <- make_ego_graph(gp, rad, nodes=j)[[1]] 
  
  # temporal neighbourhood
  ############################
  # Reduce the ego to a maximum of N neighbors (the N closest in time)
  # and where the neighborhood is fully connected.
  
  ################################################
  # temporal neighbourhood based on closer posts (in time)
  neighbors <- order((abs(as.numeric(V(eg)$date)-as.numeric(V(gp)[j]$date))))
  reduced.neighbors <- neighbors[1:min(length(neighbors), max.neighbors)]
  eg <- induced.subgraph(eg, reduced.neighbors)
  
  # Drop nodes that are not connected to the post
  #Cannot use a vertex sequence from another graph.
  # distances(eg, u)
  u <- V(eg)[V(eg)$name==post.id]
  eg <- delete_vertices(eg, which(distances(eg, u)==Inf))
  eg

}

if(FALSE){
  library(RSQLite)
  library(igraph)
  source('../R/extract_from_db.r')
  database <- 'reddit'
  con <- dbConnect(dbDriver("SQLite"), dbname = paste0("../data/", database, ".db"))
  thread <- 't3_2eh5kz'
  thread <- 't3_2dz88i'
  thread <- 't3_295qyv' #slow?
  thread <- 't3_25z91r' 
  g <- database.to.graph(thread, con, database)
  gp <- g$gp
  
  # cast dates to numeric
  dates <- as.numeric(V(gp)$date)
  gp <- remove.vertex.attribute(gp, "date")
  V(gp)$date <- dates
}

##########################"
# Test neighborhoods
#########################
# Debug
if(FALSE){
  rad <- 4
  max.neighbors <- 20
  j <- 1
  
  breakpoints <- changepoints(gp, vertical=T, horizontal=T)
  breakpoints.h <- breakpoints$breakpoints.h
  breakpoints.v <- breakpoints$breakpoints.v
  breakpoints.vh <- breakpoints$breakpoints.vh
  
  for(i in 1:vcount(gp)){
    eg <- neighborhood.temporal(gp, i, 3, breakpoints.v, breakpoints.h)
    plot(eg)
  }
  
  par(mfrow=c(1,1))
  gp_ <- as.undirected(gp)
  la = layout_as_tree(gp_, mode='out', root=which.min(V(gp_)$date))
  V(gp_)$size <- 1

  V(gp_)$color <- 'black'
  V(gp_)[V(eg)$name]$color <- 'darkgreen'
  V(gp_)[V(eg)$name]$size <- 2
  V(gp_)[j]$size <- 3
  V(gp_)[j]$color <- 'green'
  V(gp_)[breakpoints.v]$color <- 'orange'
  V(gp_)[breakpoints.h]$color <- 'yellow'
  V(gp_)[breakpoints.vh]$color <- 'red'
  plot(gp_,
       layout = la, 
       #vertex.label = order(V(gp)),
       vertex.label = NA,
       edge.width = 0.2, 
       edge.arrow.size=0.02,
       asp=9/16,
       margin=-0.15)
}


##############################
##############################
# Sequential show (for demos)
if(FALSE){
  dates <- as.numeric(V(gp_)$date)
  steps <- seq(min(dates), max(dates), length.out = 700)
  for(i in 1:100){
    V(gp_)$color <- 'black'
    V(gp_)[breakpoints]$color <- 'red'
    inactive <- as.numeric(V(gp_)$date) > steps[i]
    V(gp_)[inactive]$color <- 'white'
    plot(gp_,
         layout = la, 
         vertex.size = 1,
         vertex.label = NA,
         edge.width = 0.2, 
         edge.arrow.size=0.02,
         asp=9/16,
         margin=-0.15)
    title((steps[i]-min(dates))/3600)
    readline(paste(i, " next..."))
  }
}