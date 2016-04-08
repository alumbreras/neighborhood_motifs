# Functions to extract the neighborhood of a post in a tree
# Function to detect the changepoints of a tree w.r.t time
# between posts
# author: Alberto Lumbreras
##############################################################
library(changepoint)

#t3_2eh5kz    141
#t3_2dyzfs     36
#t3_2dz84u     11
#t3_2dz884     81
#t3_2dz88i     84
#t3_2e049q      7
#t3_2e04x4     11
#t3_2e05bb     14
DEBUG <- FALSE
changepoints.deltasequence <- function(gp, vertical=FALSE, horizontal=FALSE){
  # Changepoints based of delta deviations from the median in the sequence
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

changepoints.bertrand <- function(gp, vertical=FALSE, horizontal=FALSE){
  # Changepoints based of delta deviations from the last delta
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
    nascendants <- ego_size(gp, 2, mode='out', mindist=1)
    candidates <- V(gp)[which(nascendants==2)]
    for(j in 1:length(candidates)){
      eg <- make_ego_graph(gp, 2, nodes=candidates[j], mode='out')[[1]]
      dates <- sort(V(eg)$date)
      deltas <-  diff(dates)
      score <- (deltas[2]-deltas[1])/deltas[1]
      if(score>0.5){
        breakpoints.v <- c(breakpoints.v, candidates[j]) 
      }
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
      if(sum(children.mask)>2){
        children <- colnames(dist.matrix)[which(children.mask)]
        
        # sort children by date
        children <- children[order(V(gp)[children]$date)]
        candidates.h <- c(candidates.h, children)
        dates <- as.numeric(V(gp)[children]$date)
        deltas <- diff(dates)
        
        # this both cases should give the same result
        if(is.unsorted(dates)){
          stop("Posts are not sorted by date")
        }
        else{
          for(j in 2:length(deltas)){
            score <- (deltas[j]-deltas[j-1])/deltas[j-1]      
            if(score>0.5){
              breakpoints.h <- c(breakpoints.h, children[j+1]) 
            }
          }
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


changepoints.cp <- function(gp, vertical=FALSE, horizontal=FALSE){
  # Changepoints based of changepoint detection algorithms over the full sequence
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
      bp <- cpts(cpt.meanvar(dates, class=TRUE, method="PELT"))+1
      breakpoints.v <- c(breakpoints.v, path[bp])
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
          bp <- cpts(cpt.meanvar(dates, class=TRUE, method="PELT"))+1
          breakpoints.h <- c(breakpoints.h, children[bp])
          
          #colors <- rep("black", length(dates))
          #colors[bp] <- 'red'
          #plot(dates, col=colors, pch=19, cex=0.5)
          #title("debug")
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
  #user.name <- V(gp)[j]$user
  #root <- V(gp)[which(degree(gp, mode='out')==0)]
  post.id <- V(gp)[j]$name
  
  # structural neighbourhood
  ###########################
  eg <- make_ego_graph(gp, rad, nodes=j)[[1]] 
  #cat('\nnode:', j, 'Neigh_size: ', vcount(eg))
  
  # temporal neighbourhood
  ##########################
  u <- V(eg)[V(eg)$name==post.id]
  ego.root <- V(eg)[which(degree(eg, mode='out')==0)]
  ego.ascendants <- ego(eg,rad, nodes=u, mode='out', mindist=0)[[1]] #include ego
  ego.descendants <- ego(eg,rad, nodes=u, mode='in', mindist=1)[[1]]
  ego.parent <- neighbors(eg, u, mode='out')
  ego.siblings <- neighbors(eg, ego.parent, mode='in')
  ego.older.siblings <- ego.siblings[ego.siblings$date < u$date]

  # delete nodes only at the end to avoid re-indexing the graph
  # in the middle of the operations
  to.delete <- vector('list', 100)
  n <- 1
  #adj <- get.adjacency(eg, sparse=FALSE)
  #mat.sibling <- adj%*%t(adj)
  if (sum(V(eg)$bp.h)>0){
    breakpoints.h.ego <- V(eg)[V(eg)$bp.h==TRUE]
    #cat("\n#bp.h: ", length(breakpoints.h.ego))
    for (k in 1:length(breakpoints.h.ego)){
      bp <- breakpoints.h.ego[k]
      bp.date <- bp$date

      #bp.idx <- which(names(adj)==bp$name)
      if((bp %in% ego.older.siblings) || (bp %in% ego.ascendants)){
        # if antecedent or older brother of ego, delete older brothers
        # (= delete previous dynamics)
        bp.parent <- neighbors(eg, bp, mode='out')
        siblings <- neighbors(eg, bp.parent, mode='in')
        
        #siblings <- names(mat.sibling[bp.idx, mat.sibling[bp.idx,]==1])
        #siblings <- V(eg)[siblings]
        
        bp.older.siblings <- siblings[siblings$date < bp$date]
        to.delete[[n]] <- bp.older.siblings
        n <- n+1
      }
      else{
        # else, delete younger brothers and breakpoint
        # (= delete next dynamics)
        bp.parent <- neighbors(eg, bp, mode='out')
        siblings <- neighbors(eg, bp.parent, mode='in')
        
        #siblings <- names(mat.sibling[bp.idx, mat.sibling[bp.idx,]==1])
        #siblings <- V(eg)[siblings]
        
        bp.younger.siblings <- siblings[siblings$date > bp.date]
        to.delete[[n]] <- bp.younger.siblings
        to.delete[[n+1]] <- bp
        n <- n+2
      }
    }
  }
  
  if (sum(V(eg)$bp.v)>0){
    breakpoints.v.ego <- V(eg)[V(eg)$bp.v==TRUE]
    #cat("\n#bp.v: ", length(breakpoints.v.ego))
    
    for (k in 1:length(breakpoints.v.ego)){
      bp <- breakpoints.v.ego[k]
      bp.date <- bp$date
      
      if(bp %in% ego.ascendants){
        # if antecedent, delete even older ascendents
        # (= delete previous dynamic)
        p <- neighbors(eg, bp, mode='out')
        to.delete[[n]] <- p
        n <- n+1
      }
      else{
        # else, delete breakpoint and its descendents
        # (= detele next dynamics)
        to.delete[[n]] <- bp
        n <- n+1
      }
    }
  }
  #print(to.delete)
  to.delete <- do.call('c', to.delete[1:(n-1)])
  #to.delete <- unlist(to.delete)
  
  eg <- delete.vertices(eg, to.delete)  
  # Drop nodes that are not connected to the post
  #Cannot use a vertex sequence from another graph.
  # distances(eg, u)
  u <- V(eg)[V(eg)$name==post.id]
  eg <- delete_vertices(eg, which(distances(eg, u)==Inf))
  
  #cat('\nTempNeigh_size: ', vcount(eg))
  
  if(DEBUG){
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
    title(paste("temp:", j ))
  }
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

if(DEBUG){
  library(RSQLite)
  library(igraph)
  source('../R/extract_from_db.r')
  database <- 'reddit'
  con <- dbConnect(dbDriver("SQLite"), dbname = paste0("../data/", database, ".db"))
  thread <- 't3_2eh5kz'
  thread <- 't3_2dz88i'
  thread <- 't3_25z91r' 
  thread <- 't3_295qyv' #slow?
  thread <- 't3_2elrru'
  thread <- 't3_2c14wp' # 3075 posts
  thread <- 't3_2elzjk'
  
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
# Debug changepoint detection
if(DEBUG){
  rad <- 4
  max.neighbors <- 20
  j <- 1
  
  #breakpoints <- changepoints.deltasequence(gp, vertical=T, horizontal=T)
  #breakpoints <- changepoints.bertrand(gp, vertical=T, horizontal=T)
  breakpoints <- changepoints.cp(gp, vertical=T, horizontal=T)
  
  breakpoints.h <- breakpoints$breakpoints.h
  breakpoints.v <- breakpoints$breakpoints.v
  breakpoints.vh <- breakpoints$breakpoints.vh
  
  #Add breakpoint info as attribute
  V(gp)$bp.v <- FALSE
  V(gp)$bp.h <- FALSE
  V(gp)$bp.vh <- FALSE
  V(gp)[breakpoints.v]$bp.v <- TRUE
  V(gp)[breakpoints.h]$bp.h <- TRUE
  V(gp)[breakpoints.vh]$bp.vh <- TRUE
  
  # Detect and plot neighborhoods
  test <- function(){
  
    #Rprof()
    for(i in 1:vcount(gp)){
      cat("\nvcount:", vcount(gp))
      cat("\n******", i)
      eg <- neighborhood.temporal(gp, i, 3, breakpoints.v, breakpoints.h)
      #plot(eg)
    }
    #summaryRprof()$by.total
  
  }
  library('lineprof')
  l <- lineprof(test())

  if(FALSE){
    par(mfrow=c(1,1))
    gp_ <- as.undirected(gp)
    la = layout_as_tree(gp_, mode='out', root=which.min(V(gp_)$date), circular=FALSE)
    V(gp_)$size <- 1
  
    V(gp_)$color <- 'black'
    #V(gp_)[V(eg)$name]$color <- 'darkgreen'
    #V(gp_)[V(eg)$name]$size <- 2
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
}

BERTRAND <- FALSE
if(BERTRAND){
  # Show that score>2 is not enough. It should be score>10.
  # but this is quite arbitrary...
  posts <- V(gp)[which(ego_size(gp, 2, mode='out', mindist = 1)==1)]
  dates <- sort(posts$date)
  plot(dates)
  deltas <- diff(dates)
  bp <- c()
  scores <- rep(1, length(dates))
  for(i in 2:length(deltas)){
    score <- (deltas[i]-deltas[i-1])/deltas[i-1]
    scores[i+1] <- score
    if(score>10){
      bp <- c(bp, i+1)
    }
  }
  colors <- rep("black", length(dates))
  colors[bp] <- 'red'
  plot(dates, col=colors, pch=19, cex=0.5)
}
CHANGEPOINT <- FALSE
if(CHANGEPOINT){

  
  # Horizontal 
  posts <- V(gp)[which(ego_size(gp, 2, mode='out', mindist = 1)==1)]
  dates <- sort(posts$date)
  res <- cpt.meanvar(dates, class=TRUE, method="PELT")
  #res <- cpt.meanvar(dates, penalty="None", class=TRUE, method="SegNeigh") # slow
  plot(res)
  
  bp <- cpts(res)+1
  colors <- rep("black", length(dates))
  colors[bp] <- 'red'
  plot(dates, col=colors, pch=19, cex=0.5)
  
  # Vertical
  leafs <- which(ego_size(gp, 1, mode='in', mindist=1)==0)
  root <-  which(ego_size(gp, 1, mode='out', mindist=1)==0)
  branches <- shortest_paths(gp, root, to=leafs, mode='all')$vpath
  depths <- unlist(lapply(branches, length))
  longest.branch <- which.max(depths)
  path <- unlist(shortest_paths(gp, root, to=leafs[longest.branch], mode='all')$vpath)
  posts <- V(gp)[path]
  dates <- sort(posts$date)
  res <- cpt.meanvar(dates, class=TRUE, method="PELT")
  plot(res)
  
  bp <- cpts(res)+1
  colors <- rep("black", length(dates))
  colors[bp] <- 'red'
  plot(dates, col=colors, pch=19, cex=1)
  
  
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