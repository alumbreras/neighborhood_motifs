# Functions for Neighbourhood extraction
#########################################

neighborhood.temporal.order.ASONAM <- function(gp, j, rad, max.neighbors){
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
  
  # if ego has same date than N posts, it might fall out of the selection:
  # order((abs(V(eg)-V(gp)[j]$date)))
  # to avoid that, we ignore the first one (that should be the ego)
  # and add the ego manually
  neighbors <- c(j, order((abs(V(eg)-V(gp)[j]$date)))[-1]) 
  cat('\nextract.ego', j)
  cat("\ndates", V(eg)$date)
  cat("\n extract.neighs:", neighbors)
  reduced.neighbors <- neighbors[1:min(length(neighbors), max.neighbors)]
  eg <- induced.subgraph(eg, reduced.neighbors)
  
  # Drop nodes that are not connected to the post
  #Cannot use a vertex sequence from another graph.
  # distances(eg, u)
  u <- V(eg)[V(eg)$name==post.id]
  eg <- delete_vertices(eg, which(distances(eg, u)==Inf))
  
  if(! j %in% V(eg)) stop("Ego not in neighbourhood!")
  eg
}


neighborhood.order <- function(gp, j, rad){
  #
  # Temporal neighbourhood based on closer posts (in time)
  # Neighbours distances is sorted by geodesic distance
  # in case of tie, we get the closest in time
  #
  
  # Vertex from the closest to the furthest in time
  # if ego has same date than N posts, it might fall out of the selection:
  # order(distances(eg, j))
  # to avoid that, we ignore the first one (that should be the ego)
  # and add the ego manually
  dates <- vertex_attr(gp, 'date')
  dates.distances <- abs(dates[j]-dates)
  geodesic.distances <- distances(gp, j)
  neighbours <- head(order(geodesic.distances, dates.distances), rad)
  eg <- induced_subgraph(gp, neighbours)
  eg
}


# This generates too large neighbourhoods
neighborhood.order2_ <- function(gp, j, rad){
  #
  # Temporal neighbourhood based on closer posts (in time)
  #

  # Vertex from the closest to the furthest in time
  # if ego has same date than N posts, it might fall out of the selection:
  # order(distances(eg, j))
  # to avoid that, we ignore the first one (that should be the ego)
  # and add the ego manually
  ego.date <- vertex_attr(gp, 'date', j)
  neighbours <- c(j, order(abs(ego.date - V(gp)$date))[-1])
  for(n in min(rad,length(neighbours)):length(neighbours)){
    eg <- induced_subgraph(gp, neighbours[1:n])
    if(is.connected(eg)) break
  }
  # debug: if(! post.id %in% V(eg)$name) stop("Ego not in neighbourhood!")
  eg
}

neighborhood.order.bad_ <- function(gp, j, rad){
  #
  # Temporal neighbourhood based on closer posts (in time)
  #
  
  # Vertex from the closest to the furthest in time
  # if ego has same date than N posts, it might fall out of the selection:
  # order(distances(eg, j))
  # to avoid that, we ignore the first one (that should be the ego)
  # and add the ego manually
  neighbours <- c(j, order(distances(gp, j))[-1])
  for(n in min(rad,length(neighbours)):length(neighbours)){
    eg <- induced_subgraph(gp, neighbours[1:n])
    if(is.connected(eg)) break
  }
  # debug: if(! post.id %in% V(eg)$name) stop("Ego not in neighbourhood!")
  eg
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
  
  if (sum(V(eg)$bp.h)>0){
    breakpoints.h.ego <- V(eg)[V(eg)$bp.h==TRUE]
    for (k in 1:length(breakpoints.h.ego)){
      bp <- breakpoints.h.ego[k]
      bp.date <- bp$date
      if((bp %in% ego.older.siblings) || (bp %in% ego.ascendants)){
        # if antecedent or older brother of ego, delete older brothers
        # (= delete previous dynamics)
        bp.parent <- neighbors(eg, V(eg)[bp], mode='out') #V(eg)[bp] to avoid bug
        siblings <- neighbors(eg, V(eg)[bp.parent], mode='in')
        
        bp.older.siblings <- siblings[siblings$date < bp$date]
        to.delete[[n]] <- bp.older.siblings
        n <- n+1
      }else{
        # else, delete younger brothers and breakpoint
        # (= delete next dynamics)
        bp.parent <- neighbors(eg, V(eg)[bp], mode='out')
        siblings <- neighbors(eg, V(eg)[bp.parent], mode='in')
        
        bp.younger.siblings <- siblings[siblings$date > bp.date]
        to.delete[[n]] <- bp.younger.siblings
        to.delete[[n+1]] <- bp
        n <- n+2
      }
    }
  }
  
  if (sum(V(eg)$bp.v)>0){
    breakpoints.v.ego <- V(eg)[V(eg)$bp.v==TRUE]
    
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
  eg
  
}