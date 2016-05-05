#
# Plotting functions
#


plot.tree.breakpoints <- function(gtree, breakpoints.v, breakpoints.h, labels=c('name', 'id')){
  mypalette <- c("black", "yellow", "orange", "red", "white")
  par(mfrow=c(1,1))
  gtree.un <- as.undirected(gtree)
  V(gtree.un)$color <- 'white'
  V(gtree.un)[breakpoints.v]$color <- 'green'
  V(gtree.un)[breakpoints.h]$color <- 'blue'
  
  V(gtree.un)$size <- 3
  V(gtree.un)[breakpoints.v]$size <- 10
  V(gtree.un)[breakpoints.h]$size <- 10  
  
  la = layout_as_tree(gtree.un, mode='out', root=which.min(V(gtree.un)$date))
  if (labels=='name'){
    labels <- V(gtree)$name
  }
  else{
    labels <- as.numeric(V(gtree))
  }
  plot(gtree.un,
       layout = la,
       vertex.label = labels,
       edge.arrow.size=0.6)
}

plot.tree <- function(gtree, labels=c('name', 'id')){
  # Plots a tree graph
  # Arguments:
  #   gtree: a igraph object graph with no cycles (tree)
  if (missing(labels)){
    labels <- NA
  }
  else{
  labels <- switch(labels, 
                   'name' = V(gtree)$name, 
                   'id' = as.numeric(V(gtree)))
  }
  par(mfrow=c(1,1))
  gtree.un <- as.undirected(gtree)
  la = layout_as_tree(gtree.un, mode='out', root=which.min(V(gtree.un)$date))

  plot(gtree.un,
       layout = la,
       vertex.label = labels,
       vertex.size=3,
       edge.arrow.size=0.6)
}

plot.trees <- function(trees, labels){
  # Plots a set of trees in a grid
  # Arguments:
  #   trees: a list of igraph tree objects
  #   labels: a label for each tree.
  mypalette <- c("black", "yellow", "orange", "red", "white")
  par(mfrow=c(3,5))
  
  for(i in 1:length(trees)){
    gmotif <- as.undirected(trees[[i]])
    la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
    plot(gmotif,
         layout = la,
         vertex.color=mypalette[V(gmotif)$color],
         vertex.label = "",
         edge.arrow.size=0.6)
    title(labels[i])
  }
  
}