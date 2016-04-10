prune <- function(motif){
  # Arguments:
  #   motif: a tree graph
  # Returns:
  #   a pruned motif
  # 4 colors: 1(black) 2(red) 4(orange)
  # we can only remove black nodes
  # there can be only two black nodes together
  # delete 
  parents <- which(degree(motif, mode='in')>2)
  to.delete <- c()
  if(length(parents)==0){
    return(motif)
  }
  # If more than two siblings with no children, keep only the first two
  # (the most ancient)
  for (p in 1:length(parents)){
    siblings <- neighbors(motif, parents[p], mode='in')
    siblings <- siblings[order(V(motif)$date[siblings])]
    siblings.indegree <- degree(motif, v=siblings, mode='in')
    
    if(length(siblings)<3){
      next
    }
    x <- siblings$color
    
    counts <- 1
    delete <- rep(FALSE, length(x))
    for(i in 2:length(x)){
      if(x[i] != x[i-1]){
        counts <- 1
      }
      else if(siblings.indegree[i]>0){
        counts <- 1
      }
      else{
        counts <- counts + 1
      }
      cat('\n id:', as.numeric(siblings[i]), '--', counts)
      if(counts>2){
        delete[i] <- TRUE
      }
    }
    to.delete <- union(to.delete, siblings[delete])
  }
  motif <- delete.vertices(motif, to.delete)
  motif
}

i <- 28

par(mfrow=c(4,2))
par(mfrow=c(1,2))

#for (i in 1:length(motifs)){

#for (i in 1:100){  
  
  gmotif <- motifs[[i]]
  if(!any(degree(gmotif, mode='in')>1)){
    next
  }
  mypalette <- c("grey", "red", "white", 'orange')
  if(!any(degree(gmotif, mode='in')>3)){
    next
  }
  #root <- V(gmotif)[which(degree(gmotif, mode='out')==0)]
  
  gmotif <- as.undirected(gmotif)
  la = layout_as_tree(gmotif, mode='out', root=which.min(as.numeric(V(gmotif)$date)))
  plot(gmotif,
       layout = la,
       vertex.color=mypalette[V(gmotif)$color],
       vertex.label = V(gmotif),#"",
       edge.arrow.size=0.6)
  title(i)
  
  
  gmotif <-  prune(motifs[[i]])
  gmotif <- as.undirected(gmotif)
  
  la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
  plot(gmotif,
       layout = la,
       vertex.color=mypalette[V(gmotif)$color],
       vertex.label = V(gmotif),#"",
       edge.arrow.size=0.6)
#}

if(FALSE){
  ##########################################
  par(mfrow=c(1,2))
  empty_graph()
  
  g <- make_empty_graph(n=23)
  g <- add_edges(g, c(2,1,
                      3,2,
                      6,2,
                      5,2,
                      4,2,
                      7,3,
                      8,3,
                      8,9,
                      10,6,
                      11,10,
                      12,10,
                      14,10,
                      15,5,
                      13,10,
                      17,10,
                      18,15,
                      16,15,
                      19,2,
                      20,2,
                      21,2,
                      22,12,
                      23,22))
  
  mypalette <- c("black", "red", "white", 'orange')
  V(g)$color <- '3'
  V(g)$date <- as.numeric(V(g))
  g_ <- as.undirected(g)
  la = layout_as_tree(g_, mode='out', root=1)
  plot(g_,
       layout = la,
       vertex.color=mypalette[V(g_)$color],
       vertex.label = V(g_)$date,
       edge.arrow.size=0.6)
  
  
  
  g_ <-  prune(g)
  g_ <- as.undirected(g_)
  
  la = layout_as_tree(g_, mode='out', root=1)
  plot(g_,
       layout = la,
       vertex.color=mypalette[V(g_)$color],
       vertex.label = V(g_)$date ,
       edge.arrow.size=0.6)
}