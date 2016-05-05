# Prune a tree
# according to some criteria


library(data.table)

duplicatedN <- function(x,n=2){
  DT <- data.table(A=x)
  DT[,dup:=1:.N > n,by=A]
  return(DT$dup)
}

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
  # If more than two siblings with no children, keep only the two
  for (p in 1:length(parents)){
    siblings <- neighbors(motif, parents[p], mode='in')
    siblings.indegree <- degree(motif, v=siblings, mode='in')
     
    if(length(siblings)<3){
      next
    }
    x <- siblings$color
    delete <- duplicatedN(x,2) & siblings.indegree == 0
    to.delete <- union(to.delete, siblings[delete])
  }
  motif <- delete.vertices(motif, to.delete)
  motif
}

###################################################
# Test
###################################################
TEST <- FALSE
if(TEST){
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
  V(g)$color <- 1
  plot.tree(g)
  gpruned <- prune(g)
  plot.tree(gpruned)
  }

