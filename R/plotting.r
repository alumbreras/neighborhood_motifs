#
# Plotting functions
#

plot.tree <- function(gtree){
  # Plots a tree graph
  # Arguments:
  #   gtree: a igraph object graph with no cycles (tree)

  mypalette <- c("black", "yellow", "orange", "red", "white")
  par(mfrow=c(1,1))
  gtree.un <- as.undirected(gtree)
  la = layout_as_tree(gtree.un, mode='out', root=which.min(V(gtree.un)$date))
  plot(gtree.un,
       layout = la,
       vertex.color=mypalette[V(gtree.un)$color],
       vertex.label = "",
       vertex.size=1,
       edge.arrow.size=0.6)
}