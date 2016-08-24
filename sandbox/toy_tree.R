par(mfrow=c(1,1))
mypalette <- c("black", "red", "white", 'grey')
mypalette <- adjustcolor(mypalette, alpha.f = .5)

plot.motif <- function(g){
  root <- which.min(degree(g, mode='out'))
  g <- as.undirected(g)
  la = layout_as_tree(g, mode='out', root=root)
  
  plot(g,
       layout = la,
       vertex.color=mypalette[V(g)$color],
       vertex.size=20,
       vertex.label.cex=0,
       edge.arrow.size=0.6)
}


plot.tree <- function(g){
  root <- which.min(degree(g, mode='out'))
  g <- as.undirected(g)
  la = layout_as_tree(g, mode='out', root=root)
  
  plot(g,
       layout = la,
       vertex.color=mypalette[V(g)$color],
       vertex.size=10,
       vertex.label.cex=1,
       edge.arrow.size=0.6)
}

# Create graph
gtree <- make_empty_graph(13)
gtree <-  add_edges(gtree, c(2,1, 3,2, 4,1, 5,1, 6,1, 7,4, 8,6, 9,7, 10,9, 11,8, 12,8, 13,8))
V(gtree)$date <- 1:13
V(gtree)$user <- LETTERS[1:13]
# name = id+user
V(gtree)$name <- c("1-a", "2-b", '3-c', '4-d', '5-f', '6-g', 
                   '7-h', '8-i', '9-j', '10-k', '11-l', '12-m', '13-n')
plot.tree(gtree)

th <- 1

motifs <- list()

for (post in V(gtree)){
  
  gego <- gtree
  cat("Post:", post)
  ego.user <- vertex_attr(gtree, 'user', post)
  ego.post <- vertex_attr(gtree, 'name', post)
  
  # the tree, but ego-centered
  gego.users <-  vertex_attr(gego, 'user')
  gego.names <-  vertex_attr(gego, 'name')
  gego.colors <- rep(1, length(gego.users)) # default
  gego.colors[gego.users == ego.user] <- 2 
  gego.colors[gego.names == ego.post] <- 0 
  V(gego)$color <- gego.colors
  
  gego.pruned <- prune.preorder(gego, post)
  gego.pruned <- prune(gego)
  
  
  V(gego.pruned)[ego.post]$color <-  2 # all ego posts are now colored the same
  ego.pruned.names <- vertex_attr(gego.pruned, "name") 
  if (th %in% ego.pruned.names){ 
    V(gego.pruned)[th]$color <- 3 # root
    if (vertex_attr(gego.pruned, "user", th) == ego.user){ # V(gego.pruned)[th]$user
      V(gego.pruned)[th]$color <- 4 # ego root
    }
  } 
  # store some stats for reporting
  gego.size <- vcount(gego)
  gego.pruned.size <- vcount(gego.pruned)
  
  #Get pointer to the ego.post in the pruned tree,
  # and extract the neighbourhood as usual
  post <- V(gego.pruned)[ego.post]
  gego <-neighborhood.order(gego.pruned, post, rad)
  
  motifs[[length(motifs)+1]] <- gego
}

mat.motifs <- matrix(c(2:16), nrow=3)
mat.tree  <- matrix(1, nrow=3, ncol=3)
#mat <- matrix(c(1:15), nrow=3)
mat <- cbind(mat.tree, mat.motifs)
layout(mat)

plot.tree(gtree)
for (i in 1:length(motifs)){
  plot.tree(motifs[[i]]) 
}