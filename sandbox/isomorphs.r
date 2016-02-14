g1 <- graph.empty(1)
g2 <- make_graph(c(2,1, 3,1, 4,1))
V(g1)$color <- 2
V(g2)$color <- c(3,1,1,1)
is_isomorphic_to(g1,g2, method='vf2')

mypalette <- c("black", "red", "white")

par(mfrow=c(1,2))
la <- layout_with_fr(g1)
plot(g1,
     layout = la, 
     vertex.color = mypalette[V(g1)$color],
     vertex.label = V(g1)$color,
     vertex.size = 5, 
     edge.width = 1.5)
title("g1")

la <- layout_with_fr(g2)
plot(g2,
     layout = la, 
     vertex.color = mypalette[V(g2)$color],
     vertex.label = V(g2)$color,
     vertex.size = 5, 
     edge.width = 1.5)
title("g2")

g1 <- make_ring(3, directed=TRUE)
g2 <- make_ring(3, directed=TRUE)
V(g1)$color <- c(1,2,3)
V(g2)$color <- c(1,2,3)
isomorphic(g1,g2)
isomorphic(g1,g2, method='vf2')



mypalette <- c("black", "red", "white")

par(mfrow=c(1,2))
la <- layout_with_fr(g1)
plot(g1,
     layout = la, 
     vertex.color = mypalette[V(g1)$color],
     vertex.label = V(g1)$color,
     vertex.size = 5, 
     edge.width = 1.5)
title("g1")

la <- layout_with_fr(g2)
plot(g2,
     layout = la, 
     vertex.color = mypalette[V(g2)$color],
     vertex.label = V(g2)$color,
     vertex.size = 5, 
     edge.width = 1.5)
title("g2")























#write.graph(g1, "g1", format='graphml')
#write.graph(g2, "g2", format='graphml')
#write.graph(eg, "eg", format='graphml')
#write.graph(gmotif, "gmotif", format='graphml')

#g1 <- read.graph("g1",  format='graphml')
#g2 <- read.graph("g2",  format='graphml')
#eg <- read.graph("eg",  format='graphml')
#gmotif <- read.graph("gmotif",  format='graphml')
#is_isomorphic_to(eg, gmotif, method='vf2')

g1_bug <- read.graph("g1",  format='graphml')
g2_bug <- read.graph("g2",  format='graphml')

eg <- read.graph("eg2",  format='graphml')
gmotif <- read.graph("gmotif2",  format='graphml')
is_isomorphic_to(eg, gmotif, method='vf2')

#is_isomorphic_to(g1, eg, method='vf2')
#[1] TRUE
#is_isomorphic_to(g2, gmotif, method='vf2')
#TRUE
#is_isomorphic_to(eg, gmotif, method='vf2')
#Error in .Call("R_igraph_isomorphic_vf2", graph1, graph2, vertex.color1,  : 
#                 At topology.c:1106 : Invalid vertex color vector length, Invalid value
#               Called from: is_isomorphic_to(eg, gmotif, method = "vf2")
