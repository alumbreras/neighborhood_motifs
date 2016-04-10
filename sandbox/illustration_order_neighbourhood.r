library(igraph)
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

eg.id <- 5
deleted <- c(1,3,7,8,9,12,22,23,17,20,21)
closest <- c(2,3,4,6,7,8)
V(g)$color <- 'white'
V(g)$color[eg.id] <- "grey"

par(mfrow=c(1,1))
g <- as.undirected(g)
la = layout_as_tree(g, mode='out', root=1)#which.min(V(g)$date))
V(g)$size <- 6

frame.colors <-  rep("white",22) 
frame.colors[closest] <-"black"

plot(g,
     layout = la, 
     vertex.label = order(V(g)),
     vertex.label = NA,
     edge.width = 1, 
     edge.arrow.size=0.02,
     vertex.frame.color= frame.colors,
     vertex.label.color='black',
     asp=9/16,
     margin=-0.15)