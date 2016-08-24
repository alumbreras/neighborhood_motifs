# Igraph functions without some checks in order to make them faster
is_isomorphic_to_opt <-function (graph1, graph2, method = c("auto", "direct", "vf2", 
                                     "bliss"), ...) 
{
  graph.isomorphic.vf2(graph1, graph2, ...)$iso
}

graph.isomorphic.vf2_opt <- function (graph1, graph2, vertex.color1, vertex.color2, edge.color1, 
          edge.color2) 
{
  vertex.color1 <- V(graph1)$color
  vertex.color1 <- as.integer(vertex.color1) - 1L
  vertex.color2 <- V(graph2)$color
  vertex.color2 <- as.integer(vertex.color2) - 1L
  edge.color1 <- NULL
  edge.color2 <- NULL
    
  on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
  res <- .Call("R_igraph_isomorphic_vf2", graph1, graph2, vertex.color1, 
               vertex.color2, edge.color1, edge.color2, PACKAGE = "igraph")
  res
}

vertex_attr_opt <- function (graph, name, index = V(graph)) 
{

  if (missing(name)) {
    if (missing(index)) {
      vertex.attributes(graph)
    }
    else {
      vertex.attributes(graph, index = index)
    }
  }
  else {
    myattr <- base::.Call("R_igraph_mybracket2", graph, 9L, 
                          3L, PACKAGE = "igraph")[[as.character(name)]]
    if (!missing(index)) {
      index <- as.igraph.vs(graph, index)
      myattr <- myattr[index]
    }
    myattr
  }
}
environment(vertex_attr_opt) <- asNamespace("igraph")