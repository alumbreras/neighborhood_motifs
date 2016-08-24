options(digits=7)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source('R/pruning.R')


# Defaults
library(igraph)
igraph_options(vertex.size=10, edge.arrow.size=0.5)