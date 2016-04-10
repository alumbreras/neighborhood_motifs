#t3_29yrey check logfile:  log8453.csv
#CATCHED ERROR in thread:  t3_29b97c

gp <- database.to.graph('t3_29yrey', con, database)$gp

par(mfrow=c(1,1))
g_ <- as.undirected(gp)
root <- which.min(V(gp)$date)
la = layout_as_tree(g_, mode='out', root=root)
plot(g_,
     layout = la,
     vertex.color=mypalette[V(g_)$color],
     vertex.size = 3,
     vertex.label = "" ,
     edge.arrow.size=0.6)