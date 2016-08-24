par(mfrow=c(6,5))
lapply(1:30, function(i) {
  plot(graph.isocreate(4,i), vertex.label="", vertex.size=50, edge.arrow.size=0)
  title(i)
  })


layout(matrix(1:30, nrow=6, ncol=5, byrow = TRUE))
lapply(1:30, function(i) {
  plot(graph.isocreate(4,i), vertex.label="", vertex.size=50, edge.arrow.size=0)
  title(i)
})
