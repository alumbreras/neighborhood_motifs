par(mfrow=c(1,1))
gt <- make_tree(1000, mode="undirected")
plot(gt)
radius <- 1:100
plot(radius, sapply(radius, function(n) {ego_size(gt, n, nodes=64)}), pch=3)