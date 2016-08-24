library(rbenchmark)

x <- sample(100000, 100000)
benchmark(as.numeric(names(sort(table(x), decreasing = TRUE))),
           names(sort(table(x), decreasing = TRUE)),
           sort(table(x), decreasing = TRUE),
           table(x),
          length(x))


mygraph <-barabasi.game(10000)
V(mygraph)$color <- 1:vcount(mygraph)

benchmark(V(mygraph)$color[10],
          V(mygraph)[10]$color,
          vertex_attr(mygraph, 'color', 10),
          replications=1000)


mygraph <-barabasi.game(5)
V(mygraph)$color <- 1:vcount(mygraph)
V(mygraph)$name <- c("one", "two", "three", "four", "five")
benchmark(V(mygraph)$color[5],
          V(mygraph)[5]$color,
          vertex_attr(mygraph, 'color', V(mygraph)[5]),
          vertex_attr(mygraph, 'color', 5),
          vertex_attr(mygraph, 'color', 'five'),
          replications=1000)