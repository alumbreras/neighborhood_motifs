library(igraph)
library(R.utils)



foo1 <- function() {
  print("Tic");
  for (kk in 1:100) {
    print(kk);
    Sys.sleep(0.3);
  }
  print("Tac");
}

withTimeout(foo1(), timeout = 1, onTimeout = "warning")

foo2 <- function() {
  print("Tic");
  N <- 10000
  g <- erdos.renyi.game(N, 0.75)
  V(g)$color <- sample(5,N, replace=TRUE)
  is_isomorphic_to(g, g, method='vf2')
  print("Tac");
}

withTimeout(foo2(), timeout = 1, onTimeout = "warning")

