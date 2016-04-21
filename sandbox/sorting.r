motifs <- c('alberto', 'elisa', 'david', 'bertrand', 'julien')

counts <- c('alberto', 'alberto', 'alberto', 'alberto',
             'elisa',
             'david', 'david', 'david',
             'bertrand', 'bertrand', 'bertrand','bertrand',
             'julien', 'julien')

idx <- order(table(counts)[motifs], decreasing=TRUE)

motifs <- motifs[idx]

match(motifs, idx)

#############"

set.seed(3)
x <- sample(100,1000, replace=TRUE)
idx <- order(table(x), decreasing=TRUE)
xx <- match(x, idx)
n <- table(xx)
n
all(n == cummin(n))