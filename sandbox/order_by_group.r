# For each Species, change Sepal.Length by their order in the group
library(dplyr)
library(plyr)
data(iris)
by_species <- iris %>% arrange(Species, Sepal.Length) %>% group_by(Species) %>% mutate(rank=row_number())
# Error

detach("package:plyr", unload=TRUE)
by_species <- iris %>% arrange(Species, Sepal.Length) %>% group_by(Species) %>% mutate(rank=row_number())
by_species %>% filter(rank <= 3)


# With Base R
iris <- iris[with(iris, order(Species, Sepal.Length)), ]

# RUN A ROW COUNT FOR RANK BY SPECIES GROUP
iris$rank <- sapply(1:nrow(iris), 
                    function(i) sum(iris[1:i, c('Species')]==iris$Species[i]))

# FILTER DATA FRAME BY TOP 3
iris <- iris[iris$rank <= 3,]
