#
#  Plot the results of a clustering in whisker plots
############################################################
library(reshape2)
library(ggplot2)

data(iris)
df <- iris[,1:4]
fit <- kmeans(df, 3)

cluster.means <- aggregate(df, by=list(fit$cluster), FUN=mean)
names(cluster.means)[1] <- c("cluster")
melted.means <- melt(cluster.means, id="cluster", value.name = "mean")

cluster.devs <- aggregate(df, by=list(fit$cluster),FUN=sd)
names(cluster.devs)[1] <- c("cluster")
melted.devs <- melt(cluster.devs, id="cluster",  value.name = "var")

# set up a dataframe with means and variances of every cluster
cluster.profiles <- merge(melted.means, melted.devs, by=c("cluster", "variable"))
cluster.profiles$cluster <- as.factor(cluster.profiles$cluster)

points <- df
points$cluster <- fit$cluster
points <- melt(points, id='cluster')

p <- ggplot(points, aes(x=variable, y=value)) 
p <- p + geom_boxplot(aes(fill = factor(cluster)))
p <- p + geom_jitter(aes(color=factor(cluster)))
print(p)

# More possibilities
p <- ggplot(points, aes(x=variable, y=value)) 
p <- p + geom_boxplot(aes(fill = factor(cluster)))
print(p)


p <- ggplot(points, aes(x=variable, y=value)) 
p <- p + geom_boxplot(aes(fill = factor(cluster)))
p <- p + geom_jitter(aes(color=factor(cluster)))
print(p)

p <- ggplot(points, aes(x=variable, y=value, color=factor(cluster), fill=factor(cluster))) 
p <- p + geom_point(aes(color= factor(cluster), fill=factor(cluster)), position=position_jitterdodge(dodge.width=0.9))
p <- p + geom_boxplot(fill="white", position = position_dodge(width=0.9))
print(p)