cluster <- function(features, k){
    wss <- (nrow(features)-1)*sum(apply(features,2,var))
    for (i in 2:25){
      wss[i] <- sum(kmeans(features, i)$withinss)
    }
    plot(1:25, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
    fit <- kmeans(features, k)
    z <- fit$cluster
    
    # relabel cluster by size (to keep color consistency between executions)
    k.order <- order(table(z), decreasing=TRUE)
    z.temp <- z
    for(i in 1:max(z)){
      z.temp[z==k.order[i]] <- i
    }
    z <- z.temp
    
    return(z)
}

# Complementary
#library(mclust)
#BIC <-  mclustBIC(df.scaled)
#plot(BIC)
#fit2 <-Mclust(df.scaled)
#summary(fit2, parameters=TRUE)

plot.clusters <- function(features, clusters, sizes, colors){
  par(mfrow=c(1,1))
  pca <- princomp(features)
  
  # Colored PCA (base graphics)
  ######################################
  plot(pca$scores[,1], pca$scores[,2], 
       col  = colors, 
       cex  = sizes, 
       pch  = 19,
       xlab = "Dimension 1", ylab = "Dimension 2")
  text(pca$scores[,1], pca$scores[,2], 
       labels = rownames(features), 
       cex    = 0.7)
  title("Individual factor map (PCA)")
  dev.copy(png, paste0('2016-01-15-PCA.png'), width = 800, height = 800)
  dev.off()
  
  # Colored PCA (another library)
  ################################
  p1 <- ggbiplot(pca, 
                 obs.scale = 1, 
                 var.scale    = 1, 
                 groups       = factor(z), 
                 ellipse      = TRUE, 
                 circle       = TRUE,
                 #labels       = rownames(features),
                 labels.size  = 3.5,
                 varname.size = 4) 
  p1 <- p1 + scale_color_manual(values = alpha(cluster.colors,0.75))
  p1 <- p1 + theme_bw() +
        theme(text            = element_text(size = 15),
              aspect.ratio    = 1,
              legend.position = "none") +
        ggtitle("Individual factor map (PCA)")
  print(p1) 
  dev.copy(png, paste0('2016-01-15-PCA2.png'), width=800, height=800)
  dev.off()
  
  # Boxplots
  #########################
  # Clusters profile
  points <- as.data.frame(features)
  points$cluster <- factor(z)
  points <- melt(points, id='cluster')
  p <- ggplot(points, aes(x=variable, y=value)) + 
    scale_fill_manual(values = cluster.colors) + 
    geom_boxplot(aes(fill = cluster), position = position_dodge(width = 0.75))
  p <- p + theme_bw() +
    theme(text = element_text(size = 15),
          legend.key = element_blank()) +
    ggtitle("Clusters means and variances")
  print(p)
  dev.copy(png, paste0('2016-01-15-whiskers.png'), width=800)
  dev.off()
  
  # Colored scatter matrix
  #df.features$colors <- cluster.colors[z]
  #p1 <- ggpairs(data    = df.features,
  #              columns = 1:8,
  #              upper   = list(continuous = "points"),
  #              lower   = list(continuous = "points"),
  #              diag    = list(continuous = "density"),
  #              colour  = 'colors')
  #print(p1)
  
  
}
