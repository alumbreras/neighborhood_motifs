plot.whiskers_cluster_length <- function(df.posts, df.users, df.threads){
    
    # Prepare the data
    df.cluster_length <- merge(merge(df.posts, df.users), df.threads) %>% 
      mutate(cluster=factor(cluster)) %>%
      select(cluster, length)
    
    # Plot
    p <- ggplot(df.cluster_length, aes(x=cluster, y=length)) + 
      scale_fill_manual(values = c("white", cluster.colors)) + # no role -> white color
      geom_boxplot(aes(fill=cluster)) +
      theme_bw() +
      theme(text        = element_text(size = 15),
            legend.key  = element_blank()) +
      ggtitle("Participations vs threads length")
    print(p)
    
    # Save to file
    dev.copy(png, paste0('2016-01-15-whiskers_roles_vs_length.png'), width=800)
    dev.off()
}

plot.by_thread_cluster <- function(df.posts, df.users){
  
  df.threads <- plyr::count(df.posts, "thread")
  names(df.threads)[2] <- "length"
  
  df.participations <- merge(merge(df.posts, df.users), df.threads)
  df.participations$cluster <- factor(df.participations$cluster)
  
  by_thread_cluster <- acast(df.participations, thread~cluster)
  by_thread_cluster.perc <- as.data.frame(t(apply(by_thread_cluster, 1, function(x) x/sum(x))))
  
  
  # Add length and sort by length
  df.lengths <- plyr::count(df.participations, "thread")
  names(df.lengths) <- c('thread', 'length')
  by_thread_cluster.perc$thread <-rownames(by_thread_cluster.perc)
  by_thread_cluster.perc <- merge(by_thread_cluster.perc, df.lengths) # add lengths
  by_thread_cluster.perc <- by_thread_cluster.perc[order(by_thread_cluster.perc$length),] # sort by length
  by_thread_cluster.perc <- subset(by_thread_cluster.perc, select=-c(thread)) # drop again thread column
  by_thread_cluster.norm <- by_thread_cluster.perc
  by_thread_cluster.norm$length <-  by_thread_cluster.norm$length/max(by_thread_cluster.perc$length)
  
  # Plot
  heatmap(as.matrix(by_thread_cluster.norm), 
          Colv=NA, Rowv=NA, labRow=NA, ylab="Threads", xlab="Roles",
          main = "Participations by role")
  
  # Save to file
  dev.copy(png, paste0('2016-01-15-thread_role_composition.png'), width=600)
  dev.off()
}

by_thread_cluster_nfirst <- function(df.posts, nfirst=15){
  
  # Sort posts by their position in the thread
  df.posts <- df.posts[with(df.posts, order(thread, date)),]
  df.posts$rank <- sapply(1:nrow(df.posts), 
                          function(i) sum(df.posts[1:i, c('thread')]==df.posts$thread[i]))
  # Thread lengths
  df.threads <- plyr::count(df.posts, 'thread')
  names(df.threads)[2] <- "length"
  
  # Add lengths to dataframe so that we can filter by length later
  df.posts <- merge(df.posts, df.threads)
  
  # Posts before NFIRST and in threads longer than NFIRST
  mask <- df.posts$rank <= nfirst & df.posts$length>nfirst
  df.first <- df.posts[mask,]
  
  return(df.first)
}