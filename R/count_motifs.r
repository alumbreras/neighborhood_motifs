library(RSQLite)
library(data.table)
source('R/extract_from_db.r')
source('R/tree_changepoints.r')


prune <- function(motif){
  # Arguments:
  #   motif: a tree graph
  # Returns:
  #   a pruned motif
  # 4 colors: 1(black) 2(red) 4(orange)
  # we can only remove black nodes
  # there can be only two black nodes together
  # delete 
  parents <- which(degree(motif, mode='in')>2)
  to.delete <- c()
  if(length(parents)==0){
    return(motif)
  }
  # If more than two siblings with no children, keep only the first two
  # (the most ancient)
  for (p in 1:length(parents)){
    siblings <- neighbors(motif, parents[p], mode='in')
    siblings <- siblings[order(V(motif)$date[siblings])]
    siblings.indegree <- degree(motif, v=siblings, mode='in')
    
    if(length(siblings)<3){
      next
    }
    x <- siblings$color
    
    counts <- 1
    delete <- rep(FALSE, length(x))
    for(i in 2:length(x)){
      if(x[i] != x[i-1]){
        counts <- 1
      }
      else if(siblings.indegree[i]>0){
        counts <- 1
      }
      else{
        counts <- counts + 1
      }
      if(counts>2){
        delete[i] <- TRUE
      }
    }
    to.delete <- union(to.delete, siblings[delete])
  }
  motif <- delete.vertices(motif, to.delete)
  motif
}

count_motifs_by_post <- function(threads, 
                                 database='reddit', 
                                 neighbourhood=c('order', 'time'),
                                 rad=2,
                                 max.neighbors=4){
  
  con <- dbConnect(dbDriver("SQLite"), dbname = paste0("./data/", database, ".db"))

    # For every thread, recontruct its graph. Then count 
  # if neighborhood at distance 1 and binary tree, max nodes is 4
  # if neighborhood at distance 1 and binary tree, max nodes is 10
  # 4   10   22   30   46   62   93  123...
  motifs <- list()
  posts.id <- vector()
  posts.motifs.id <- vector()
  plotted.trees <- 1
  
  id.process <- Sys.getpid()
  filelog <- paste0("log", id.process, ".csv")
  
  
  nthreads <- length(threads)
  posts.motifs <- data.frame()
  for(i in 1:nthreads){
    tryCatch({
      
      write.table(threads[i], file="tried_threads.csv", row.names=FALSE, col.names=FALSE, append=TRUE)
      cat(threads[i], file=filelog, sep=",", append=TRUE)
      
      #Extract the egos of every post
      g <- database.to.graph(threads[i], con, database)
      gp <- g$gp
      cat('graph', file=filelog, sep=",", append=TRUE)
      
      size <- vcount(gp)
      cat('\n', i, '/', nthreads, '/', threads[i], ' size: ', size)
      if(size>1000){
        cat("\n WARNING: skipping thread of size ", size)
        next 
      }
      
      # cast dates to numeric
      dates <- as.numeric(V(gp)$date)
      gp <- remove.vertex.attribute(gp, "date")
      V(gp)$date <- dates
      
      root.post <- V(gp)[which(degree(gp, mode='out')==0)]$name
      cat('root', file=filelog, sep=",", append=TRUE)
      
      if(neighbourhood=='time'){
        breakpoints <- changepoints.cp(gp, vertical=T, horizontal=T)
        breakpoints.h <- breakpoints$breakpoints.h
        breakpoints.v <- breakpoints$breakpoints.v
        breakpoints.vh <- breakpoints$breakpoints.vh
        
        V(gp)$bp.v <- FALSE
        V(gp)$bp.h <- FALSE
        V(gp)$bp.vh <- FALSE
        V(gp)[breakpoints.v]$bp.v <- TRUE
        V(gp)[breakpoints.h]$bp.h <- TRUE
        V(gp)[breakpoints.vh]$bp.vh <- TRUE
        cat('bp', file=filelog, sep=",", append=TRUE)
      }
      
      for(j in 1:vcount(gp)){
        user.name <- V(gp)[j]$user
        post.id <- V(gp)[j]$name
        
        # Detect the neighborhood with the prefered method
        if(neighbourhood=='order'){
          eg <- neighborhood.temporal.order(gp, j, rad, max.neighbors)
        }
        if(neighbourhood=='time'){
          eg <- neighborhood.temporal(gp, j, 2, breakpoints.v, breakpoints.h)
        }
        
        # See if it matches any seen motif
        u <- V(eg)[V(eg)$name==post.id] # identify the ego vertex
        uu <- V(eg)[V(eg)$user==user.name] # posts writen by ego author
        
        # note the <= in case two siblings have the same date
        V(eg)$color[V(eg)$date<=V(eg)[u]$date] <- 1 
        V(eg)$color[V(eg)$date>V(eg)[u]$date] <- 2 
        if(degree(eg, v=u, mode='out')==1){
          p.user.name <- neighbors(eg, u, mode='out')$user
          V(eg)[V(eg)$user==p.user.name]$color <- 3 # posts writen by parent of ego
        }
        V(eg)[uu]$color <- 4
        V(eg)[u]$color <- 5
        V(eg)[V(eg)$name==root.post]$color <- 6
    
        # Prune neighbourhood to reduce number of possible neighbourhoods
        eg <- prune(eg)
        
        is.new <- TRUE
        if(length(motifs)>0){
          for(motif.id in 1:length(motifs)){   
            gmotif <- motifs[[motif.id]] 
            
            # the vf2 does not like graphs of different size
            if(vcount(eg) != vcount(gmotif)){
              next
            }          
            
            if(is_isomorphic_to(eg, gmotif, method='vf2')){            
              is.new <- FALSE
              break
            }}}
        
        # If motif is new, add it to the list
        if(is.new){
          motif.id <- length(motifs)+1
          motifs[[motif.id]] <- eg
        }
        
        posts.motifs <- rbindlist(list(posts.motifs, 
                                       data.frame(postid=post.id,motif=motif.id)))
      } # end egos
      cat('done', file=filelog, sep="\n", append=TRUE)
      # keep track of threads that gave no problems
      write.table(threads[i], file="processed_threads.csv", row.names=FALSE, col.names=FALSE, append=TRUE)
    }, 
    error=function(e){
      e
      cat('\n CATCHED ERROR in thread: ', threads[i], "check logfile: ", filelog)
      cat(paste('\n CATCHED ERROR in thread: ', threads[i]), 
          file=paste0('exception_',filelog), sep="\n", append=TRUE)
      stop("ERROR")
    }
    )
  } # end threads
  
  posts.motifs <- data.frame(posts.motifs)
  
  if(nrow(posts.motifs)==0){
    return(list(posts.motifs = NA,
                motifs = NA))
  }
  
  names(posts.motifs) <- c('postid', 'motif')
  
  # Sort by frequency (and relabel: 1 for the most frequent and so forth)
  idx <- order(table(posts.motifs$motif), decreasing = TRUE)
  posts.motifs$motif <- match(posts.motifs$motif, idx)
  motifs <- motifs[idx]
  
  return(list(posts.motifs = posts.motifs,
              motifs = motifs))
}


merge.motif.counts.pair <- function(res1, res2){
  # Merge two dataframes obtained by count_motifs_by_post with some differnt motifs

  df.posts.motifs.1 <- res1$posts.motifs
  df.posts.motifs.2 <- res2$posts.motifs
  #plot.motif.counts(res1)
  #plot.motif.counts(res2)
  motifs.1 <- res1$motifs
  motifs.2 <- res2$motifs
  duplicated_motifs <- c()
  motifs.1.new <- list()
  mapping <- vector()
  for(i in 1:length(motifs.2)){
    dupl <- FALSE
    for(j in 1:length(motifs.1)){
      if(vcount(motifs.2[[i]]) != vcount(motifs.1[[j]])){
        next
      }  
      if(is_isomorphic_to(motifs.2[[i]], motifs.1[[j]], method='vf2')){
        dupl <- TRUE
        cat("\n", i, " -> ", j)
        mapping[i] <- j 
        break
      }
    }
    # if motif.2 not found among motifs.1, give him its own position in 1
    if(!dupl){
      new.pos <- length(motifs.1.new)+1
      cat("\nnew ", i, " -> ", new.pos)
      motifs.1.new[[new.pos]] <- motifs.2[[i]]
      mapping[i] <- length(motifs.1) + new.pos
    }
  }
  
  motifs.merged <- c(motifs.1, motifs.1.new)
  df.posts.motifs.2$motif <- mapping[df.posts.motifs.2$motif]  

  df.posts.motifs.merged <- rbind(df.posts.motifs.1, df.posts.motifs.2)
  res <- list(posts.motifs = df.posts.motifs.merged,
              motifs = motifs.merged)
  
  return(res)
  
}

# merge results
merge.motif.counts <- function(df.list){
  res <- df.list[[1]]
  for (i in 2:length(df.list)){
    res <- merge.motif.counts.pair(res, df.list[[i]])
  }
  
  # Sort by frequency (and relabel: 1 for the most frequent and so forth)
  posts.motifs  <- res$posts.motifs
  motifs <- res$motifs
  idx <- order(table(posts.motifs$motif), decreasing = TRUE)
  posts.motifs$motif <- match(posts.motifs$motif, idx)
  motifs <- motifs[idx]
  
  return(list(posts.motifs = posts.motifs,
              motifs = motifs))
}

plot.motif.counts <- function(res){
  df.post.motif  <- res$posts.motifs
  motifs <- res$motifs
  ####################################################
  # Plot found neighborhoods
  ####################################################
  #mypalette <- c("black", "red", "white", 'orange')
  mypalette <- c("grey", "black", "yellow", "orange", "red", "white")
  par(mfrow=c(3,5))
  for(i in 1:length(motifs)){
    gmotif <- as.undirected(motifs[[i]])
    la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
    plot(gmotif,
         layout = la,
         vertex.color=mypalette[V(motifs[[i]])$color],
         vertex.label = "",
         edge.arrow.size=0.6)
    #title(1, sub=sum(df.post.motif$motif==i))
    cat(sum(df.post.motif$motif==i))
    title(paste(i),sub=sum(df.post.motif$motif==i))  
  }
}

