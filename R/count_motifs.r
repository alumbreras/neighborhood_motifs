library(RSQLite)
library(data.table)
library(digest)
#library(R.utils)

source('R/extract_from_db.r')
source('R/tree_changepoints.r')
source('R/pruning.r')


count_motifs_by_post <- function(threads, 
                                 database='reddit', 
                                 neighbourhood=c('struct', 'order', 'time'),
                                 rad=2,
                                 max.neighbors=4,
                                 chunk.id=0){
  
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

      #Extract the egos of every post
      g <- database.to.graph(threads[i], con, database)
      gp <- g$gp

      size <- vcount(gp)
      cat('\n', i, '/', nthreads, '/', threads[i], ' size: ', size, ' chunk.id: ', chunk.id )
      if(size>1000){
        cat("\n WARNING: skipping thread of size ", size)
        next 
      }
      
      # cast dates to numeric
      dates <- as.numeric(V(gp)$date)
      gp <- remove.vertex.attribute(gp, "date")
      V(gp)$date <- dates
      
      root.post <- V(gp)[which(degree(gp, mode='out')==0)]$name

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
      }
      
      for(j in 1:vcount(gp)){
        if(j %% 25==1){
          cat('\n', i, '/', nthreads, '/', threads[i], ' size: ', size, ' chunk.id: ', chunk.id, " post: ", j )
          
        }
        
        gp.ego <- V(gp)[j]
        user.name <- gp.ego$user
        post.id <- gp.ego$name
        
        # Detect the neighborhood with the prefered method
        if(neighbourhood=='order'){
          eg <- neighborhood.temporal.order(gp, j, rad, max.neighbors)
        }
        if(neighbourhood=='time'){
          eg <- neighborhood.temporal(gp, j, 2, breakpoints.v, breakpoints.h)
        }
        if(neighbourhood=='struct'){
          eg <- make_ego_graph(gp, rad, nodes=j)[[1]]
        }

        u <- V(eg)[V(eg)$user==user.name] # posts writen by ego author
        
        #mypalette <- c("black", "yellow", "orange", "red", "white")
        
        V(eg)$color <- 1
        if(degree(eg, v=u, mode='out')==1){
          p.user.name <- neighbors(eg, u, mode='out')$user
          V(eg)[V(eg)$user==p.user.name]$color <- 2 # posts writen by parent of ego
        }
        V(eg)[u]$color <- 4
        tryCatch({V(eg)[root.post]$color <- 5}, error = function(e){}) # exception if root not in there
        
        # Prune neighbourhood to reduce number of possible neighbourhoods
        eg <- prune(eg)

        # If neighbourhood is too big, sometimes isomorphism check takes too long
        if(vcount(eg)>10){
          #cat("\nSkipping neighbourhood isomorphism in thread ", threads[i])
          next
        }

        # See if it matches any seen motif
        ###################################
        is.new <- TRUE
        if(length(motifs)>0){
          for(motif.id in 1:length(motifs)){   
            gmotif <- motifs[[motif.id]] 
            
            # the vf2 does not like graphs of different size
            if(vcount(eg) != vcount(gmotif)){
              next
            }          

            if(length(unique(V(eg)$color)) != length(unique(V(gmotif)$color))){
              next
            } 
            
            if(all(table(V(eg)$color) != table(V(gmotif)$color))){
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
                                       data.frame(postid = post.id, 
                                                  motif = motif.id,
                                                  conversation = digest(V(eg)$name)))) # hash to identify conversation
      } # end egos
      #cat('done', file=filelog, sep="\n", append=TRUE)
      # keep track of threads that gave no problems
      write.table(threads[i], file="processed_threads.csv", row.names=FALSE, col.names=FALSE, append=TRUE)
    }, 
    error=function(e){
      print(e)
      cat('\n CATCHED ERROR in thread: ', threads[i], "check logfile: ", filelog)
      cat(paste('\n CATCHED ERROR in thread: ', threads[i]), 
          file=paste0('exception_',filelog), sep="\n", append=TRUE)
      stop("ERROR")
    }
    )
  } # end threads
  
  posts.motifs <- data.frame(posts.motifs)
  
  if(nrow(posts.motifs)==0){
    cat("No neighbourhoods in this chunk")
    return(list(posts.motifs = NA,
                motifs = NA))
  }
  
  names(posts.motifs) <- c('postid', 'motif', 'conversation')
  
  # Sort by frequency (and relabel: 1 for the most frequent and so forth)
  idx <- order(table(posts.motifs$motif), decreasing = TRUE)
  posts.motifs$motif <- match(posts.motifs$motif, idx)
  motifs <- motifs[idx]
  
  cat("chunk.id: ", chunk.id, " completed!!")
  return(list(posts.motifs = posts.motifs,
              motifs = motifs))
}


merge.motif.counts.pair <- function(res1, res2){
  # Merge two dataframes obtained by count_motifs_by_post with some differnt motifs
  
  df.posts.motifs.1 <- res1$posts.motifs
  df.posts.motifs.2 <- res2$posts.motifs
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
  
  # check this is sorted
  n <- table(posts.motifs$motif[idx])
  n <- table(posts.motifs$motif)
  if(! all(n == cummin(n))){
    cat('Something wrong in sorting motifs!')
  }
  
  
  return(list(posts.motifs = posts.motifs,
              motifs = motifs))
}

plot.motif.counts <- function(res){
  df.post.motif  <- res$posts.motifs
  motifs <- res$motifs
  ####################################################
  # Plot found neighborhoods
  ####################################################
  mypalette <- c("black", "yellow", "orange", "red", "white")
  par(mfrow=c(3,5))
  for(i in 1:length(motifs)){
    gmotif <- as.undirected(motifs[[i]])
    la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
    plot(gmotif,
         layout = la,
         vertex.color=mypalette[V(motifs[[i]])$color],
         vertex.label = "",
         edge.arrow.size=0.6)
    cat(sum(df.post.motif$motif==i))
    title(paste(i),sub=sum(df.post.motif$motif==i))  
  }
}

