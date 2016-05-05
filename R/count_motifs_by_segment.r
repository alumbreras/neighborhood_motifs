library(data.table)
source('R/extract_from_db.r')
source('R/tree_changepoints.r')

tree.segments <- function(gp, breakpoints.v, breakpoints.h){
  # Return the segments (subgraphs) of g according to its changepoints
  cat("\nCOMPUTE SEGMENTS OF TREE\n")
  segs <- list()
  processed <- c()
  cat("breakpoints:", breakpoints.v)
  for (i in 1:vcount(gp)){
    # check if already in some segment
    if (V(gp)[i] %in% processed){ 
      cat('\n next')
      next
    }
    cat('\nnode: ', i)
    # detect neighbourhood
    gs <- neighborhood.temporal(gp, i, 10, breakpoints.v, breakpoints.h)
    cat('\nsegment: ', as.numeric(V(gp)[V(gs)$name]))
    
    # plot it 
    V(gp)[breakpoints.v]$color <- 'green'
    V(gp)[breakpoints.h]$color <- 'blue'
    V(gp)[V(gs)$name]$color <- "red"
    plot.tree(gp, labels = 'id')
    V(gp)[V(gs)$name]$color <- "white"
    
    segs[[length(segs)+1]] <- gs  
    # add them to processed list
    processed <- union(processed, V(gp)[V(gs)$name])
    #plot.tree.breakpoints(gp, breakpoints.v, breakpoints.h)
  }
  segs
}

count_motifs_by_usersegment <- function(threads, 
                                 database='reddit', 
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
      segs <- tree.segments(gp, breakpoints.v, breakpoints.h)
      
      # for every segment (local dynamic)
      for(i in 1:length(segs)){
        gs <- segs[[i]]
        users <- V(gs)$user 
        # for every user (not post) in the segment
        for(j in 1:length(users)){
          u <- users[i]
          post.id <- V(gs)$name[V(gs)$user == u] #first user post as reference
          V(gs)$color <- 1 # black by default
      
              #if(degree(eg, v=u, mode='out')==1){
          #  p.user.name <- neighbors(gs, u, mode='out')$user
          #  V(eg)[V(eg)$user==p.user.name]$color <- 2 # posts writen by parent of ego
          #}
      
          V(gs)$color[V(gs)$user == u] <- 4  # posts written by ego author
          tryCatch({V(gs)[root.post]$color <- 5}, error = function(e){}) # root (exception if root not in there)
          
          # Prune neighbourhood to reduce number of possible neighbourhoods
          #gs <- prune(gs)
          
          # If neighbourhood is too big, sometimes isomorphism check takes too long
          if(vcount(gs)>100){
            cat("\nSkipping neighbourhood isomorphism in thread ", threads[i], "-", vcount(gs))
            next
          }
          
          # See if it matches any seen motif
          ###################################
          is.new <- TRUE
          if(length(motifs)>0){
            for(motif.id in 1:length(motifs)){   
              gmotif <- motifs[[motif.id]] 
              
              # the vf2 does not like graphs of different size
              if(vcount(gs) != vcount(gmotif)){
                next
              }          
              
              if(length(unique(V(gs)$color)) != length(unique(V(gmotif)$color))){
                next
              } 
              
              if(all(table(V(gs)$color) != table(V(gmotif)$color))){
                next
              }
              
              if(is_isomorphic_to(gs, gmotif, method='vf2')){            
                is.new <- FALSE
                break
              }}}
          
          # If motif is new, add it to the list
          if(is.new){
            motif.id <- length(motifs)+1
            motifs[[motif.id]] <- gs
          }
          
          posts.motifs <- rbindlist(list(posts.motifs, 
                                         data.frame(user=u, postid=post.id, motif=motif.id)))
        } # end users
      } # end segments
  } # end threads
  
  posts.motifs <- data.frame(posts.motifs)
  
  if(nrow(posts.motifs)==0){
    cat("No neighbourhoods in this chunk")
    return(list(participation.motifs = NA,
                motifs = NA))
  }
  
  names(posts.motifs) <- c('postid', 'motif')
  
  # Sort by frequency (and relabel: 1 for the most frequent and so forth)
  idx <- order(table(posts.motifs$motif), decreasing = TRUE)
  posts.motifs$motif <- match(posts.motifs$motif, idx)
  motifs <- motifs[idx]
  
  cat("chunk.id: ", chunk.id, " completed!!")
  return(list(participation.motifs = posts.motifs,
              motifs = motifs))
}