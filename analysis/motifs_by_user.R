library(igraph)
library(dplyr)
library(data.table)
library(parallel)
library(doParallel)
library(foreach)
library(digest)
library(ggplot2)
library(gplots)
library(reshape2)
library(igraph)
library(RColorBrewer)
library(profvis)
source('R/tree_changepoints.r')
source('R/neighbourhoods.R')
#source('R/igraph_optimised.R')

options(digits=7)
#options(error = recover)
options(error = NULL)
if(detectCores() > 20)
{
  ncores <- 20 # mediamining server
} else {
  ncores <- 6 # local computer
}



subforums <- c("MachineLearning",
               "TwoXChromosomes",
               "france",  
               "podemos",
               "gameofthrones")

#subforums <- c("podemos",'MachineLearning')
#subforums <- c("MachineLearning")

# Filter short threads and count user threads
# then leave only threads with some active user that 
# has participated in at least 100 threads
#MIN.THREADS_BY_USER <- 50
#MAX.THREADS_BY_USER <- 100000 # just for tests

# PARAMETERS OF THE EXPERIMENT
################################################################
# ok, and 2h
NPOSTS_BY_USER <- 250 # How many posts should we sample per user
NTOP.USERS <- 1000
method = 'radius'
rad <- 2
file.output = 'data/res.motifs_by_user_radius_r2_u1000_p250.rda'

# Error in { : task 5 failed - "Unknown vertex selected"
NPOSTS_BY_USER <- 100 # How many posts should we sample per user
NTOP.USERS <- 50
method = 'order'

NPOSTS_BY_USER <- 250 # How many posts should we sample per user
NTOP.USERS <- 1000
method = 'radius'
rad <- 2
file.output = 'data/res.motifs_by_user_radius_r2_u1000_p250.rda'

# < 1h
NPOSTS_BY_USER <- 150 # How many posts should we sample per user
NTOP.USERS <- 100
method = 'radius'
rad <- 2

# < 1h
NPOSTS_BY_USER <- 150 # How many posts should we sample per user
NTOP.USERS <- 100
method = 'radius'
rad <- 1

# < 1h
NPOSTS_BY_USER <- 250 # How many posts should we sample per user
NTOP.USERS <- 100
method = 'order'
rad <- 3


# ~8h
NPOSTS_BY_USER <- 50 # How many posts should we sample per user
NTOP.USERS <- 100
method =  'time'
rad <- 2
################################################################

model = paste0('motifs_', method, '_r', rad, '_u', NTOP.USERS, '_p', NPOSTS_BY_USER)
file.output = paste0('data/res.', model, '.rda')

deleted <- c('AutoModerator', '[deleted]')
data.list <- list()
count <- 1
i <- 1




duplicatedN <- function(x,n=2){
  DT <- data.table(A=x)
  DT[,dup:=1:.N > n,by=A]
  return(DT$dup)
}

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
  # If more than two siblings with no children, keep only the two
  for (p in 1:length(parents)){
    siblings <- neighbors(motif, parents[p], mode='in')
    siblings.indegree <- degree(motif, v=siblings, mode='in')
    
    if(length(siblings)<3){
      next
    }
    x <- siblings$color
    if (length(unique(x))>1){ # if many colors, allow one of each
      delete <- duplicatedN(x,1) & siblings.indegree == 0
      to.delete <- union(to.delete, siblings[delete])
    } else{# if all children have the same color, allow two children
      delete <- duplicatedN(x,2) & siblings.indegree == 0
      to.delete <- union(to.delete, siblings[delete])
    }
  }
  motif <- delete.vertices(motif, to.delete)
  motif
}

# Pruning to be applied over the whole tree 
# and before the order-based neighbourhoods
prune.preorder <- function(motif, j){
  # Arguments:
  #   motif: a tree graph
  # Returns:
  #   a pruned motif
  # 4 colors: 1(black) 2(red) 4(orange)
  # we can only remove black nodes
  # there can be only two black nodes together
  # delete 
  parents <- which(degree(motif, mode='in')>2)
  
  dates <- vertex_attr(motif, 'date')
  order.idx <- order(abs(dates[j]-dates))
  rank.idx <- rank(abs(dates[j]-dates), ties.method='first')
  
  to.delete <- c()
  if(length(parents)==0){
    return(motif)
  }
  # If more than two siblings with no children, keep only the two
  for (p in 1:length(parents)){
    # get siblings sorted by distance to the ego
    siblings <- neighbors(motif, parents[p], mode='in')
    siblings.idx <- order(rank.idx[siblings]) # sorted index
    siblings <- siblings[siblings.idx]
    siblings.indegree <- degree(motif, v=siblings, mode='in')
    
    if(length(siblings)<3){
      next
    }
    x <- siblings$color
    if (length(unique(x))>1){ # if many colors, allow one of each
      delete <- duplicatedN(x,1) & siblings.indegree == 0
      to.delete <- union(to.delete, siblings[delete])
    } else{# if all children have the same color, allow two children
      delete <- duplicatedN(x,2) & siblings.indegree == 0
      to.delete <- union(to.delete, siblings[delete])
    }
  }
  motif <- delete.vertices(motif, to.delete)
  motif
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
      if(isomorphic(motifs.2[[i]], motifs.1[[j]], method='vf2')){
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

plot.motifs.dict <- function(motifs.dict){
  par(mfrow=c(4,5))
  mypalette <- c("black", "red", "white", 'grey')
  for(i in 1:length(motifs.dict)){
    motif <- motifs.dict[[i]]
    root <- which.min(degree(motif, mode='out'))
    gmotif <- as.undirected(motif)
    #la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
    la = layout_as_tree(gmotif, mode='out', root=root)
    
    plot(gmotif,
         layout = la,
         #vertex.color=mypalette[V(motif)$color],
         vertex.color=mypalette[V(motif)$color],
         vertex.label = "",
         vertex.size=40,
         edge.arrow.size=0.6)
    title(i) 
  }
}
 
# Plot found neighborhoods
# If printed in file, we assume all motifs go in the same page
plot.motif.counts <- function(data, nrow=6, ncol=6, 
                              nmotifs = length(data$motifs), 
                              filename=NA,
                              width = 210,
                              height = 297,
                              res = 300) {
  df.post.motif  <- data$posts.motifs
  motifs <- data$motifs
  mypalette <- c("black", "red", "white", 'grey')
  
  # grid layout
  if(is.na(filename)) {
    par(mfrow=c(nrow, ncol)) 
  } else{
    nmotifs <- min(nrow*ncol, length(motifs))
    png(file = filename, width = width, height = height, units = 'mm', res = res)
    par(mar=c(2, 2, 1, 2) + 0.1)
    layout(matrix(1:(nrow*ncol), nrow = nrow, ncol = ncol, byrow = TRUE))
  }

  for(i in 1:nmotifs){
    root <- which.min(degree(motifs[[i]], mode='out'))
    gmotif <- as.undirected(motifs[[i]])
    #la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
    la = layout_as_tree(gmotif, mode='out', root=root)
    
    plot(gmotif,
         layout = la,
         vertex.color=mypalette[V(motifs[[i]])$color],
         vertex.label = "",
         vertex.size=40,
         edge.arrow.size=0.6)
    cat(" ", sum(df.post.motif$motif==i))
    #title(paste(i),sub=sum(df.post.motif$motif==i))
    title(i) 
  }
  # disable the device if it is a file 
  if(! is.na(filename)) dev.off()
}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
i <- 1
for (i in 1:length(subforums)){

  # Load the entire dataframe of posts
  subforum <- subforums[i]
  load(paste0('data/df.posts.', subforum, '.rda'))
  cat('\nsubforum:', subforum )
  
  # Add Dates and parent users
  parent.users <- df.posts[match(df.posts$parent, df.posts$postid),]$user
  df.posts <- df.posts %>% mutate(date=as.numeric(date)) %>% 
                           mutate(date=date - min(date)) %>%
                           mutate(parent.user=parent.users)
  
  # Threshold for short threads 
  # Probably 0 is good choice: allows to detect e.g. question posters that fail
  df.posts <- df.posts %>% 
              group_by(thread) %>% mutate(size = n()) %>% ungroup %>% 
              filter(size>=0)
  
  # Select users with more post (ignore deleted posts)
  df.users <- df.posts %>% 
              group_by(user) %>% 
                summarise(nthreads = length(unique(thread)), 
                        nposts = n()) %>%
                filter(! user %in% deleted) %>%
                arrange(desc(nposts), desc(nthreads), user) %>%
                mutate(user.rank = match(user, unique(user))) %>% 
              ungroup %>%
              filter(user.rank<=NTOP.USERS)
  
  cat('\n Less active user from the sample:')
  print(tail(df.users,1))
  if(min(df.users$nposts)<10) stop("Some users have too few posts")

  
  # Posts written by those users. Select N from each user
  df.users.posts <- df.posts %>% 
                    filter(user %in% df.users$user) %>%
                    group_by(user) %>%
                    filter(row_number() %in% as.integer(seq(from=1, to=n(), 
                                                            length.out = min(n(), NPOSTS_BY_USER)))) %>%
                    ungroup
  
  # All posts from threads with selected user
  df.posts <- df.posts %>% 
              filter(thread %in% unique(df.users.posts$thread)) %>%
              as.data.frame
  
  # In this full dataframe, mark posts written from selected users
  df.posts$active.post <- ifelse(df.posts$postid %in% df.users.posts$postid, TRUE, FALSE)
  
  all.threads <- unique(df.posts$thread)
  cat('\n Threads: ', length(all.threads))
  
  # Create the total graph (it is a forest of trees)
  df.edges <- df.posts %>% filter(!is.na(parent.user)) %>% select(postid, parent)
  df.vertices <- df.posts %>% select(postid, user, active.post, date)
  gforest <- graph.data.frame(df.edges, vertices = df.vertices)
  

  
  # Save to for gephi 
  # write.gexf(df.vertices, df.edges, output='gforest.gexf')
  
  # Search for conversation motifs thread by thread
  #################################################
  # split to parallelise
  chunks <- split(all.threads, ceiling(seq_along(all.threads)/(length(all.threads)/ncores)))
  cat("\n# chunks:", length(chunks))

  ################################
  # Parallelise
  ################################
  cl = makeCluster(ncores, outfile="",  port=11439)
  registerDoParallel(cl)
  
  # all trees in separated graphs
  gtrees <- decompose.graph(gforest)
  clusterEvalQ(cl, library('igraph'))
  gtrees.roots <- unlist(parLapply(cl, gtrees, function(g) names(which(degree(g, mode='out')==0))))
  
  
  
  ch <- 1 
  res.parallel <- foreach(ch = 1:length(chunks), 
                          .packages=c('igraph', 'dplyr', 'foreach', 
                                      'data.table', 'digest', 'changepoint')) %dopar%
  #for (ch in 1:length(chunks))
  {
    
    #prof <- profvis({
      
    chunk <- chunks[[ch]]
    
    # result containers
    motifs.dict <- list()
    df.chunk <- data.frame()
    # for printing
    th.counts <- 0 
    post.counts <- 0 
    total <- length(chunk)
    th <- chunk[1]
    for (th in chunk) 
    {
      url.thread <- paste0('https://www.reddit.com/r/', subforum, '/comments/', 
                            substr(th, 4,10)) # for info
      #th <- 't3_2z60au'
      th.counts <- th.counts + 1
      cat('\n', subforum, th.counts, ' / ', total , '\t--chunk--', ch) 
          #"# iso mean pos:", mean(df.chunk$motif.found.in, na.rm=TRUE),
          #"# iso mean pos naive:", mean(df.chunk$motif.found.in.naive, na.rm=TRUE))
      
      # the thread is the subtree of node th
      #gtree <- make_ego_graph(gforest, 1000, nodes = th, mode = 'in')[[1]] # without optimisation
      gtree <- gtrees[[which(gtrees.roots==th)]] # optimised x300 
      tree.size <- vcount(gtree)
      
      # Debug:
      #tree.motifs <- list()
      
      if(method=='time'){
        breakpoints <- changepoints.cp(gtree, vertical=T, horizontal=T)
        breakpoints.h <- breakpoints$breakpoints.h
        breakpoints.v <- breakpoints$breakpoints.v
        breakpoints.vh <- breakpoints$breakpoints.vh
        
        V(gtree)$bp.v <- FALSE
        V(gtree)$bp.h <- FALSE
        V(gtree)$bp.vh <- FALSE
        V(gtree)[breakpoints.v]$bp.v <- TRUE
        V(gtree)[breakpoints.h]$bp.h <- TRUE
        V(gtree)[breakpoints.vh]$bp.vh <- TRUE
      }

      # Extract motif around each post
      #post <- which(V(gtree)$active.post == TRUE)[1]
      for(post in V(gtree)[active.post]) # only posts of active users
      {
        ego.user <- vertex_attr(gtree, 'user', post) # optimisation for V(gtree)[post]$user
        ego.post <- vertex_attr(gtree, 'name', post) # optimisation for V(gtree)[post]$name

        # This is the core: the extraction of the neighbourhood by different methods
        # Extract neighbourhood 
        # radius
        # gego <- make_ego_graph(gtree, 1, nodes = post, mode = 'all')[[1]]
        # Detect the neighborhood with the prefered method
        if(method=='radius'){
          gego <- make_ego_graph(gtree, rad, nodes=post, mode = 'all')[[1]]
        }
        
        if(method=='order'){
          
          # the tree, but ego-centered
          gego <- gtree
          
            # We prune first so that children don't monopolise the positions
            # even if they are closest in time.
            # two children are informative enough. Then leave place for
            # parents or other (potentially) interesting neighbours
            
            # Color/label it
            gego.users <-  vertex_attr(gego, 'user')
            gego.names <-  vertex_attr(gego, 'name')
            gego.colors <- rep(1, length(gego.users)) # default
            gego.colors[gego.users == ego.user] <- 2 
            gego.colors[gego.names == ego.post] <- 0 
            V(gego)$color <- gego.colors
          
            # Prune only if small. Otherwise it is too slow
            if(tree.size < 100){
              #### Prune it and final colours #########################
              gego.pruned <- try({prune.preorder(gego, post)}, function(e) stop(e, 'Error pruning'))
              ########################################################
            } 
            else {
              gego.pruned <- gego
            }
            
            # finish colouring
            # root colour at the end so that it is not overwritten
            V(gego.pruned)[ego.post]$color <-  2 # all ego posts are now colored the same
            ego.pruned.names <- vertex_attr(gego.pruned, "name") 
            if (th %in% ego.pruned.names){ 
              V(gego.pruned)[th]$color <- 3 # root
              if (vertex_attr(gego.pruned, "user", th) == ego.user){ # V(gego.pruned)[th]$user
                V(gego.pruned)[th]$color <- 4 # ego root
              }
            }
          
          # store some stats for reporting
          gego.size <- vcount(gego)
          gego.pruned.size <- vcount(gego.pruned)
          
          #Get pointer to the ego.post in the pruned tree,
          # and extract the neighbourhood as usual
          post.new <- V(gego.pruned)[name==ego.post]
          gego <- tryCatch({neighborhood.order(gego.pruned, post.new, rad)}, 
                           error = function(e) stop(e, th))
          V(gego)$thread <- url.thread # for info
          
          # Debug
          # tree.motifs[[length(tree.motifs)+1]] <- gego
          
          # Debug:
          # if motif is some of this, plot it
          #if(vcount(gego) == vcount(sospechoso1)){
          #  if(isomorphic(gego, sospechoso1, 'vf2')){
          #    cat("Sospechoso en:\n")
          #    cat(paste0('https://www.reddit.com/r/', subforum, '/comments/',substr(th, 4,10)))
          #  }
          #}
          
        }
        
  
        if(method=='time'){
          gego <- tryCatch({neighborhood.temporal(gtree, ego.post, rad, breakpoints.v, breakpoints.h)},
                           error = function(e) stop(e, th, "neighbourhood.temporal"))
        } 
        
        if (!is_igraph(gego)) {
          stop("gego: Not a graph object")
        }
        
        
        if(method!='order'){
            # Color/label it
            gego.users <-  vertex_attr(gego, 'user') # optimised V(gego)$user
            gego.names <-  vertex_attr(gego, 'name') # optimised V(gego)$name
            gego.colors <- rep(1, length(gego.users)) # default
            gego.colors[gego.users == ego.user] <- 2 # ego posts
            gego.colors[gego.names == ego.post] <- 0 # ego (just to tell prune() who is the real ego)
            V(gego)$color <- gego.colors
            
            
            # slower?
            #V(gego)$color <- 1 #"other"
            #V(gego)[user == ego.user]$color <- 2 # other ego posts
            #V(gego)[ego.post]$color <-  0 # ego (just to tell prune() who is the real ego)
    
            # Prune it and final colours
            gego.pruned <- try({prune(gego)}, function(e) stop(e, 'Error pruning'))
            # finish colouring
            # root colour at the end so that it is not overwritten
            V(gego.pruned)[ego.post]$color <-  2 # all ego posts are now colored the same
            
            ego.pruned.names <- vertex_attr(gego.pruned, "name") # optimised V(gego.pruned)$name
            if (th %in% ego.pruned.names){ 
              V(gego.pruned)[th]$color <- 3 # root
              if (vertex_attr(gego.pruned, "user", th) == ego.user){ # V(gego.pruned)[th]$user
                V(gego.pruned)[th]$color <- 4 # ego root
              }
            }        
            # store some stats for reporting
            gego.size <- vcount(gego)
            gego.pruned.size <- vcount(gego.pruned)
            
            # if pruned graph is still too big, skip it (is_isomorphic_to takes too long)
            if(gego.pruned.size>10){
              # store NA result and skip
              df.chunk <- rbindlist(list(df.chunk,
                                         data.frame(postid=ego.post,
                                                    motif = NA,
                                                    neigh.size = gego.size,
                                                    neigh.pruned.size = gego.pruned.size,
                                                    conversation.hash = digest(ego.pruned.names), # after pruning
                                                    conversation.hash.full = -1, #digest(V(gego)$name), # before pruning,
                                                    motif.dict.size = NA,
                                                    motif.found.in = NA,
                                                    motif.found.in.naive = NA,
                                                    url = url.thread,
                                                    subforum = subforum)))
              next
            }
        }else {
          gego.pruned <- gego # we alredy puned before !
        }
        ###########################
        
        # Order motifs by frequency from time to time 
        if(post.counts %% 50 == 0 || post.counts < 10){
          idx.order <- as.numeric(names(sort(table(df.chunk$motif), decreasing = TRUE)))
        }
        idx.order <- as.numeric(names(sort(table(df.chunk$motif), decreasing = TRUE)))
        # find motif
        is.new <- TRUE                               
        pos <- 0
        for(idx in idx.order){
          pos <- pos + 1
          gmotif <- motifs.dict[[idx]]
          
          gego.pruned.color <- vertex_attr(gego.pruned, 'color') # optimised V(gego.pruned)$color 
          gmotif.color <- vertex_attr(gmotif, 'color')   # optimised V(gmotif)$color
          
          if(length(gego.pruned.color) != length(gmotif.color)) next # vf2 doesn't like different sizes
          
          # I don't know why this dosn't work
          #if(length(unique(gego.pruned.color) != length(unique(gmotif.color)))) next

          # Classic 
          #if(vcount(gego.pruned) != vcount(gmotif)){ next }          
          #if(length(unique(V(gego.pruned)$color)) != length(unique(V(gmotif)$color))){ next }
          #if(all(table(V(gego.pruned)$color) != table(V(gmotif)$color))){ next }
          
          if(isomorphic(gego.pruned, gmotif, method = 'vf2')){ 
            is.new <- FALSE; 
            motif.found.in <- pos
            motif.found.in.naive <- motifid
            break 
            } # break as soon as you find something
        }
        
        # if new, add motif the dictionary
        # note: we are modifying a local copy
        if(is.new){
          motifid <- length(motifs.dict) + 1
          motifs.dict[[motifid]] <- gego.pruned
          #plot(gego.pruned)
          #plot.motifs.dict(motifs.dict)
          #title(length(motifs.dict), outer=TRUE, line=-30)
          #cat(paste("new motif at position,", motifid))

          motif.found.in <- motifid
          motif.found.in.naive <- motifid
          
          idx.order <- as.numeric(names(sort(table(df.chunk$motif), decreasing = TRUE)))
        }
        
        # we won't have it if we used order 'method'
        ego.pruned.names <- vertex_attr(gego.pruned, "name")
        
        # store result
        df.chunk <- rbindlist(list(df.chunk,
                                   data.frame(postid=V(gtree)[post]$name,
                                              motif = motifid,
                                              neigh.size = gego.size,
                                              neigh.pruned.size = gego.pruned.size,
                                              conversation.hash = digest(ego.pruned.names), # after pruning
                                              conversation.hash.full = -1, #digest(V(gego)$name), # before pruning
                                              motif.dict.size = length(motifs.dict),
                                              motif.found.in = motif.found.in,
                                              motif.found.in.naive = motif.found.in.naive,
                                              url = url.thread,
                                              subforum = subforum)))
        post.counts <- post.counts + 1
        
      } # end of th
      
      # Debug
      if(FALSE){
        par(mfrow=c(1,1))
        mat.motifs <- matrix(c(2:16), ncol=5, nrow=3, byrow=TRUE)
        mat.tree  <- matrix(1, nrow=dim(mat.motifs)[1], ncol=4)
        mat <- cbind(mat.tree, mat.motifs)
        layout(mat)
        plot.tree(gtree)
        for (idx in head(1:length(tree.motifs), 15)){
          
          plot.motif(tree.motifs[[idx]]) 
        }
        title(paste(subforum, th))
      }
    } # for (th in chunk) // end of chunk
    
    #}) # END PROFILING
    
    # chunk results and its dictionary # (send them to the upper level)
    res <- list(posts.motifs = df.chunk, motifs = motifs.dict)
    return(res)
    
  } # foreach(chunk = chunks,...) %dopar% // end of all chunks

  stopCluster(cl)
  
  remove(gforest)
  remove(df.posts)
  remove(df.vertices)
  
  # Synchronise dictionaries and labels of a same forum
  cat('\nMerging all chunks')
  if(length(res.parallel)>1){
    res.subforum <- merge.motif.counts(res.parallel)
  }
  else{
    res.subforum <- res.parallel
  }
  
  data.list[[i]] <- res.subforum
}

print(Sys.time())

# save it!
save(data.list, file=file.output)
if(FALSE){
  save(data.list, file="data/motifs_by_user_structural_radius_1.rda")
  load("data/motifs_by_user_structural_radius_1.rda")
}

#############################
# Load results
#############################
if(FALSE){
  filename <- 'data/res.motifs_by_user_struct_r1_u100_p150.rda'
  filename <- 'data/res.motifs_by_user_struct_r2_u100_p150.rda'
  filename <- 'data/res.motifs_order_r2_u100_p100.rda'
  filename <- 'data/res.motifs_order_r3_u100_p250.rda'
  filename <- 'data/res.motifs_order_r4_u100_p150.rda'
  load(filename) # loads data.list object
  model <- NA
  # model ='motifs_radius_r2_u100_p150'

}

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
# Create a dataframe with all posts info
################################################################################
# Merge all the dictionaries that we can compare the results easier
# Remove lines with NA
cat("\nMerging all subforums")
if(length(data.list)>1) {
  all.merged <- merge.motif.counts(data.list)
} else {
  all.merged <- data.list[[1]]
}
# Remove incomplete cases and duplicates
all.merged$posts.motifs <- all.merged$posts.motifs[complete.cases(all.merged$posts.motifs),]

# Remove duplicates (don't count the same conversation twice for the same user)
# dd  <- all.merged$posts.motifs  %>% group_by(conversation.hash, motif) %>% summarise(reps = n()) %>% arrange(desc(reps)) %>% filter(reps>1) %>% as.data.frame
# dd.unique <-  all.merged$posts.motifs %>% distinct(conversation.hash, .keep_all = TRUE)
all.merged$posts.motifs <- all.merged$posts.motifs %>% distinct(conversation.hash, .keep_all = TRUE)


# Re-sort motifs ids by their sum of probabilities
# to avoid that the biggest dataset imposes its criteria
df.motifs.prob.avg <- all.merged$posts.motifs %>% 
  group_by(subforum, motif) %>% summarize(freq=n()) %>% ungroup %>% 
  group_by(subforum) %>% mutate(prob=freq/sum(freq)) %>% ungroup %>%
  group_by(motif) %>% summarise(probavg = mean(prob)) %>%
  arrange(desc(probavg))
idx.new <- df.motifs.prob.avg$motif
all.merged$posts.motifs$motif <- match(all.merged$posts.motifs$motif, idx.new)
all.merged$motifs <- all.merged$motifs[idx.new]

# Create a new df.posts with the motif neighbourhood of each post,
# and only for the active users
data.list_ <- list()
for (i in 1:length(subforums)){
  subforum <- subforums[i]
  load(paste0('data/df.posts.', subforum, '.rda'))
  parent.users <- df.posts[match(df.posts$parent, df.posts$postid),]$user
  df.posts <- df.posts %>% 
    mutate(date=as.numeric(date)) %>% 
    mutate(rdate = as.Date(as.POSIXct(date, origin="1970-01-01")))  %>%
    mutate(date=date - min(date)) %>%
    mutate(parent.user=parent.users) %>%
    mutate(subforum=subforum)
  data.list_[[i]] <- df.posts
}
df.posts <- rbindlist(data.list_)

  # Finally, we store a dictionary of motifs
# and the data.frame of posts-motif (with extra)
motifs.dict <- all.merged$motifs
df <- merge(all.merged$posts.motifs, df.posts, by=c('postid', 'subforum'), all.x=TRUE)
df$subforum <- as.factor(df$subforum)
df$parent <- as.factor(df$parent)
df$user <- as.factor(df$user)
df$thread <- as.factor(df$thread)
df$parent.user <- as.factor(df$parent.user)

# subforum, thread, user, motif
#df <- df %>% select(subforum, postid, user, motif)

# Check all users have 100 threads or so
# And that there are only the crawled users
cat('Total users:', length(unique(df$user)))
df.users <- df %>% group_by(subforum, user) %>% mutate(nposts = n()) %>% ungroup

df.freqs_by_user <- df %>% select(subforum, motif, user) %>%
  group_by(user) %>% mutate(nposts = n()) %>% ungroup %>%
  group_by(subforum, user, nposts, motif) %>% summarise(freq = n()) %>%
  group_by(subforum, user) %>% mutate(prob=freq/sum(freq)) %>% ungroup

# subforum, motif, freq, prob
df.freqs_by_forum <- df %>% 
  group_by(subforum, motif) %>% summarize(freq=n()) %>% ungroup %>% 
  group_by(subforum) %>% mutate(prob=freq/sum(freq)) %>% ungroup

############################################################
# Plot themes
###########################################################
theme_horizontal <- theme_bw() + 
                    theme(strip.background = element_rect(fill = 'white'), 
                          legend.position = "none",
                          aspect.ratio=1)
                    

theme_vertical <- theme_bw() + 
  theme(strip.background = element_rect(fill = 'white'), 
        legend.position = "none")

###############################################################
# PLOT MOTIF FREQUENCIES: BY FORUM AND BY USER
###############################################################
# Plot dictionary
ncol <- 8
plot.motif.counts(all.merged, nrow=4, ncol=4, n=50)
plot.motif.counts(all.merged,
                  nrow = 4, #ceiling(length(motifs.dict)/ncol),
                  ncol = 8,# ncol,
                  width = 210, # default: 210
                  height = 180, # default: 297
                  #res = 1200,
                  filename = paste0('ch3_dict_', model, ".png"))

par(mfrow=c(1,1))

# Simple
ggplot(filter(df, as.numeric(motif)<100), 
       aes(x=as.numeric(motif), group=subforum, color=subforum)) + 
  geom_point(stat='count') +
  #scale_y_log10() +
  theme_bw()+
  ggtitle('Total counts')

# Plot triad probability by forum
# if there are many motifs 
# use geom_point #and motif as number
g <- ggplot(filter(df.freqs_by_forum, motif<=30), 
             aes(x=as.factor(motif), y=prob)) +
             #aes(x=motif, y=prob)) +
  #geom_point(size=0.5) +
  #scale_x_discrete(breaks=c(1,5,10,15,20,25,30,35,40,45,50)) +
  scale_x_discrete(breaks=seq(1,26,by=2)) +
  geom_bar(stat='identity', aes(fill=as.factor(motif))) +
  facet_grid(. ~ subforum, scale = 'fixed') +
  theme_horizontal + theme(axis.text.x = element_text(size=8, angle = 90, hjust = 1))+
  ylab('probability') + xlab('motif')
#g <- g + ggtitle("Neighbourhood census (radius=1)")
print(g)
ggsave(file=paste0('ch3_', model, '_motifprob_byforum.png'), 
       width=200, height=70, units='mm')

# Plot motif probability by user
###########################################
# Plot the aggregated probability distribution over motifs
# Each p(motif | user) is a categorical distribution
g <- ggplot(filter(df.freqs_by_user, motif<=30), 
             aes(x=as.factor(motif), y= freq/nposts, fill=as.factor(motif))) + 
  #scale_x_discrete(breaks=c(1,5,10,15,20,25,30,35,40,45,50)) +
  scale_x_discrete(breaks=seq(1,26,by=2)) +
  geom_boxplot(aes(group=motif),  outlier.size=0.5) + # default outlier.size=1.5
  facet_grid(. ~ subforum, scale = 'fixed') +
  theme_horizontal + theme(axis.text.x = element_text(size=8, angle = 90, hjust = 1)) +
  ylab('probability') + xlab('motif')
print(g)
ggsave(file=paste0('ch3_', model, '_motifprob_byuser.png'), 
       width=200, height=70, units='mm')


##################################
# Clusters
##################################
# http://sna.stanford.edu/lab.php?l=6
par(mfrow=c(1,1))
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

# user x motifs matrix. Remove casual motifs
#user_motif <- acast(df, user~motif) # original
#user_motif <- t(apply(user_motif, 1, function(x) x/sum(x))) # normalize to bernouilli distributions
#user_motif <- user_motif[,1:10] # filter rare motifs. take a look at the above plots to choose the limit

#heatmap(user_motif)
#colfunc <- colorRampPalette(c('#ffeda0','#ffeda0','#f03b20'))
#heatmap.2(user_motif,col=colfunc ,scale="row", trace="none")
# Assign user ids as integers

#df <- df %>% group_by(subforum) %>% mutate(userid = match(user, unique(user))) %>% ungroup


# Motifs should be relevant for at least 10 users. Filter those that are not
by_user_motifs <- acast(df, user~motif) # original
by_user_motifs <- t(apply(by_user_motifs, 1, function(x) x/sum(x))) # normalize to bernouilli distributions

# Use the elbow technique to decide who are the irrelevant
# If they have more than users*0.01 outliers, keep them
plot(apply(by_user_motifs, 2, function(n) mean(n)), xlim=c(0,100))
n.outliers <- apply(by_user_motifs, 2, function (n) sum(is_outlier(n)))
max.probs <- apply(by_user_motifs, 2, function(x) max(x))
# Change the 0.1 threshold for more or less aggressive cuts
is.irrelevant <- sapply(1:length(n.outliers), 
                        function(i) {
                          (((n.outliers[i] < 0.1 * dim(by_user_motifs)[1]) || (max.probs[i] <0.1)) &&
                          (i>20)) || (i > 100)})
idx.irrelevants <- which(is.irrelevant)
idx.relevants <- which(!is.irrelevant)


# Collapse all these strange motifs into an "other" category
df.freqs_by_user <- df.freqs_by_user %>% 
  mutate(motif = ifelse(motif %in% idx.relevants, motif,0)) %>%
  group_by(subforum, user, nposts, motif) %>% 
  summarise(freq=sum(freq),
            prob=sum(prob))

df <- df %>%
    mutate(motif = ifelse(motif %in% idx.relevants, motif, 0))

# Alternative plot with with ggplot
#####################################
# Add user id so that id can be shared by facets
df.freqs_by_user <- df.freqs_by_user %>% 
  group_by(subforum) %>% mutate(userid = match(user, unique(user)))

ggplot(filter(df.freqs_by_user, motif<=1000), aes(y=userid, x=as.factor(motif))) +
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  geom_raster(aes(fill = prob)) + 
  scale_fill_gradient(low = "white", high = 'steelblue') +
  facet_grid(. ~ subforum, scale = 'fixed') +
  theme_horizontal + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, size=6),
        aspect.ratio=1) +
  xlab('motif') + ylab('user')
ggsave(file=paste0('ch3_', model, '_features_matrix.png'), 
       width=200, height=70, units='mm')


# Cluster by forum
i <- 1
NCLUSTERS <- 10
data.list_ <- list()
for (i in 1:length(subforums)){
  df.forum <- filter(df, subforum==subforums[i])
  user_motif <- acast(df.forum, user~motif) # original
  user_motif <- t(apply(user_motif, 1, function(x) x/sum(x))) # normalize to bernouilli distributions

  
  user_cors <- cor(t(user_motif), method='pearson')
  dissimilarity <- 1 - user_cors
  user_dist <- as.dist(dissimilarity)
  user_hclust <- hclust(user_dist, method="ward.D2")
  
  # Decide number
  plot(user_hclust)
  
  z <- cutree(user_hclust, h=0.9)
  df.user_cluster <- data.frame(user=names(z), cluster=as.vector(z), subforum=subforums[i])
 
  data.list_[[i]] <- df.user_cluster
}
df.users_clusters <- rbindlist(data.list_)
df.users_clusters <- merge(df.freqs_by_user, df.users_clusters)
df.users_clusters$cluster <- as.factor(df.users_clusters$cluster)

# Give id following cluster labels so that they are plot together
df.users_clusters <- df.users_clusters %>% 
          group_by(subforum) %>% 
          arrange(cluster) %>%
          mutate(userid = match(user, unique(user))) %>%
          ungroup

# plot heatmap (visual result) putting clusters together
# draw horizontal lines to show the cluster boundaries
# Detect boundaries
df.boundaries <- df.users_clusters %>% 
                  group_by(subforum, cluster) %>% 
                  summarize(size=length(unique(user))) %>%
                  mutate(boundary=cumsum(size)+0.5) %>%
                  ungroup %>% as.data.frame

ggplot(filter(df.users_clusters, motif<=100), 
       aes(x=as.factor(motif), y=userid)) +
  scale_x_discrete(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  geom_raster(aes(fill = prob)) + 
  scale_fill_gradient(low = "white", high = 'steelblue') +
  facet_grid(. ~ subforum, scale = 'fixed') +
  geom_hline(data=df.boundaries, aes(yintercept = boundary))+
  theme_horizontal + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, size=6)) +
  xlab('motif') + ylab('user')
ggsave(file=paste0('ch3_', model, '_cluster_matrix.png'), 
       width=200, height=70, units='mm')

# Another view (quite nice for visual confirmation, but not for reporting)
ggplot(filter(df.users_clusters, motif<=100), 
       aes(x=as.factor(motif), y=prob, group=user, color=cluster)) +
  geom_line(alpha=0.1)+
  stat_summary(fun.y= mean, aes(y=prob, group=cluster), geom='line', alpha=1, size=0.75)+
  stat_summary(fun.y= mean, aes(y=prob, group=cluster), geom='point', alpha=1, size=1.5)+
  facet_grid(. ~ subforum, scale = 'fixed') +
  theme_horizontal + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6)) +
  ylab("probability") + xlab('motif')
ggsave(file=paste0('ch3_', model, '_cluster_profiles.png'), 
       width=200, height=70, units='mm')

# Description of every group
# to put it into a table
df.summary <- df.users_clusters %>% 
              group_by(subforum, cluster, motif) %>% 
              summarise(mean.prob=mean(prob)) %>% 
              ungroup %>% 
              arrange(subforum, cluster, motif) %>%
              as.data.frame