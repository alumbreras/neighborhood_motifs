library(igraph)
library(dplyr)
library(data.table)
library(parallel)
library(doParallel)
library(foreach)
library(digest)
library(profvis)
library(ggplot2)
library(gplots)
library(reshape2)
library(igraph)
library(RColorBrewer)
source('R/tree_changepoints.r')

options(digits=7)
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

#subforums <- c("MachineLearning", 'france')

# Filter short threads and count user threads
# then leave only threads with some active user that has participated in at least 100 threads
MIN.THREADS_BY_USER <- 50
MAX.THREADS_BY_USER <- 100000 # just for tests
NPOSTS_BY_USER <- 500 # How many posts should we sample per user
NTOP.USERS <- 1000
method = 'time'

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
  # 1. other
  # 2. ego
  # 3. replied
  # 4. root
  mypalette <- c("black", "yellow", "orange", "red", "white")
  mypalette <- c("black", "red", "white")
  par(mfrow=c(3,5))
  for(i in 1:length(motifs)){
    root <- which.min(degree(motifs[[i]], mode='out'))
    gmotif <- as.undirected(motifs[[i]])
    #la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
    la = layout_as_tree(gmotif, mode='out', root=root)
    
    plot(gmotif,
         layout = la,
         vertex.color=mypalette[V(motifs[[i]])$color],
         vertex.label = "",
         edge.arrow.size=0.6)
    cat(" ", sum(df.post.motif$motif==i))
    title(paste(i),sub=sum(df.post.motif$motif==i))  
  }
}

plot.motif.list <- function(res){
  df.post.motif  <- res$posts.motifs
  motifs <- res$motifs
  ####################################################
  # Plot found neighborhoods
  ####################################################
  # 1. other
  # 2. ego
  # 3. replied
  # 4. root
  mypalette <- c("black", "yellow", "orange", "red", "white")
  mypalette <- c("black", "red", "white")
  par(mfrow=c(3,5))
  par(mfrow=c(1,10))
  for(i in 1:length(motifs)){
    root <- which.min(degree(motifs[[i]], mode='out'))
    gmotif <- as.undirected(motifs[[i]])
    la = layout_as_tree(gmotif, mode='out', root=root)
    
    plot(gmotif,
         layout = la,
         vertex.color=mypalette[V(motifs[[i]])$color],
         vertex.label = "",
         vertex.size=40)
    cat(" ", sum(df.post.motif$motif==i))
    title(paste(i), line=-30)  
  }
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
for (i in 1:length(subforums)){
  
  # Load the entire dataframe of posts
  subforum <- subforums[i]
  load(paste0('data/df.posts.', subforum, '.rda'))
  cat('\nsubforum:', subforum )
  
  # Add Dates and parent users
  parent.users <- df.posts[match(df.posts$parent, df.posts$postid),]$user
  df.posts <- df.posts %>% 
    mutate(date=as.numeric(date)) %>% 
    mutate(rdate = as.Date(as.POSIXct(date, origin="1970-01-01")))  %>%
    mutate(date=date - min(date)) %>%
    mutate(parent.user=parent.users)
  
  # Filter short threads and count user threads
  # then leave only threads with some active user that has participated in at least 100 threads
  df.posts <- df.posts %>% 
    group_by(thread) %>% mutate(size = n()) %>% ungroup %>% filter(size>10) %>% # remove short threads
    group_by(user) %>% 
    mutate(nthreads = length(unique(thread))) %>% # count threads by user
    mutate(nposts = n()) %>%
    mutate(selected.post = row_number() %in% as.integer(seq(from=1, to=n(), length.out = min(n(),NPOSTS_BY_USER)))) %>% # 100 posts per user
    ungroup %>%
    arrange(desc(nposts), desc(nthreads), user) %>%
    mutate(user.rank = match(user, unique(user))) %>%
    #mutate(active.user = (nthreads >= MIN.THREADS_BY_USER & nthreads < MAX.THREADS_BY_USER)) %>% # filter users 
    mutate(active.user = ifelse(user.rank<=NTOP.USERS, TRUE, FALSE)) %>%
    mutate(active.user = ifelse(user %in% deleted, FALSE, active.user)) %>%
    mutate(active.post = active.user & selected.post)
  
  # Keep only threads with active users and selected posts
  df.posts <- df.posts %>%
    group_by(thread) %>% mutate(th.active.posts = sum(active.post)) %>% 
    ungroup %>%
    filter(th.active.posts > 0) %>% # select threads with posts to sample
    arrange(thread, date)
  
  # debug
  # users <- df.posts %>% group_by(user) %>% summarise(total=n(), selected = sum(selected.post)) %>% as.data.frame
  # users %>% arrange(desc(selected) ) %>% head(100)
  
  active.users <- df.posts %>% filter(active.user==TRUE) %>% distinct(user) %>% select(user) %>%
    filter(! user %in% deleted) %>% 
    unlist %>% as.vector
  
  # sample of posts writen by the active users
  # selected.posts <- df.posts %>% filter(active.user==TRUE & selected.post==TRUE) %>% 
  #                   select(postid) %>% unlist %>% as.vector
  
  all.threads <- unique(df.posts$thread)
  cat('\n Users: ', length(active.users))
  cat('\n Threads: ', length(all.threads))
  
  # Create the total graph (it is a forest of trees)
  df.edges <- df.posts %>% filter(!is.na(parent.user)) %>% select(postid, parent)
  df.vertices <- df.posts %>% select(postid, user, active.post, date)
  gforest <- graph.data.frame(df.edges, vertices = df.vertices)
  
  # Save to for gephi 
  # write.gexf(df.vertices, df.edges, output='gforest.gexf')
  
  # Search for conversation motifs thread by thread
  #################################################
  chunks <- split(all.threads, ceiling(seq_along(all.threads)/(length(all.threads)/ncores))) # split to parallelise
  cat("\n# chunks:", length(chunks))
  #chunks <- chunks[1:2] # TODO: limited just for debugging
  ################################""
  # Parallelise
  ################
  #cl = makeCluster(ncores, outfile="",  port=11439) # outfile "" makes it printing on the screen
  #registerDoParallel(cl)
  res.parallel <- foreach(ch = 1:length(chunks), .packages=c('igraph', 'dplyr', 'foreach', 'data.table', 'digest', 'changepoint')) %do%
  {
    ch <- 6
    chunk <- chunks[[ch]]
    # result containers
    motifs.dict <- list()
    df.chunk <- data.frame()
    
    # for printing
    j <- 1 
    total <- length(chunk)
    #th <- chunk[1]
    cat(total)
    
    for (th in chunk){
      cat('\n','--chunk--', ch, '--th--', th)
    }
    
    for (th in chunk) 
    {
      cat('\n', subforum, j, ' / ', total , '--chunk--', ch, '--th--', th)
      j <- j + 1
      # the thread is the subtree of node th
      gtree <- make_ego_graph(gforest, 10000, nodes = th, mode = 'in')[[1]]
      
      
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
      #for(post in V(gtree))
      post <- which(V(gtree)$active.post == TRUE)[1]
      for(post in which(V(gtree)$active.post == TRUE)) # only posts of active users
      {
        ego.user <- V(gtree)[post]$user
        ego.post <- V(gtree)[post]$name
        #ego.parent.user <- neighbors(gtree, post, mode = 'out')$user # gtree instead gego so that it never escapes
        
        # This is the core: the extraction of the neighbourhood by different methods
        # Extract neighbourhood 
        # radius
        # gego <- make_ego_graph(gtree, 1, nodes = post, mode = 'all')[[1]]
        # Detect the neighborhood with the prefered method
          rad <- 10
          gego <- neighborhood.temporal(gtree, post, rad, breakpoints.v, breakpoints.h)
        
        # Color/label it
        V(gego)$color <- 1 #"other"
        #V(gego)[user == ego.parent.user]$color <- 2 #"replied"
        V(gego)[user == ego.user]$color <- 2 # other ego posts
        V(gego)[ego.post]$color <-  0 # ego (just to tell prune() who is the real ego)
        if (th %in% V(gego)$name) V(gego)[th]$color <- 3 #"root"
        
        # Prune it
        gego.pruned <- prune(gego)
        V(gego.pruned)[ego.post]$color <-  2 # all ego posts are now colored the same
        if (th %in% V(gego.pruned)$name) V(gego.pruned)[th]$color <- 3 #"root"
        
        
        # store some stats for reporting
        gego.size <- vcount(gego)
        gego.pruned.size <- vcount(gego.pruned)
        
        # if pruned graph is still too big, skip it (is_isomorphic_to takes too long)
        if(gego.pruned.size>20) return(NULL)
        
        # motif
        is.new <- TRUE
        for(motifid in seq_along(motifs.dict)){
          gmotif <- motifs.dict[[motifid]]
          if(vcount(gego.pruned) != vcount(gmotif)) next # vf2 doesn't like different sizes
          if(length(unique(gego.pruned$color)) != length(unique(gmotif$color))) next # early check
          if(any(table(gego.pruned$color) != table(gmotif$color))) next # early chek
          
          is.iso <- is_isomorphic_to(gego.pruned, gmotif, method = 'vf2')
          if(is.iso){ is.new <- FALSE; break } # break as soon as you find something
        }
        
        # if new, add motif the dictionary
        # note: we are modifying a local copy
        if(is.new){
          motifid <- length(motifs.dict) + 1
          motifs.dict[[motifid]] <- gego.pruned
        }
        
        # store result
        df.chunk <- rbindlist(list(df.chunk,
                                   data.frame(postid=V(gtree)[post]$name,
                                              motif = motifid,
                                              neigh.size = gego.size,
                                              neigh.pruned.size = gego.pruned.size,
                                              conversation.hash = digest(V(gego.pruned)$name), # after pruning
                                              conversation.hash.full = digest(V(gego)$name), # before pruning
                                              subforum = subforum)))
        cat("rows:", nrow(df.chunk))
      }
    } # for (th in chunk) // end of chunk
    
    # chunk results and its dictionary # (send them to the upper level)
    cat('\nMerging all chunks')
    res <- list(posts.motifs = df.chunk, motifs = motifs.dict)
    return(res)
    
  } # foreach(chunk = chunks,...) %dopar% // end of all chunks
  #stopCluster(cl)
  
  remove(gforest)
  remove(df.posts)
  remove(df.vertices)
  
  # Synchronise dictionaries and labels of a same forum
  if(length(res.parallel)>1){
    res.subforum <- merge.motif.counts(res.parallel)
  }
  else{
    res.subforum <- res.parallel
  }
  
  data.list[[i]] <- res.subforum
}
