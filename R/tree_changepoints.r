# Functions to extract the neighborhood of a post in a tree
# Function to detect the changepoints of a tree w.r.t time
# between posts
# author: Alberto Lumbreras
##############################################################
library(changepoint)

#t3_2eh5kz    141
#t3_2dyzfs     36
#t3_2dz84u     11
#t3_2dz884     81
#t3_2dz88i     84
#t3_2e049q      7
#t3_2e04x4     11
#t3_2e05bb     14
DEBUG <- FALSE


changepoints.cp <- function(gp, vertical=FALSE, horizontal=FALSE){
  # Changepoints based of changepoint detection algorithms over the full sequence
  # Detect the posts that took a time too long to the previous post
  # - to its parent (vertical changepoint)
  # - to its siblings (horizontal changepoints)
  if(!is.numeric(V(gp)$date)){
    stop('Dates are not numeric')
  }
  
  breakpoints.v <- c()
  breakpoints.h <- c()
  
  # Vertical changepoints (from root to every leaf)
  ##################################################
  if(vertical==TRUE){
    leafs <- which(ego_size(gp, 1, mode='in', mindist=1)==0)
    root <-  which(ego_size(gp, 1, mode='out', mindist=1)==0)
    for(j in 1:length(leafs)){
      #j<-17
      # Path and its deltas 
      path <- unlist(shortest_paths(gp, root, to=leafs[j], mode='all')$vpath)
      if(length(path)<5){
        next
      }
      dates <- as.numeric(V(gp)[path]$date)
      bp <- cpts(cpt.meanvar(dates, class=TRUE, method="PELT"))+1
      breakpoints.v <- c(breakpoints.v, path[bp])
    }
    breakpoints.v <- names(breakpoints.v)
    
  }
  
  # Horizontal changepoints (between siblings)
  ############################################
  if(horizontal==TRUE){
    candidates.h <- c()
    dist.matrix <- distances(gp, mode = 'in')
    for (i in 1:nrow(dist.matrix)){
      children.mask <- dist.matrix[i,]==1
      if(sum(children.mask)>3){
        children <- colnames(dist.matrix)[which(children.mask)]
        
        # sort children by date
        children <- children[order(V(gp)[children]$date)]
        candidates.h <- c(candidates.h, children)
        dates <- as.numeric(V(gp)[children]$date)
        
        # this both cases should give the same result
        if(is.unsorted(dates)){
          stop("Posts are not sorted by date")
        }
        else{
          bp <- cpts(cpt.meanvar(dates, class=TRUE, method="PELT"))+1
          breakpoints.h <- c(breakpoints.h, children[bp])
          
          #colors <- rep("black", length(dates))
          #colors[bp] <- 'red'
          #plot(dates, col=colors, pch=19, cex=0.5)
          #title("debug")
        }
      }
    }
  }
  # Breakpoint only if both in vertical 
  # and horizontal
  ########################################
  if(vertical && horizontal){
    # include posts that are breakpoints in vertical and horizontal
    # and posts that are vertical breakpoints but have no chance to be horizontal breakpoints
    # because they have not enough siblings
    breakpoints <- union(intersect(breakpoints.v, breakpoints.h),
                         setdiff(breakpoints.v, candidates.h))
  }
  if(vertical && !horizontal){
    breakpoints <- breakpoints.v
  }
  if(!vertical && horizontal){
    breakpoints <- breakpoints.h
  }
  
  list(breakpoints.vh = V(gp)[breakpoints],
       breakpoints.v = V(gp)[breakpoints.v],
       breakpoints.h = V(gp)[breakpoints.h])
}

##########################"
# Test neighborhoods
#########################
# Debug changepoint detection
if(DEBUG){
  rad <- 4
  max.neighbors <- 20
  j <- 1
  
  #breakpoints <- changepoints.deltasequence(gp, vertical=T, horizontal=T)
  #breakpoints <- changepoints.bertrand(gp, vertical=T, horizontal=T)
  breakpoints <- changepoints.cp(gp, vertical=T, horizontal=T)
  
  breakpoints.h <- breakpoints$breakpoints.h
  breakpoints.v <- breakpoints$breakpoints.v
  breakpoints.vh <- breakpoints$breakpoints.vh
  
  #Add breakpoint info as attribute
  V(gp)$bp.v <- FALSE
  V(gp)$bp.h <- FALSE
  V(gp)$bp.vh <- FALSE
  V(gp)[breakpoints.v]$bp.v <- TRUE
  V(gp)[breakpoints.h]$bp.h <- TRUE
  V(gp)[breakpoints.vh]$bp.vh <- TRUE
  
  # Detect and plot neighborhoods
  test <- function(){
  
    #Rprof()
    for(i in 1:vcount(gp)){
      cat("\nvcount:", vcount(gp))
      cat("\n******", i)
      eg <- neighborhood.temporal(gp, i, 3, breakpoints.v, breakpoints.h)
      #plot(eg)
    }
    #summaryRprof()$by.total
  
  }
  library('lineprof')
  l <- lineprof(test())

  if(FALSE){
    par(mfrow=c(1,1))
    gp_ <- as.undirected(gp)
    la = layout_as_tree(gp_, mode='out', root=which.min(V(gp_)$date), circular=FALSE)
    V(gp_)$size <- 1
  
    V(gp_)$color <- 'black'
    #V(gp_)[V(eg)$name]$color <- 'darkgreen'
    #V(gp_)[V(eg)$name]$size <- 2
    V(gp_)[j]$size <- 3
    V(gp_)[j]$color <- 'green'
    V(gp_)[breakpoints.v]$color <- 'orange'
    V(gp_)[breakpoints.h]$color <- 'yellow'
    V(gp_)[breakpoints.vh]$color <- 'red'
    plot(gp_,
         layout = la, 
         #vertex.label = order(V(gp)),
         vertex.label = NA,
         edge.width = 0.2, 
         edge.arrow.size=0.02,
         asp=9/16,
         margin=-0.15)
    }
}

BERTRAND <- FALSE
if(BERTRAND){
  # Show that score>2 is not enough. It should be score>10.
  # but this is quite arbitrary...
  posts <- V(gp)[which(ego_size(gp, 2, mode='out', mindist = 1)==1)]
  dates <- sort(posts$date)
  plot(dates)
  deltas <- diff(dates)
  bp <- c()
  scores <- rep(1, length(dates))
  for(i in 2:length(deltas)){
    score <- (deltas[i]-deltas[i-1])/deltas[i-1]
    scores[i+1] <- score
    if(score>10){
      bp <- c(bp, i+1)
    }
  }
  colors <- rep("black", length(dates))
  colors[bp] <- 'red'
  plot(dates, col=colors, pch=19, cex=0.5)
}
CHANGEPOINT <- FALSE
if(CHANGEPOINT){

  
  # Horizontal 
  posts <- V(gp)[which(ego_size(gp, 2, mode='out', mindist = 1)==1)]
  dates <- sort(posts$date)
  res <- cpt.meanvar(dates, class=TRUE, method="PELT")
  #res <- cpt.meanvar(dates, penalty="None", class=TRUE, method="SegNeigh") # slow
  plot(res)
  
  bp <- cpts(res)+1
  colors <- rep("black", length(dates))
  colors[bp] <- 'red'
  plot(dates, col=colors, pch=19, cex=0.5)
  
  # Vertical
  leafs <- which(ego_size(gp, 1, mode='in', mindist=1)==0)
  root <-  which(ego_size(gp, 1, mode='out', mindist=1)==0)
  branches <- shortest_paths(gp, root, to=leafs, mode='all')$vpath
  depths <- unlist(lapply(branches, length))
  longest.branch <- which.max(depths)
  path <- unlist(shortest_paths(gp, root, to=leafs[longest.branch], mode='all')$vpath)
  posts <- V(gp)[path]
  dates <- sort(posts$date)
  res <- cpt.meanvar(dates, class=TRUE, method="PELT")
  plot(res)
  
  bp <- cpts(res)+1
  colors <- rep("black", length(dates))
  colors[bp] <- 'red'
  plot(dates, col=colors, pch=19, cex=1)
  
  
}

##############################
##############################
# Sequential show (for demos)
if(FALSE){
  dates <- as.numeric(V(gp_)$date)
  steps <- seq(min(dates), max(dates), length.out = 700)
  for(i in 1:100){
    V(gp_)$color <- 'black'
    V(gp_)[breakpoints]$color <- 'red'
    inactive <- as.numeric(V(gp_)$date) > steps[i]
    V(gp_)[inactive]$color <- 'white'
    plot(gp_,
         layout = la, 
         vertex.size = 1,
         vertex.label = NA,
         edge.width = 0.2, 
         edge.arrow.size=0.02,
         asp=9/16,
         margin=-0.15)
    title((steps[i]-min(dates))/3600)
    readline(paste(i, " next..."))
  }
}