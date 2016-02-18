count_motifs_by_post <- function(threads, database='reddit'){

  con <- dbConnect(dbDriver("SQLite"), dbname = paste0("./data/", database, ".db"))
  
  # For every thread, recontruct its graph. Then count 
  # if neighborhood at distance 1 and binary tree, max nodes is 4
  # if neighborhood at distance 1 and binary tree, max nodes is 10
  # 4   10   22   30   46   62   93  123...
  
  # the maximum egonet will be a binary tree
  rad <- 4
  max.neighbors <- 4
  motifs <- list()
  posts.id <- vector()
  posts.motifs.id <- vector()
  plotted.trees <- 1
  
  nthreads <- length(threads)
  for(i in 1:nthreads){
    cat('\n', i, '/', nthreads)
    
    #Extract the egos of every post
    g <- database.to.graph(threads[i], con, database)
    gp <- g$gp
    egos <- make_ego_graph(gp, rad, nodes=V(gp))
    
    if (FALSE){
      V(gp)$color <- "black"
      if(vcount(gp)>10 && vcount(gp)<30){
        par(mfrow=c(3,3))
        la <- layout_with_fr(gp)
        plot(as.undirected(gp),
             layout = la, 
             vertex.label = "",
             vertex.size = 1.5 + 1.5 * log( graph.strength(gp), 3 ), 
             edge.width = 1.5)  
        dev.copy(png, paste0('2015-01-15-tree', plotted.trees, '.png'))
        dev.off()
        plotted.trees <- plotted.trees +1
      }
    }  
    
    for(j in 1:length(egos)){
      # Reduce the ego to a maximum of N neighbors (the N closest in time)
      # and where the neighborhood is fully connected.
      # Set a different color (only) to the ego post
      user.name <- V(gp)[j]$user
      post.id <- V(gp)[j]$name
      eg <- egos[[j]]
      neighbors <- order((abs(as.numeric(V(eg)$date)-as.numeric(V(gp)[j]$date))))
      reduced.neighbors <- neighbors[1:min(length(neighbors), max.neighbors)]
      eg <- induced.subgraph(eg, reduced.neighbors)
      u <- V(eg)[V(eg)$name==post.id]
      mypalette <- c("black", "red", "white")
      V(eg)$color <- 1 
      V(eg)[u]$color <- 2 
      #V(eg)[V(eg)$user==user.name]$color <- 2 # any post from the user
      V(eg)[V(eg)$user=="root"]$color <- 3
    
      # Drop nodes that are not connected to the post
      eg <- delete_vertices(eg, which(distances(eg, u)==Inf))

      # See if it matches any seen motif
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

      posts.id <- c(posts.id, post.id)
      posts.motifs.id <- c(posts.motifs.id, motif.id)
    } # end egos
  } # end threads
  
  posts.motifs <- data.frame(postid=posts.id, motif=posts.motifs.id)
  return(list(posts.motifs = posts.motifs,
              motifs = motifs))
}