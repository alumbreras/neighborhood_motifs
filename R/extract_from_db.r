#TODO: time: respect time spans between posts

# Dataframe with date formats to parse the original databases 
config <- data.frame(matrix(NA, 4,2))
names(config) <- c("date", "tz")
config[1,] <- c("%Y-%m-%dT%H:%M:%SZ", "UTC") # boards
config[2,] <- c("%Y-%m-%d %H:%M:%S", "UTC") # imdb
config[3,] <- c("%a %b %d %H:%M:%S", "CEST") # slashdot
config[4,] <- c("%s", "UTC") # reddit (Unix epochs)
datasets <- c("boards", "imdb", "slashdot", "reddit")


posts.to.graph <- function(posts, dataset){
  
  idx <- which(datasets==dataset)
  FORMAT.DATE <- config[idx,]$date
  TZ <- config[idx,]$tz  
  
  dfposts.vertices <- posts[c("postid", "user",  "date", "text")]
  if(dataset == "boards") {
    dfposts.edges <- posts[c("postid", "real_parent")]
    real <- dfposts.edges$real_parent==""
    dfposts.edges[real,]$real_parent= posts[real,]$parent
    names(dfposts.edges) <- c("postid", "parent")
  } else {
    dfposts.edges <- posts[c("postid", "parent")]
    names(dfposts.edges) <- c("postid", "parent")
  }  
  
  # Create an edge only if it has a parent (i.e.: avoid root -> NA)
  dfposts.edges[dfposts.edges==""] <- NA 
  dfposts.edges <- dfposts.edges[!is.na(dfposts.edges$parent), ]
  
  # assert
  if(TRUE){
    src.posts <- unique(c(dfposts.edges[,1]))
    dst.posts <- unique(c(dfposts.edges[,2]))
    vertices.posts <- dfposts.vertices[,1]
    src.posts %in% vertices.posts
    dst.posts %in% vertices.posts
  }

  
  g <- try(graph.data.frame(dfposts.edges, vertices=dfposts.vertices, directed=TRUE))
  if ('try-error' %in% class(g)){
    stop("Couldn't create graph from dataframe. Skip thread")
  } 
  
  # Check there is only one orphan vertex (the root)
  root <- V(g)[igraph::degree(g, mode="out")==0]
  if(length(root)>1){
    stop("\n Multiple roots. Skip thread\n")
  }
  
  # date conversion after graph is created (it does strange things if done before)
  V(g)$date <- as.integer(as.POSIXct(strptime(V(g)$date, FORMAT.DATE, tz=TZ)))
  
  if (any(is.na(V(g)$date))){
    stop("\n Deleted posts. Skip thread\n")
  }
  
  # Assert: root is the older post
  if (V(g)[order(V(g)$date)][1]!= root){
    stop("\n Root is not the older post. Skip thread\n")
  }
  
  # Assert: graph is connected
  if (!is.connected(g)){
    stop("\n Graph is not connected. Skip thread\n")
  }
  
  # Create users temporal social network
  dfusers.edges <- data.frame()
  for(e in 1:dim(dfposts.edges)[1]){
    post.src <- dfposts.edges[e,]$postid
    post.dst <- dfposts.edges[e,]$parent
    user.src <- posts[posts$postid == post.src,]$user
    user.dst <- posts[posts$postid == post.dst,]$user
    time <- posts[posts$postid == post.src,]$date
    dfusers.edges <- rbind(dfusers.edges, as.character(c(user.src, user.dst, time)))
    dfusers.edges[,1] <- as.character(dfusers.edges[,1]) 
    dfusers.edges[,2] <- as.character(dfusers.edges[,2])
    dfusers.edges[,3] <- as.character(dfusers.edges[,3])
  }
  names(dfusers.edges) <- c("src", "dst", "time")
  
  # From dates to seconds from the first post
  dfusers.edges[,3] <- as.POSIXct(strptime(dfusers.edges[,3], FORMAT.DATE, tz=TZ))
  
  dfusers.edges[,3] <- as.integer(dfusers.edges[,3])
  #t0 <- min(dfusers.edges[,3])
  dfusers.edges[,3] <- dfusers.edges[,3] #- t0
  
  #dfusers.edges[,3] <- rank(dfusers.edges[,3])
  dfusers.vertices <- unique(posts[c("user")])
  
  # Set up posts network (column names and dates)
  dfposts.edges.temp <- data.frame()
  for(e in 1:dim(dfposts.edges)[1]){
    post.src <- dfposts.edges[e,]$postid
    post.dst <- dfposts.edges[e,]$parent
    time <- posts[posts$postid == post.src,]$date
    dfposts.edges.temp <- rbind(dfposts.edges.temp, as.character(c(post.src, post.dst, time)))
    dfposts.edges.temp[,1] <- as.character(dfposts.edges.temp[,1]) 
    dfposts.edges.temp[,2] <- as.character(dfposts.edges.temp[,2])
    dfposts.edges.temp[,3] <- as.character(dfposts.edges.temp[,3])  
  }
  
  dfposts.edges <- dfposts.edges.temp
  names(dfposts.edges) <- c("src", "dst", "time")
  dfposts.edges[,3] <- as.POSIXct(strptime(dfposts.edges[,3], FORMAT.DATE, tz=TZ))
  
  dfposts.edges[,3] <- as.integer(dfposts.edges[,3])
  #t0 <- min(dfposts.edges[,3])
  dfposts.edges[,3] <- dfposts.edges[,3] #- t0
  
  #dfposts.edges[,3] <- rank(dfposts.edges[,3])
  
  gu <- graph.data.frame(dfusers.edges, dfusers.vertices, directed=TRUE)
  E(gu)$time <- dfusers.edges[,3]
  gp<- graph.data.frame(dfposts.edges, dfposts.vertices, directed=TRUE)
  E(gp)$time <- dfposts.edges[,3]
  
  # Shorten IDs
  if(dataset=="boards"){
    V(gu)$name <- unlist(lapply(V(gu)$name, function(x) unlist(strsplit(as.character(x), "="))[2])) 
    V(gp)$name <- unlist(lapply(V(gp)$name, function(x) unlist(strsplit(as.character(x), "="))[2])) 
    V(gp)$user <- unlist(lapply(V(gp)$user, function(x) unlist(strsplit(as.character(x), "="))[2]))
  }
  
  res <- list(gu = gu,
              gp = gp)
  class(res) <- "threadgraph"
  
  res
}

database.to.posts <- function(idthread, con){
  query <- paste0("select * from posts where thread =  '", idthread, "'")
  posts <- dbGetQuery(con, query)
  posts
}

database.to.graph <- function(idthread, con, dataset){
  # Get a list of the first nthreads of dataset in the form of trees
  #
  # Args:
  #  nthreads: number of threads to process
  #  dataset: name of dataset
  #
  # Returns:
  #  A list of trees (igraph objects) 
      
  posts <- database.to.posts(idthread, con)
  graph <- try(posts.to.graph(posts, dataset)) 
  graph
}


plot.threadgraph <- function(threadgraph, title=""){
  par(mfrow=c(1,2))
  
  gp <- threadgraph$gp
  gu <- threadgraph$gu
  
  #colors for the nodes are chosen from the very beginning
  V(gu)$color <- rainbow(length(unique(V(gu))))
  
  # users are given consecutive numbers 
  # and then posts are given the color of their users
  vnames.p <- match(V(gp)$user, unique(V(gp)$user))
  V(gp)$color <- rainbow(length(unique(V(gp)$user)))[vnames.p]
  
  layout.users <- layout_with_fr(gu)
  layout.posts <- layout_with_fr(gp)
  plot(gu,
       layout = layout.users, 
       vertex.label = "",
       vertex.size = 1 + 2 * log( graph.strength(gu), 3 ), 
       edge.width = 1.5,
       asp=9/16,
       margin=-0.15)
  title("users")
  plot(gp,
       layout = layout.posts, 
       vertex.label = "",
       vertex.size = 1 + 2 * log( graph.strength(gp), 3 ), 
       edge.width = 1.5,
       asp=9/16,
       margin=-0.15)
  title("posts")
  title(title, outer=TRUE, line=-1)
}