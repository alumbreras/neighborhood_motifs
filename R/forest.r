library(igraph)
library(rgexf)
library(dplyr)
forest <- function(df.posts){
  df.edges <- select(df.posts, postid, parent, date)
  df.vertices <- select(df.posts, postid, user, date)
  df.edges <- filter(df.edges, parent%in%df.vertices$postid) # only edges with known vertices
  gforest <- graph.data.frame(df.edges, directed=TRUE, vertices=df.vertices)
  gforest
}


plot.forest <- function(gforest){
  #V(gforest)$date <- as.numeric(V(gforest)$date) - min(as.numeric(V(gforest)$date))
  #E(gforest)$date <- as.numeric(E(gforest)$date) - min(as.numeric(E(gforest)$date))
  
  la <- layout_with_lgl(gforest)
  plot(gforest,
       layout = la, 
       vertex.size = 1,
       vertex.label = NA,
       edge.width = 0.2, 
       edge.arrow.size=0.02,
       asp=9/16,
       margin=-0.15)
  
  # Dynamic graph
  df.edges$date <- as.numeric(df.edges$date) - as.numeric(min(df.edges$date))
  df.vertices$date <- as.numeric(df.vertices$date) - as.numeric(min(df.vertices$date))
  df.nodeDyn <- data.frame(start=df.vertices$date, end=max(df.vertices$date))
  df.edgeDyn <- data.frame(start=df.edges$date, end=max(df.edges$date))
  df.nodeDyn <- data.frame(start=df.vertices$date, end=df.vertices$date)
  df.edgeDyn <- data.frame(start=df.edges$date, end=df.edges$date)
  write.gexf(select(df.vertices, postid, user), select(df.edges, postid, parent), 
             nodeDynamic=df.nodeDyn, 
             edgeDynamic=df.edgeDyn,
             defaultedgetype='directed',
             output='podemos_dynamic.gexf')
}

socialnetwork <- function(df.posts){
  
  df.posts$date <- as.numeric(df.posts$date) - min(as.numeric(df.posts$date))
  df.posts <- filter(df.posts, user!='[deleted]')
  
  # Use this to make every root as a different user
  #idx.root <- which(df.posts$user=='root') # give every root its own identity
  #df.posts[idx.root,]$user <- sapply(1:length(idx.root), function(x) paste0("root.",x))
  
  # Edges and vertices in the posts graph
  ########################################
  df.edges <- select(df.posts, postid, parent, date)
  df.vertices <- select(df.posts, postid, user, date)
  df.edges <- filter(df.edges, parent%in%df.vertices$postid) # only edges with known vertices
  

  # Edges and vertices in the users graph
  #######################################
  df.edges <- merge(df.edges, df.vertices, by='postid') %>%
              merge(df.vertices, by.x='parent', by.y='postid') %>%
              select(-parent, -postid, -date, -date.y)
  names(df.edges) <- c('date', 'from', 'to')
  df.edges <- df.edges[,c(2,3,1)]
  df.edges <- df.edges[order(df.edges$date),] # sort chronologically
  
  # vertex are users and date is first date they appear
  df.vertices <- select(df.vertices, user, date) %>%
                 arrange(date) # sort chronologically               
  
  df.vertices <- df.vertices[!duplicated(df.vertices$user),] # don't keep duplicates, only first instance
  names(df.vertices) <- c('user', 'start')
    
  # the end time of a vertex is the last time it appeared in df.edges
  df.edges.inv <- df.edges[nrow(df.edges):1,]
  df.to.last <- select(df.edges.inv[!duplicated(df.edges.inv$to),], to, date)
  df.from.last <- select(df.edges.inv[!duplicated(df.edges.inv$from),], from,date)
  df.temp <- merge(df.from.last, df.to.last, by.x='from', by.y='to', all=TRUE)
  df.temp[is.na(df.temp)] <- 0
  df.temp$end <- apply(df.temp, 1, function(x) max(x[2],x[3]))
  df.temp <- select(df.temp, from, end)
  df.vertices <- merge(df.vertices, df.temp, by.x='user', by.y='from', all.x=TRUE)


  df.vertices$username <- df.vertices$user
  df.vertices <- df.vertices[df.vertices$user!='root',] # delete root
  df.edges <- df.edges[df.edges$to!='root',] # delete root
  
  gsna <- graph.data.frame(df.edges, directed=TRUE, vertices=df.vertices)
  #gsna
  ############################################################################
  ############################################################################
  # Dynamic graph  
  # Convert to date time
  #df.vertices$start <- as.POSIXct(as.numeric(df.vertices$start), origin="1970-01-01") %>% format(format="%FT%T")
  #df.vertices$end <- as.POSIXct(as.numeric(df.vertices$end), origin="1970-01-01") %>% format(format="%FT%T")
  #df.edges$date <- as.POSIXct(as.numeric(df.edges$date), origin="1970-01-01") %>% format(format="%FT%T")
  
  df.nodeDyn <- select(df.vertices, start, end)
  df.edgeDyn <- data.frame(start=df.edges$date, end=df.edges$date)
  write.gexf(select(df.vertices, user, username), select(df.edges, from, to), 
             nodeDynamic=df.nodeDyn, 
             edgeDynamic=df.edgeDyn,
             defaultedgetype='directed',
             output='podemos_SNA_dynamic.gexf')
}

