#########################
# Role co-participation
#########################
m.thread_cluster <- merge(df.posts, df.users, by='user') %>% 
  acast(thread~cluster) %>%
  apply(1, function(x) x/sum(x)) %>%
  t

role.cooccur <- t(m.thread_cluster)%*%m.thread_cluster
image(role.cooccur)

##############################
# User co-participations
##############################
# TODO: It might be interesting to do blockmodeling over the cooccur matrix
user_thread_matrix <- group_by(df.posts, user, thread)  %>% 
  tally() %>%
  acast(user ~ thread, fun.aggregate = length)
user_thread_matrix <- ifelse(user_thread_matrix>0, 1,0)
cooccur <- user_thread_matrix%*%t(user_thread_matrix)
cooccur <- ifelse(cooccur>0, 1,0)
image(cooccur)
title(paste("density:", mean(cooccur)*100))

# Co-participations of active users
user_thread_matrix <- user_thread_matrix[df.users$cluster!=0,]
cooccur.active <- user_thread_matrix%*%t(user_thread_matrix)
cooccur.active <- ifelse(cooccur.active>0, 1,0)
image(cooccur.active)
title(paste("density:", mean(cooccur.active)*100))

heatmap(cooccur.active, 
        Colv=NA, Rowv=NA, labRow=NA)

# Graph
#############################################################
par(mfrow=c(1,1))
g <- graph.adjacency(cooccur, mode = "undirected", diag = FALSE)
V(g)$cluster <- df.users$cluster # add clusters to colors users
g <- delete.vertices(g, "root") # remove root since this is an artificial user

# Remove micro-components (probably threads that have just started at the end of January)
memberships <- components(g)$membership
g <- induced_subgraph(g, memberships==1)
# remove non-active users (those that have no role)
g <- induced_subgraph(g, V(g)$cluster!=0)

# Save to file for Gephi
fname <- paste0('forum', '.edges')
write_graph(g, fname, format = c("edgelist"))

# Plot graph, only for active users
degrees <- degree(g)
q_degree <- quantile(degrees, 0.5)
V(g)$label <- V(g)$name
#V(g)$label[degree(g)<800] = ""
la <- layout_with_fr(g)
la <- layout_with_lgl(g)
memberships.eigen <- cluster_leading_eigen(g)$membership  
table(memberships.eigen)

sizes <- ifelse(degree(g)>100, 1+0.5*tanh(degrees-q_degree), 0)
colors <- cluster.colors[V(g)$cluster]
plot(g,
     layout = la, 
     vertex.color = memberships.eigen,
     #vertex.color = colors,
     vertex.size = 1,
     vertex.label.cex = 0.8,
     edge.width = 0.2, 
     edge.arrow.size=0.02,
     asp=9/16,
     margin=-0.15)


# TODO: graph of who replied to who. That would be more conversation-based and more related to the current analysis
df.edges <- select(df.posts, postid, parent, date)
df.vertices <- select(df.posts, postid, user, date)
df.edges <- filter(df.edges, parent%in%df.vertices$postid) # only edges with known vertices

df.edges <- merge(df.edges, df.vertices, by='postid') %>%
  merge(df.vertices, by.x='parent', by.y='postid') %>%
  select(-parent, -postid, -date, -date.y)
names(df.edges) <- c('date', 'from', 'to')