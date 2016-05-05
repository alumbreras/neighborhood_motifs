#
# Repeat the time-based extraction but instead of post by posts, 
# local-dynamic by local-dynamic
# The (initial) motivation is to avoid etracting overlapping neighbourhoods 
# from a same user
#################################################
source('R/count_motifs_by_segment.r')
###################################################
# Load data
###################################################
load('./R_objects/dfposts_podemos.Rda')
df.posts <- df.posts
df.posts$date <- as.numeric(df.posts$date)
df.posts <- data.frame(df.posts) %>% arrange(date)
df.posts <- df.posts[1:75000,] # Paper
df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"
df.posts$forum <- 'podemos'

#########################################################
# Compute neighborhoods by user-dynamic_segment
#########################################################
# Only long threads
#df.threads <- filter(df.threads, length>10)

chunks <- split(df.threads$thread, ceiling(seq_along(df.threads$thread)/135))
length(chunks)

res.seq <- count_motifs_by_usersegment(as.vector(unlist(chunks)), 
                                      database='reddit')