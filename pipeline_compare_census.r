# Compare the nieghbourhood census of two or more forums
# author: Alberto Lumbreras

df.posts <- load_posts(database='reddit', forum='gameofthrones')
save(df.posts,file="dfposts_gameofthrones.Rda")

df.posts <- load_posts(database='reddit', forum='sex')
save(df.posts,file="dfposts_sex.Rda")

df.posts <- load_posts(database='reddit', forum='4chan')
save(df.posts,file="dfposts_4chan.Rda")

df.posts <- load_posts(database='reddit', forum='complexsystems')
save(df.posts,file="dfposts_complexsystems.Rda")

df.posts <- load_posts(database='reddit', forum='datascience')
save(df.posts,file="dfposts_datascience.Rda")
