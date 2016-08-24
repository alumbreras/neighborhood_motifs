#  Deprecated. This is now in rlumbreras
library(RSQLite)
library(data.table)
source('R/extract_from_db.r')
# SELECT p.postid, p.parent, p.user, p.date FROM posts p, threads t WHERE p.thread LIKE t.threadid AND t.forum LIKE 'podemos'
load_posts <- function(database='reddit', forum='podemos'){
  con <- dbConnect(dbDriver("SQLite"), dbname = paste0("./data/", database, ".db"))
  
  # Threads in forum
  thread.ids <- dbGetQuery(con,  paste0("SELECT threadid FROM threads WHERE forum='", forum, "'"))

  # Dataframe of users and threads where they participated
  df.posts <- data.frame()
  for(i in 1:nrow(thread.ids)){ 
    thread.posts <- dbGetQuery(con,  paste0("SELECT postid, parent, user, date FROM posts WHERE thread='", thread.ids[i,], "'"))
    thread.posts$thread <- thread.ids[i,]
    
    #df.posts <- rbind(df.posts, thread.posts)
    df.posts <- rbindlist(list(df.posts,thread.posts))
    
    if(i%%1000==0){
      cat('\n', i, '/', nrow(thread.ids))
    }
  }
  return(data.frame(df.posts))
}
