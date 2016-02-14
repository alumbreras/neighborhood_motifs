library(RSQLite)
source('R/extract_from_db.r')

load_posts <- function(database='reddit', forum='podemos'){
  con <- dbConnect(dbDriver("SQLite"), dbname = paste0("./data/", database, ".db"))
  
  # Threads in forum
  thread.ids <- dbGetQuery(con,  paste0("select threadid from threads where forum='", forum, "'"))
  
  # Dataframe of users and threads where they participated
  df.posts <- data.frame()
  for(i in 1:nrow(thread.ids)){ 
    #thread.posts <- dbGetQuery(con,  paste0("select user,date from posts where thread='", thread.ids[i,], "'"))
    thread.posts <- dbGetQuery(con,  paste0("SELECT postid, parent, user, date FROM posts WHERE thread='", thread.ids[i,], "'"))
    thread.posts$thread <- thread.ids[i,]
    df.posts <- rbind(df.posts, thread.posts)
  }
  return(df.posts)
}