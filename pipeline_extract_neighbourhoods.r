# Extract list of triads where a user participates.
#
# author: Alberto Lumbreras
#
#https://www.reddit.com/r/europe/CasualConversation
#https://www.reddit.com/r/europe/
#https://www.reddit.com/r/datascience
#https://www.reddit.com/r/science/
#https://www.reddit.com/r/france
#https://www.reddit.com/r/catalunya
#https://www.reddit.com/r/es
library(parallel)
library(doParallel)
library(foreach)
library(ggplot2)
library(ggbiplot)
library(gplots)

library(dplyr)
library(reshape2)
library(igraph)
library(ggbiplot)
#library(RSQLite)
#library(GGally)

source('R/load_participations.r')
source('R/count_motifs.r')
source('R/normalize_counts.r')
source('R/clustering.r')
source('R/plotting.r')

##########################################################
# Load Data
##########################################################
load('./R_objects/dfposts_podemos.Rda')
df.posts$date <- as.numeric(df.posts$date)
df.posts <- data.frame(df.posts) %>% arrange(date)
df.posts <- df.posts[1:75000,] # Paper
#df.posts <- df.posts[1:5000,] # Debug

df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"

# Print dates range
start.date <- as.POSIXct(min(as.numeric(df.posts$date)), origin = "1970-01-01") 
end.date <- as.POSIXct(max(as.numeric(df.posts$date)), origin = "1970-01-01")
print(paste("Start date:", start.date))
print(paste("End date:", end.date))

cat("Nunmber of posts:", nrow(df.posts))
cat('Number of threads: ', nrow(df.threads))
cat('Number of users: ', nrow(df.users))
cat('Number of active users', nrow(filter(df.users, posts>MIN_POSTS)))

#########################################################
# Compute neighborhood around every post
#########################################################
# Only long threads
#df.threads <- filter(df.threads, length>10)

chunks <- split(df.threads$thread, ceiling(seq_along(df.threads$thread)/1000))
length(chunks)

if(FALSE){
  
  chunks <- "t3_2bmb4v"
  
  # Profiling
  library(profvis)
  prof <- profvis({
    res.seq <- count_motifs_by_post(as.vector(unlist(chunks))[1], 
                                    database='reddit',
                                    neighbourhood='struct')
  })
  
  # sequential
  res.seq <- count_motifs_by_post(as.vector(unlist(chunks)), 
                                  database='reddit',
                                  neighbourhood='time')
}

# parallel
ncores <- detectCores() - 2
cl<-makeCluster(ncores, outfile="", port=11439)
registerDoParallel(cl)
pck <- c('RSQLite', 'data.table', 'changepoint', 'digest')
res.parallel <- foreach(i=1:length(chunks), .packages = pck)%dopar%{
  source('R/extract_from_db.r')
  count_motifs_by_post(chunks[[i]], 
                       database='reddit',
                       neighbourhood='time', chunk.id=i)
}
stopCluster(cl)
res <- merge.motif.counts(res.parallel)

save(res, file='./R_objects/res_time_75000_podemos.Rda') 
# save(res, file='./R_objects/res_order_75000_podemos.Rda') 
#save(res, file='./R_objects/res_struct_75000_podemos.Rda') 
# save(res, file='./R_objects/res_time_75000_gameofthrones.Rda') 
# save(res, file='./R_objects/res_order_75000_gameofthrones.Rda') 
#save(res, file='./R_objects/res_struct_75000_gameofthrones.Rda') 
# save(res, file='./R_objects/res_time_75000_4chan.Rda')
# save(res, file='./R_objects/res_order_75000_4chan.Rda')
#save(res, file='./R_objects/res_struct_75000_4chan.Rda')
#save(res, file='./R_objects/res_struct_10000_4chan.Rda')
#save(res, file='./R_objects/res_struct_5000_podemos.Rda')


#load("res_time_75000.Rda")

#save(res, file='./R_objects/res_order_2_4_75000_4chan.Rda') 

#save(res,file="res_time_75000_4chan.Rda")
#load("res_time_75000.Rda")

#save(res,file="res_order_2_4_75000_gameofthrones.Rda")
#load("res_order_2_4_75000_gameofthrones.Rda")

#load('res_2_4_order_75000.Rda') 

# Plot found motifs and their frequency
#plot.motif.counts(res)

#dev.copy(png, 'neighbourhoods_time.png')
#dev.off()
