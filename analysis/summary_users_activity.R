# General statistics to better chose the top users
# ##################################################
library(dplyr)
library(data.table)
library(reshape2)
library(parallel)
library(doParallel)
library(foreach)
library(ggplot2)

subforums <- c("MachineLearning",
               "TwoXChromosomes",
               "france",  
               "podemos",
               "gameofthrones")

deleted <- c('AutoModerator', '[deleted]')



cl = makeCluster(5, outfile="",  port=11439) # outfile "" makes it printing on the screen
registerDoParallel(cl)
df <- foreach(i = 1:length(subforums), .combine=rbind, .packages='dplyr') %dopar% {
  
  # Load the entire dataframe of posts
  subforum <- subforums[i]
  load(paste0('data/df.posts.', subforum, '.rda'))
  cat('\nsubforum:', subforum )
  
  # Filter short threads and count user threads
  # then leave only threads with some active user that has participated in at least 100 threads
  df.users <- df.posts %>% 
    group_by(thread) %>% mutate(size = n()) %>% ungroup %>% filter(size>0) %>% # remove short threads
    group_by(user) %>% 
    summarise(nthreads = length(unique(thread)), 
              nposts = n()) %>%
    filter(! user %in% deleted) %>%
    arrange(desc(nposts), desc(nthreads), user) %>%
    mutate(user.rank = match(user, unique(user)),
           subforum = subforum) %>% 
    ungroup
}
stopCluster(cl)
df <- df %>% as.data.frame

df %>% group_by(subforum) %>% filter(nposts==100) %>% as.data.frame

g <- ggplot(filter(df, user.rank<100), aes(x=user.rank, y=nposts, color=subforum)) +
    geom_point()+ geom_line() +
    facet_grid(subforum ~ ., scale='free')+
    ylim(c(0,1000))+
    theme_bw()
print(g)

