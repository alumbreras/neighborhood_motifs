# Extract the neighborhood census of a forum
# and plot its evolution
# author: Alberto Lumbreras
#################################################
library(dplyr)
library(reshape2)
library(ggplot2)
source('R/load_participations.r')
source('R/count_motifs.r')
select <- dplyr::select # avoid confusion with MASS function

###################################################
# Load data
###################################################
df.posts <- load_posts(database='reddit', forum='podemos')
df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"

######################################################
# Extract neighborhood around every post
######################################################
res <- count_motifs_by_post(df.threads$thread, database='reddit')
df.post.motif  <- res$posts.motifs
motifs <- res$motifs

# Sort by frequency
idx <- order(table(df.post.motif$motif), decreasing = TRUE)
motifs <- motifs[idx]
df.post.motif$motif <- match(df.post.motif$motif, idx)

df.posts <- merge(df.posts, df.post.motif, all.x=TRUE, sort=FALSE)

####################################################
# Plot neighborhoods by frequency
####################################################
mypalette <- c("black", "red", "white")
par(mfrow=c(3,5))
for(i in 1:length(motifs)){
  gmotif <- as.undirected(motifs[[i]])
  la = layout_as_tree(gmotif, mode='out', root=which.min(V(gmotif)$date))
  plot(gmotif,
       layout = la,
       vertex.color=mypalette[V(motifs[[i]])$color],
       vertex.label = "",
       edge.arrow.size=0.6)
  title(paste(i),sub=sum(df.post.motif$motif==i))  
}


################################################
# Plot cumsum with sliding window
# Note: lay with both times, real and sequential
################################################
df.census <- arrange(df.posts, date) %>%
             acast(date~motif) %>%
             as.data.frame

# chronological dates
df.census$date <- rownames(df.census)

# sequential dates
df.census$date <- 1:nrow(df.census)

steps <- seq(min(as.numeric(df.census$date)),
         max(as.numeric(df.census$date)), 
         by=3600)

df.census.window <- vector()
for(i in 1:(length(steps)-1)){
  df.census.window <- filter(df.census, date>steps[i] & date<steps[i+1]) %>%
                      select(-date) %>%
                      colSums %>%
                      rbind(df.census.window, .)
                      #rbind(df.census.window) # TODO rbind(df.census.window, .) ?
}

df.census.window <- as.data.frame(df.census.window)
df.census.window$date <- head(steps,-1)
df.census.window <- melt(df.census.window, id.vars=c("date"))

df.census.window$date <- as.POSIXct(df.census.window$date, origin="1970-01-01")

# Plot absolute values to view general trend, periodicity...
ggplot(df.census.window, aes(x=date, y=value, group=variable, color=variable, fill=variable)) + 
  geom_line()

# Plot evolution of census (%) with total number of posts
ggplot(df.census.window, aes(x=date, y=value)) + 
  geom_bar(aes(group=variable, color=variable, fill=variable), 
           position = 'fill', stat='identity') + 
  stat_summary(fun.y=function(x){sum(x)/200}, color = 'black', geom ='line')


#########################################################
# Plot relationship between length and census
#########################################################

# Compute rank of every post in its thread
# compute cumsum per group (per thread)

df.census <- arrange(df.posts, thread, date) %>%
             dcast(thread+date~motif)
             
df.census.length <- vector()
ranks <- vector()
for(i in 1:nrow(df.census)){
  cat('\n', i)
  ranks <- c(ranks, sum(df.census[1:i, c('thread')]==df.census$thread[i])) # store also rank of post in thread
  df.census.length <- filter(df.census[1:i,], thread==df.census$thread[i]) %>%
                      select(-thread, -date) %>%
                      colSums %>%
                      rbind(df.census.length, .) %>%
                      as.data.frame
}       
#df.census.length <- df.census.length.bck
df.census.length$rank <- ranks
df.census.length$date <- df.census$date
df.census.length$thread <- df.census$thread

# Select a subsample of threads to plot
# ... at random
sample.threads <-  sample(unique(df.census.length$thread), 10)

# by length
sample.threads <- filter(df.threads, length>100) %>% select(thread) 
sample.threads <- sample.threads$thread

df.census.length.sample <- filter(df.census.length, thread %in% sample.threads)
df.census.length.sample <- melt(df.census.length.sample, id.vars=c('date', 'rank', 'thread'))

ggplot(filter(df.census.length.sample, variable==1), aes(x=rank, y=value, group=thread, color=thread)) + geom_line()

p <- ggplot(filter(df.census.length.sample, variable %in% c(1,2,3,4,5,6,7,8)), aes(x=rank, y=value, group=thread, color=thread)) + geom_line()
#p + facet_grid(~ variable)
p + facet_wrap(~ variable, ncol=4, nrow=4)

################################################
# Can we predict length based on first census
################################################

# merge census matrix with thread length