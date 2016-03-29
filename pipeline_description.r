library(dplyr)
library(reshape2)
library(igraph)
library(ggplot2)

source('R/load_participations.r')

MIN_POSTS <- 100 # number of post to consider a user as active
###################################################
# Load data
###################################################
#df.posts <- load_posts(database='reddit', forum='podemos')
#save(df.posts,file="dfposts.Rda")
load('dfposts.Rda')
df.posts <- as.data.frame(df.posts)
df.posts <- data.frame(df.posts) %>% arrange(date)
df.threads <- plyr::count(df.posts, "thread")
df.users <- plyr::count(df.posts, 'user')                                                                                                                                   
names(df.threads)[2] <- "length"
names(df.users)[2] <- "posts"
df.posts$date <- as.numeric(df.posts$date)

# Plot general overview of forum
n <- hist(df.threads$length, breaks=0:max(df.threads$length), plot=FALSE)$counts
plot(1:max(df.threads$length), n, log='xy', 
     ylab='Number of threads', xlab='Length')
title(main='Threads length')

plot(cumsum(table(df.threads$length)), pch=19, cex=0.5,
     ylab='Length (cum)', xlab='Thread')
title('Threads length (cumulative)')

n <- hist(df.users$posts, breaks=0:max(df.users$posts), plot=FALSE)$counts
plot(1:max(df.users$posts), n, log='xy', 
     ylab='Number of users', xlab='Posts')
title(main='Users posts')

plot(cumsum(table(df.users$posts)), pch=19, cex=0.5,
     ylab='Posts (cum)', xlab='User')
title('Users posts (cumulative)')

# How are the delta times between posts?
#######################################
# delta to previous post in thread
df.posts <- arrange(df.posts, thread, date) %>% 
                              group_by(thread) %>% 
                              mutate(rank = rank(date, ties.method = "first"), 
                                     delta = c(NA, diff(date))) %>% as.data.frame
# delta to parent
df.posts <- mutate(df.posts, parent.date=df.posts$date[match(df.posts$parent, df.posts$postid)])
df.posts <- mutate(df.posts, delta.to.parent=date-parent.date)

# Plot
par(mfrow=c(1,1))
df.posts <- arrange(df.posts, date)
plot(df.posts$delta, pch=19, cex=0.5)
title("Time to last post in thread")

# plot by thread
plot(match(df.posts$thread, unique(df.posts$thread)), 
     df.posts$delta,
     xlab='Thread', ylab='Seconds', ylim=c(0,3600*12),
     pch=19, cex=0.5)
abline(h=3600*24*30, col='red')
abline(h=3600*24*60, col='red')
abline(h=3600*24*90, col='red')
abline(h=3600*24*120, col='red')
abline(h=3600*24*150, col='red')
title("Time to last post in thread (by thread)")

# same with density estimation (not super useful)
#df.posts_ <- df.posts[complete.cases(df.posts),] %>%
#             filter(delta<200*1)
#den <- kde2d(match(df.posts_$thread, unique(df.posts_$thread)),
#      df.posts_$delta)
#filled.contour(den)

# plot delta to parent
plot(match(df.posts$thread, unique(df.posts$thread)),
     df.posts$delta.to.parent)
abline(h=3600*24*30, col='red')
abline(h=3600*24*60, col='red')
abline(h=3600*24*90, col='red')
abline(h=3600*24*120, col='red')
abline(h=3600*24*150, col='red')
title("Delta (to parent) by thread")

# Delta probability distribution
deltas <- filter(df.posts, !is.na(delta.to.parent))$delta.to.parent
n <- hist(deltas, breaks=0:max(deltas), plot=FALSE)$counts
plot(1:max(deltas), n, log='xy', ylab='Number of posts', xlab='Delta', pch=19, cex=0.5)
title(main='Posts deltas to parent (log-log)')


deltas <- filter(df.posts, !is.na(delta))$delta
n <- hist(deltas, breaks=0:max(deltas), plot=FALSE)$counts
plot(1:max(deltas), n, log='xy', ylab='Number of posts', xlab='Delta', pch=19, cex=0.5)
title(main='Posts deltas (log-log)')

#density for better aesthetics
#den <- kde2d(1:max(deltas), n)
#filled.contour(den)

# Delta cumsums
plot(cumsum(table(df.posts$delta.to.parent)), pch=19, cex=0.5,
     ylab='Delta (cum)', xlab='Post')
title('Posts deltas (cumulative)')

# Delta (to parent) by user
# group by user and then compute mean and avg
# Only active users
df.posts <- filter(df.posts, complete.cases(df.posts))
df.posts.act <- merge(filter(count(df.posts, user), n>100), df.posts, all.y=FALSE)

df.users_deltas <- select(df.posts.act, user, delta.to.parent) 
df.users_deltas.summarized <- group_by(df.users_deltas, user)  %>%
                summarise(mean=mean(delta.to.parent), sd=sd(delta.to.parent), q50=quantile(delta.to.parent, 0.5)) %>%
                arrange(mean) %>%
                as.data.frame
df.users_deltas <- merge(df.users_deltas.summarized, df.users_deltas, by="user", sort=FALSE)

# one way to plot it
plot(df.users_deltas.summarized$mean)
arrows(1:nrow(df.users_deltas.summarized), 
       df.users_deltas.summarized$mean-df.users_deltas.summarized$sd, 
       1:nrow(df.users_deltas.summarized), 
       df.users_deltas.summarized$mean+df.users_deltas.summarized$sd, 
       code=3, angle=90, length=0.1)  

# another way to plot it
plot(df.users_deltas.summarized$mean, df.users_deltas.summarized$sd,
     xlab='Mean', ylab='Sd', pch=19, cex=0.5)
title('Deltas by user')

plot(df.users_deltas.summarized$q50, df.users_deltas.summarized$sd,
     xlab='Median', ylab='Sd', pch=19, cex=0.5)
title('Deltas by user')

plot(df.users_deltas.summarized$mean, df.users_deltas.summarized$sd,
     xlab='Mean', ylab='Sd', pch=19, cex=0.5,
     xlim=c(0,3600*24), ylim=c(0,3600*24))

plot(df.users_deltas.summarized$q50, df.users_deltas.summarized$sd,
     xlab='Median', ylab='Sd', pch=19, cex=0.5,
     xlim=c(0,3600*24), ylim=c(0,3600*24))
title('Deltas by user')

# Ensenyar estas graficas a Bertrand
# conclusion: los mas predecibles son los mas rapidos de media
# pero parece que en general todos son rapidos, solo que algunos tienen posts outliers