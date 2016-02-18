head(beav1, 100)
data(beav1)
beav1$day <- as.factor(beav1$day)
beav1[beav1$day==346,]$time <- 1:sum(beav1$day==346)
beav1[beav1$day==347,]$time <- 1:sum(beav1$day==347)
beav1 <- filter(beav1, time<23)
ggplot(beav1, aes(x=time, y=temp, group=day, fill=day, color=day)) + stat_summary(fun.y = mean, na.rm=TRUE, geom='line')

ggplot(beav1, aes(x=time, y=temp, group=day, fill=day, color=day)) + 
  geom_line()

data(beav1)
beav1$day <- as.factor(beav1$day)
beav1[beav1$day==346,]$time <- 1:sum(beav1$day==346)
beav1[beav1$day==347,]$time <- 1:sum(beav1$day==347)
beav1 <- filter(beav1, time<23)
ggplot(beav1, aes(x=time, y=temp, group=day, fill=day, color=day)) + 
  stat_summary(fun.y = mean, na.rm=TRUE, geom='line')