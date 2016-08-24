library(ggplot2)

# Original data
df.test <- data.frame(value=floor(rexp(10000, 1/2)))

# Compute the frequency of every value
# or the probability
freqs <- tabulate(df.test$value)
probs <- freqs / sum(freqs)

# Create a new dataframe with the frequencies (or probabilities)
df.freqs <- data.frame(n=1:length(freqs), freq=freqs, probs=probs) 

# Plot them, usually in log-log
g <- ggplot(df.freqs, aes(x=n, y = freq)) + geom_point() + 
  scale_y_log10() + scale_x_log10()
plot(g)


g <- ggplot(df.test, aes(x = value)) + geom_histogram() + 
  scale_y_log10() + scale_x_log10()
plot(g)