setwd("F:/Tools/Rworkspace/FYP-data-analysis/TT individual")
library("ggplot2")
library("BayesFactor")
data <- read.csv("67060.csv", header = TRUE)

plot(data$trials, data$rate)
plot(data$blocks, data$rateB)

# get the fitting line
fit <- lm(log(rate) ~ trials, data=data)
summary(fit)

fitB <- lm(log(rateB) ~ blocks, data=data)
summary(fitB)

# plot the data
plot.trials <- ggplot(data,aes(trials, rate, group=trialSegment)) + 
  geom_point() + 
  ylim(0,1) + 
  geom_smooth(formula=y~log(x))
plot.trials

plot.blocks <- ggplot(data,aes(blocks, rateB, group=blockSegment)) + 
  geom_point() + 
  ylim(0,1.5) +
  geom_smooth(formula=y~log(x))
plot.blocks
