setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/TT individual")
library("ggplot2")
library("BayesFactor")
data <- read.csv("individual data.csv", header = TRUE)

plot(data$trials, data$rate)
plot(data$blocks, data$rateB)

# get the fitting line
fit <- lm(log(X67060) ~ trials, data=data)
summary(fit)

fitB <- lm(log(rateB) ~ blocks, data=data)
summary(fitB)

# plot the data
plot.trials <- ggplot(data,aes(trials, X69060)) + 
  geom_point() + 
  ylim(0,1) + 
  geom_smooth(formula=y~log(x))
plot.trials

plot.blocks <- ggplot(data,aes(blocks, rateB)) + 
  geom_point() + 
  ylim(0,1.5) +
  geom_smooth(formula=y~log(x))
plot.blocks

