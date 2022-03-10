setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/TT individual")
library("ggplot2")
library("BayesFactor")
data <- read.csv("individual data.csv", header = TRUE)

plot(data$trials, data$rate)

# get the fitting line
fit <- lm(log(rate) ~ trials, data=data)
summary(fit)

# plot the data
plot.trials <- ggplot(data,aes(trials, rate, group=blocks)) + 
  geom_point() + 
  geom_smooth(method=lm, formula=y~log(x))
plot.trials

plot.blocks <- ggplot(data,aes(block, rateS, group=blockS)) + 
  geom_point() + 
  geom_smooth(formula=y~log(x))
plot.blocks

# multiple participants in one graph
#ggplot() + 
#  geom_point(data=data, mapping=aes(trials, rate)) + 
#  geom_point(data=data, mapping=aes(trials, rate.1)) + 
#  geom_point(data=data, mapping=aes(trials, rate.2))

             