setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/TT individual")
library("ggplot2")
library("BayesFactor")
data <- read.csv("individual data.csv", header = TRUE)

I <- data$X68965
blockma <- matrix(I, ncol = 15, byrow = TRUE)
block_average <- apply(blockma, 1, mean)
block <- 1:12
trial <- 1:180
logI <- log(I)
logB <- log(block_average)
data1 <- data.frame(trial, block, block_average, logI, logB)

# logrithmic regression for trials and block average
fitT <- lm(log(I) ~ trials, data=data)
summary(fitT)
bfT <- regressionBF(logI ~ trial, data=data1)
extractBF(bfT)
fitB <- lm(log(block_average) ~ block)
summary(fitB)
bfB <- regressionBF(logB ~ block, data=data1)
extractBF(bfB)

plot.trial <- ggplot(data,aes(trials, I)) + 
  geom_point() + 
  geom_smooth(method=lm, formula=y~log(x))
plot.trial
plot.block <- ggplot(mapping = aes(block, block_average)) + 
  geom_point() + 
  geom_smooth(method=lm, formula=y~log(x))
plot.block
