setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/TT individual")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
data <- read.csv("individual data.csv", header = TRUE)

for(i in 5:ncol(data)) {
  ID <- colnames(data[i])
  I <- data[ , i]
  blockma <- matrix(I, ncol = 15, byrow = TRUE)
  block_average <- apply(blockma, 1, mean)
  block <- 1:12
  trial <- 1:180
  logI <- log(I)
  logB <- log(block_average)
  data1 <- data.frame(trial, block, block_average, logI, logB)
  plot.trial <- ggplot(data,aes(trials, I)) + 
    geom_line(colour="gray") +
    geom_point() +
    geom_smooth(method=lm, formula=y~log(x), colour="blue", fullrange=FALSE, level=.01) +
    geom_smooth(method=lm, formula=y~x, colour="black", fullrange=FALSE, level=.01) +
    geom_smooth(method=loess, formula=y~log(x), colour="green", fullrange=FALSE, level=.01)
  plot.block <- ggplot(mapping = aes(block, block_average)) + 
    geom_point() + 
    geom_smooth(method=lm, formula=y~log(x))
  plot.block
  ggsave(plot.trial, filename=paste("trial", ID, ".png",sep=""),device="png", path="F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/TT individual/plot/trial")
  ggsave(plot.block, filename=paste("block", ID, ".png",sep=""),device="png", path="F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/TT individual/plot/block")
}
  

# logrithmic regression for trials and block average
fitT <- lm(log(I) ~ trials, data=data)
summary(fitT)
bfT <- regressionBF(logI ~ trial, data=data1)
extractBF(bfT)
fitB <- lm(log(block_average) ~ block)
summary(fitB)
bfB <- regressionBF(logB ~ block, data=data1)
extractBF(bfB)

