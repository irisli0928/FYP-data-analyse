setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/error comparison")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")

file_listA <- list.files(pattern = ".*accuracy.*.csv")
file_listB <- list.files(pattern = ".*distance.*.csv")

data <- data.frame()

for (i in 1:length(file_listA)){
  accuracyALL <- read.csv(file_listA[i], header = TRUE)[, 2:5]
  distanceALL <- read.csv(file_listB[i], header = TRUE)[, 2:5]
  for (k in 1:ncol(accuracyALL)){
    accuracy <- accuracyALL[, k]
    distance <- abs(distanceALL[, k])
    dataO <- cbind(accuracy, distance)
    data <- rbind.data.frame(data, dataO)
  }
}

plot <- ggplot(mapping = aes(distance, accuracy), data = data) +
  geom_point() + 
  geom_smooth(method = lm, formula=y~x) +
  geom_smooth(method=loess, formula=y~x, colour="orange") +
  ylab("error")
ggsave(plot, filename=paste("all2", ".png",sep=""),device="png", path="F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/error comparison/plot all")


correlation <- cor.test(distance, accuracy, method=c("pearson", "kendall", "spearman"), data=data)
