setwd("F:/Tools/Rworkspace/FYP-data-analysis/TP")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")

file_list <- list.files(pattern = "*.csv")
length(file_list)

distance <- data.frame(1:180)
accuracy <- data.frame(1:180)


for (i in 1:length(file_list)){
  I <- read.csv(file_list[i], header = TRUE)
  dataD <- na.omit(data.frame(I[, "start_distance"]))
  dataA <- na.omit(data.frame(I[, "accuracy"]))
  names(dataD) <- c(paste("distance_", I[1, "participant"]))
  names(dataA) <- c(paste("accuracy_", I[1, "participant"]))
  distance <- cbind.data.frame(distance, dataD)
  accuracy <- cbind.data.frame(accuracy, dataA)
}

distance <- distance[, -1]
accuracy <- accuracy[, -1]

write.csv(distance, "F:/Tools/Rworkspace/FYP-data-analysis/TP/output/distanceTP.csv")
write.csv(accuracy, "F:/Tools/Rworkspace/FYP-data-analysis/TP/output/accuracyTP.csv")
