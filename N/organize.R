setwd("F:/Tools/Rworkspace/FYP-data-analysis/N")
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
distance <- abs(distance)
accuracy <- accuracy[, -1]

accuracyO <- data.frame(1:180)
data <- data.frame()
for (k in 1:ncol(distance)){
  d1 <- distance[, k]
  a1 <- accuracy[, k]
  aO <- vector()
  for (j in 1:length(d1)){
    if (d1[j]>=40){
      aO <- rbind(aO, a1[j])
    }
  }
  trial <- 1:length(aO)
  fit <- lm(aO ~ trial)
  dataO <- cbind(t(as.numeric(coefficients(fit))),t(as.numeric(summary(fit)$coefficients[, 2])), t(as.numeric(summary(fit)$coefficients[, 4])), t(as.numeric(summary(fit)$r.squared)))
  length(aO) <- 180
  data <- rbind(data, dataO)
  accuracyO <- cbind.data.frame(accuracyO, aO)
}
names(data) <- c(paste("coeff", names(coefficients(fit))), paste("Std. Error", names(summary(fit)$coefficients[, 2])), paste("P-value", names(summary(fit)$coefficients[, 4])), "R-squared")
write.csv(data, "F:/Tools/Rworkspace/FYP-data-analysis/N/output/fit.csv")

write.csv(distance, "F:/Tools/Rworkspace/FYP-data-analysis/N/output/distanceN.csv")
write.csv(accuracy, "F:/Tools/Rworkspace/FYP-data-analysis/N/output/accuracyN.csv")
