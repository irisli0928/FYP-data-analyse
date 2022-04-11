setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/error comparison")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")

distanceI <- read.csv("distanceBP.csv", header = TRUE)
accuracyI <- read.csv("accuracyBP.csv", header = TRUE)

datafit <- data.frame()

for (i in 2:ncol(distanceI)){
  ID <- colnames(distanceI[i])
  distance <- abs(distanceI[, i])
  accuracy <- accuracyI[, i]
  plot <- ggplot(mapping = aes(distance, accuracy)) +
    geom_point() + 
    geom_smooth(method = lm, formula=y~x) +
    geom_smooth(method = lm, formula=y~exp(x)) +
    geom_smooth(method=loess, formula=y~x, colour="orange") +
    ylab("error")
  ggsave(plot, filename=paste("c_", ID, ".png",sep=""),device="png", path="F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/error comparison/BP")
  fit <- lm(accuracy ~ distance)
  dataO <- cbind(paste("c_", ID, sep=""), t(as.numeric(coefficients(fit))),t(as.numeric(summary(fit)$coefficients[, 2])), t(as.numeric(summary(fit)$coefficients[, 4])), t(as.numeric(summary(fit)$r.squared)))
  datafit <- rbind.data.frame(datafit, dataO)
}
names(datafit) <- c("ID", paste("coeff", names(coefficients(fit))), paste("Std. Error", names(summary(fit)$coefficients[, 2])), paste("P-value", names(summary(fit)$coefficients[, 4])), "R-squared")

write.csv(datafit, "BP.csv")
