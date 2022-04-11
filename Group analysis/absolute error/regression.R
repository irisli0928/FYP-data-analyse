setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/absolute error")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")

accuracyI <- read.csv("accuracyN.csv", header = TRUE)
datafit <- data.frame()
datafit2<- data.frame()

for (i in 2:ncol(accuracyI)){
  ID <- colnames(accuracyI[i])
  trial <- 1:180
  accuracy <- accuracyI[, i]
  plot <- ggplot(mapping = aes(trial, accuracy)) +
    geom_point() + 
    geom_smooth(method = lm, formula=y~x) +
    geom_smooth(method = lm, formula=log(y)~x, colour="orange") +
    ylab("error")
  ggsave(plot, filename=paste(ID, ".png",sep=""),device="png", path="F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/absolute error/N")
  fit <- lm(accuracy ~ trial)
  # fit2 <- lm(accuracy~log(trial))
  fit2 <- lm(log(accuracy)~trial)
  dataO <- cbind(ID, t(as.numeric(coefficients(fit))),t(as.numeric(summary(fit)$coefficients[, 2])), t(as.numeric(summary(fit)$coefficients[, 4])), t(as.numeric(summary(fit)$r.squared)))
  dataO2 <- cbind(ID, t(as.numeric(coefficients(fit2))),t(as.numeric(summary(fit2)$coefficients[, 2])), t(as.numeric(summary(fit2)$coefficients[, 4])), t(as.numeric(summary(fit2)$r.squared)))
  datafit <- rbind.data.frame(datafit, dataO)
  datafit2 <- rbind.data.frame(datafit2, dataO2)
}
names(datafit) <- c("ID", paste("coeff", names(coefficients(fit))), paste("Std. Error", names(summary(fit)$coefficients[, 2])), paste("P-value", names(summary(fit)$coefficients[, 4])), "R-squared")
names(datafit2) <- c("ID", paste("coeff", names(coefficients(fit2))), paste("Std. Error", names(summary(fit2)$coefficients[, 2])), paste("P-value", names(summary(fit2)$coefficients[, 4])), "R-squared")

write.csv(datafit, "Nlinear.csv")
write.csv(datafit2, "Nlog.csv")
