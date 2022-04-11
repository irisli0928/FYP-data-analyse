setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/absolute error")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")

file_list <- list.files(pattern = ".*accuracy.*.csv")
rowlength <- data.frame()
data <- data.frame()


for (i in 1:length(file_list)){
  I <- read.csv(file_list[i], header = TRUE)
  means <- colMeans(I)
  means <- means[2:length(means)]
  dim(means) <- c(length(means), 1)
  data <- rbind.data.frame(data, means)
  rowlength <- rbind(rowlength, length(means))
}

r1 <- cbind(rep("TT", as.numeric(rowlength[5,1])), rep("Trial", as.numeric(rowlength[5,1])), rep("True", as.numeric(rowlength[5,1])), rep("with", as.numeric(rowlength[5,1])))
r2 <- cbind(rep("TP", as.numeric(rowlength[4,1])), rep("Trial", as.numeric(rowlength[4,1])), rep("positive", as.numeric(rowlength[4,1])), rep("with", as.numeric(rowlength[4,1])))
r3 <- cbind(rep("BT", as.numeric(rowlength[2,1])), rep("Block", as.numeric(rowlength[2,1])), rep("True", as.numeric(rowlength[2,1])), rep("with", as.numeric(rowlength[2,1])))
r4 <- cbind(rep("BP", as.numeric(rowlength[1,1])), rep("Block", as.numeric(rowlength[1,1])), rep("positive", as.numeric(rowlength[1,1])), rep("with", as.numeric(rowlength[1,1])))
r5 <- cbind(rep("N", as.numeric(rowlength[3,1])), rep("None", as.numeric(rowlength[3,1])), rep("None", as.numeric(rowlength[3,1])), rep("without", as.numeric(rowlength[3,1])))

dataANOVA <- cbind.data.frame(rbind.data.frame(r1,r2,r3,r4,r5), data)
colnames(dataANOVA) <- data.frame("group", "frequency", "content","feedback", "meanP")


one.way <- aov(meanP ~ group, data = dataANOVA)
summary(one.way)
one.way2 <- aov(meanP ~ feedback, data = dataANOVA)
summary(one.way2)
interaction <- aov(meanP ~ frequency*content, data = dataANOVA)
summary(interaction)


dataANOVA %>%
  group_by(group) %>%
  get_summary_stats(meanP, type = "mean_sd")
ggboxplot(dataANOVA, x = "group", y = "meanP")

dataANOVA %>%
  group_by(feedback) %>%
  get_summary_stats(meanP, type = "mean_sd")
ggboxplot(dataANOVA, x = "feedback", y = "meanP")

tukey.plot.aov <- aov(meanP ~ frequency:content, data = dataANOVA)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
par(mar=c(4.1, 12, 4.1, 2.1))
plot(tukey.plot.test, las = 1, mar = 0.1)

sink("ANOVA.txt")
print(dataANOVA %>%
        group_by(group) %>%
        get_summary_stats(meanP, type = "mean_sd"))
print(dataANOVA %>%
        group_by(feedback) %>%
        get_summary_stats(meanP, type = "mean_sd"))
print(summary(one.way))
print(summary(one.way2))
print(summary(interaction))
sink()
