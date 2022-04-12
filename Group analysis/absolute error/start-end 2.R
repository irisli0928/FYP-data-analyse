setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/absolute error")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")
library("rstatix")
library("MBESS")

file_list <- list.files(pattern = ".*accuracy.*.csv")
data <- data.frame()
start_end <- data.frame()
rowlength <- data.frame()



for (i in 1:length(file_list)){
  I <- read.csv(file_list[i], header = TRUE)
  I <- I[, -1]
  ID <- file_list[i]
  Before <- head(I, 15)
  After <- tail(I, 15)
  Before <- colMeans(Before)
  After <- colMeans(After)
  ttest <- t.test(Before, After)
  dataO <- cbind(ID, ttest$statistic, ttest$p.value, ttest$stderr)
  data <- rbind.data.frame(data, dataO)
  afterO <- cbind(ID, Before, After)
  start_end <- rbind.data.frame(start_end, afterO)
  rowlength <- rbind(rowlength, nrow(afterO))
}
names(data) <- c("ID", "t-value", "p-value", "Std error")
write.csv(data, "start-endTtest2.csv")

r55 <- cbind(rep("TT", as.numeric(rowlength[5,1])), rep("Trial", as.numeric(rowlength[5,1])), rep("True", as.numeric(rowlength[5,1])), rep("with", as.numeric(rowlength[5,1])))
r44 <- cbind(rep("TP", as.numeric(rowlength[4,1])), rep("Trial", as.numeric(rowlength[4,1])), rep("positive", as.numeric(rowlength[4,1])), rep("with", as.numeric(rowlength[4,1])))
r22 <- cbind(rep("BT", as.numeric(rowlength[2,1])), rep("Block", as.numeric(rowlength[2,1])), rep("True", as.numeric(rowlength[2,1])), rep("with", as.numeric(rowlength[2,1])))
r11 <- cbind(rep("BP", as.numeric(rowlength[1,1])), rep("Block", as.numeric(rowlength[1,1])), rep("positive", as.numeric(rowlength[1,1])), rep("with", as.numeric(rowlength[1,1])))
r33 <- cbind(rep("N", as.numeric(rowlength[3,1])), rep("None", as.numeric(rowlength[3,1])), rep("None", as.numeric(rowlength[3,1])), rep("without", as.numeric(rowlength[3,1])))

start_end <- start_end[, -1]

dataANOVA <- cbind.data.frame(rbind.data.frame(r11,r22,r33,r44,r55), start_end)
colnames(dataANOVA) <- data.frame("group", "frequency", "content","feedback", "before", "after")

write.csv(dataANOVA, "ANOVAmean2.csv")
dataANOVA <- read.csv("ANOVAmean2.csv", header = TRUE)

dataANOVA <- dataANOVA %>%
  gather(key = "time", value = "error", before, after) %>%
  convert_as_factor(group, time)

dataANOVA %>%
  group_by(group, time) %>%
  get_summary_stats(error, type = "mean_sd")
ggboxplot(dataANOVA, x = "group", y = "error",
          color = "time", palette = "jco")

interaction2 <- aov(error ~ group*time, data = dataANOVA)
summary(interaction2)
interaction3 <- aov(error ~ frequency*time, data = dataANOVA)
summary(interaction3)
interaction4 <- aov(error ~ content*time, data = dataANOVA)
summary(interaction4)
interaction5 <- aov(error ~ feedback*time, data = dataANOVA)
summary(interaction5)

blocking <- aov(error ~ time+content+frequency, data = dataANOVA)
summary(blocking)

dataANOVA %>%
  group_by(feedback, time) %>%
  get_summary_stats(error, type = "mean_sd")
ggboxplot(dataANOVA, x = "frequency", y = "error",
          color = "time", palette = "jco")
sink("meanComparisonANOVA2.txt")
print(dataANOVA %>%
        group_by(group, time) %>%
        get_summary_stats(error, type = "mean_sd"))
print(dataANOVA %>%
        group_by(frequency, time) %>%
        get_summary_stats(error, type = "mean_sd"))
print(dataANOVA %>%
        group_by(content, time) %>%
        get_summary_stats(error, type = "mean_sd"))
print(dataANOVA %>%
        group_by(feedback, time) %>%
        get_summary_stats(error, type = "mean_sd"))
print(summary(interaction2))
print(summary(interaction3))
print(summary(interaction4))
print(summary(interaction5))
sink()
