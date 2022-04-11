setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/absolute error")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")

file_list <- list.files(pattern = ".*accuracy.*.csv")
data <- data.frame()

# for (i in 1:length(file_list)){
#   I <- read.csv(file_list[i], header = TRUE)
#   for (k in 2:ncol(I)){
#     K <- I[, k]
#     ID <- file_list[i]
#     Before <- head(K, 15)
#     After <- tail(K, 15)
#     ttest <- t.test(Before, After)
#     dataO <- cbind(ID, ttest$statistic, ttest$p.value, ttest$stderr)
#     data <- rbind.data.frame(data, dataO)
#   }
# }

afterM <- data.frame()

for (i in 1:length(file_list)){
  I <- read.csv(file_list[i], header = TRUE)
  ID <- file_list[i]
  Before <- rowMeans(head(I, 15))
  After <- rowMeans(tail(I, 15))
  ttest <- t.test(Before, After)
  dataO <- cbind(ID, ttest$statistic, ttest$p.value, ttest$stderr)
  data <- rbind.data.frame(data, dataO)
  afterO <- cbind(ID, After)
  afterM <- rbind.data.frame(afterM, afterO)
}
names(data) <- c("ID", "t-value", "p-value", "Std error")

afterM <- afterM[, 2]

r5 <- cbind(rep("TT", 15), rep("Trial", 15), rep("True", 15), rep("with", 15))
r4 <- cbind(rep("TP", 15), rep("Trial", 15), rep("positive", 15), rep("with", 15))
r2 <- cbind(rep("BT", 15), rep("Block", 15), rep("True", 15), rep("with", 15))
r1 <- cbind(rep("BP", 15), rep("Block", 15), rep("positive", 15), rep("with", 15))
r3 <- cbind(rep("N", 15), rep("None", 15), rep("None", 15), rep("without", 15))

dataANOVA <- cbind.data.frame(rbind.data.frame(r1,r2,r3,r4,r5), afterM)
colnames(dataANOVA) <- data.frame("group", "frequency", "content","feedback", "meansA")

write.csv(dataANOVA, "ANOVAmean.csv")
dataANOVA <- read.csv("ANOVAmean.csv", header = TRUE)

one.way <- aov(meansA ~ group, data = dataANOVA)
summary(one.way)
one.way2 <- aov(meansA ~ feedback, data = dataANOVA)
summary(one.way2)
interaction <- aov(meansA ~ frequency*content, data = dataANOVA)
summary(interaction)

dataANOVA %>%
  group_by(group) %>%
  get_summary_stats(meansA, type = "mean_sd")
ggboxplot(dataANOVA, x = "group", y = "meansA")

dataANOVA %>%
  group_by(feedback) %>%
  get_summary_stats(meansA, type = "mean_sd")
ggboxplot(dataANOVA, x = "feedback", y = "meansA")

dataANOVA %>%
  group_by(frequency) %>%
  get_summary_stats(meansA, type = "mean_sd")
ggboxplot(dataANOVA, x = "frequency", y = "meansA")


write.csv(data, "ttestAverage.csv")

