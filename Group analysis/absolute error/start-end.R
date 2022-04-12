setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/absolute error")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")
library("rstatix")

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

start_end <- data.frame()

for (i in 1:length(file_list)){
  I <- read.csv(file_list[i], header = TRUE)
  I <- I[, -1]
  ID <- file_list[i]
  Before <- rowMeans(head(I, 15))
  After <- rowMeans(tail(I, 15))
  ttest <- t.test(Before, After)
  dataO <- cbind(ID, ttest$statistic, ttest$p.value, ttest$stderr)
  data <- rbind.data.frame(data, dataO)
  afterO <- cbind(ID, Before, After)
  start_end <- rbind.data.frame(start_end, afterO)
}
names(data) <- c("ID", "t-value", "p-value", "Std error")
write.csv(data, "start-endTtest.csv")

start_end <- start_end[, -1]

r5 <- cbind(rep("TT", 15), rep("Trial", 15), rep("True", 15), rep("with", 15))
r4 <- cbind(rep("TP", 15), rep("Trial", 15), rep("positive", 15), rep("with", 15))
r2 <- cbind(rep("BT", 15), rep("Block", 15), rep("True", 15), rep("with", 15))
r1 <- cbind(rep("BP", 15), rep("Block", 15), rep("positive", 15), rep("with", 15))
r3 <- cbind(rep("N", 15), rep("None", 15), rep("None", 15), rep("without", 15))

dataANOVA <- cbind.data.frame(rbind.data.frame(r1,r2,r3,r4,r5), start_end)
colnames(dataANOVA) <- data.frame("group", "frequency", "content","feedback", "before", "after")

write.csv(dataANOVA, "ANOVAmean.csv")
dataANOVA <- read.csv("ANOVAmean.csv", header = TRUE)

# one.way <- aov(error ~ group, data = dataANOVA)
# summary(one.way)
# one.way2 <- aov(meansA ~ feedback, data = dataANOVA)
# summary(one.way2)
# interaction <- aov(error ~ frequency*content, data = dataANOVA)
# summary(interaction)
# 
# dataANOVA %>%
#   group_by(group) %>%
#   get_summary_stats(error, type = "mean_sd")
# ggboxplot(dataANOVA, x = "group", y = "error")
# 
# dataANOVA %>%
#   group_by(feedback) %>%
#   get_summary_stats(meansA, type = "mean_sd")
# ggboxplot(dataANOVA, x = "feedback", y = "meansA")
# 
# dataANOVA %>%
#   group_by(frequency) %>%
#   get_summary_stats(meansA, type = "mean_sd")
# ggboxplot(dataANOVA, x = "frequency", y = "meansA")
# 
# write.csv(data, "ttestAverage.csv")



#avergae performance
dataANOVA <- dataANOVA %>%
  gather(key = "time", value = "error", before, after) %>%
  convert_as_factor(group, time)

dataANOVA %>%
  group_by(group, time) %>%
  get_summary_stats(error, type = "mean_sd")
ggboxplot(dataANOVA, x = "content", y = "error",
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


# data2 <- rbind.data.frame(dataANOVA[16:30, ], dataANOVA[61:75, ], dataANOVA[91:105, ], dataANOVA[136:150, ])
# interaction3 <- aov(error ~ group*time, data = data2)
# summary(interaction3)

dataANOVA %>%
  group_by(feedback, time) %>%
  get_summary_stats(error, type = "mean_sd")
ggboxplot(dataANOVA, x = "feedback", y = "error",
          color = "time", palette = "jco")
sink("meanComparisonANOVA.txt")
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
