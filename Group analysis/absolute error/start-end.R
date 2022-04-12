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
data2 <- data.frame()
start_end <- data.frame()
meanD <- data.frame()


for (i in 1:length(file_list)){
  I <- read.csv(file_list[i], header = TRUE)
  I <- I[, -1]
  ID <- file_list[i]
  Before <- head(I, 15)
  After <- tail(I, 15)
  sdO <- data.frame()
  for (k in 1:ncol(Before)){
    sd1 <- sd(Before[, k])
    sd2 <- sd(After[, k])
    sdO <- rbind(sdO, cbind(sd1, sd2))
  }
  meanD <- rbind(meanD, sdO)
  Before <- rowMeans(Before)
  After <- rowMeans(After)
  ttest <- t.test(Before, After)
  ttest2 <- t.test(sdO$sd1, sdO$sd2)
  dataO <- cbind(ID, ttest$statistic, ttest$p.value, ttest$stderr)
  dataO2 <- cbind(ID, ttest2$statistic, ttest2$p.value, ttest2$stderr)
  data <- rbind.data.frame(data, dataO)
  data2 <- rbind.data.frame(data2, dataO2)
  afterO <- cbind(ID, Before, After)
  start_end <- rbind.data.frame(start_end, afterO)
}
names(data) <- c("ID", "t-value", "p-value", "Std error")
write.csv(data, "start-endTtest.csv")
names(data2) <- c("ID", "t-value", "p-value", "Std error")

start_end <- start_end[, -1]

r5 <- cbind(rep("TT", 15), rep("Trial", 15), rep("True", 15), rep("with", 15))
r4 <- cbind(rep("TP", 15), rep("Trial", 15), rep("positive", 15), rep("with", 15))
r2 <- cbind(rep("BT", 15), rep("Block", 15), rep("True", 15), rep("with", 15))
r1 <- cbind(rep("BP", 15), rep("Block", 15), rep("positive", 15), rep("with", 15))
r3 <- cbind(rep("N", 15), rep("None", 15), rep("None", 15), rep("without", 15))

r55 <- cbind(rep("TT", as.numeric(rowlength[5,1])), rep("Trial", as.numeric(rowlength[5,1])), rep("True", as.numeric(rowlength[5,1])), rep("with", as.numeric(rowlength[5,1])))
r44 <- cbind(rep("TP", as.numeric(rowlength[4,1])), rep("Trial", as.numeric(rowlength[4,1])), rep("positive", as.numeric(rowlength[4,1])), rep("with", as.numeric(rowlength[4,1])))
r22 <- cbind(rep("BT", as.numeric(rowlength[2,1])), rep("Block", as.numeric(rowlength[2,1])), rep("True", as.numeric(rowlength[2,1])), rep("with", as.numeric(rowlength[2,1])))
r11 <- cbind(rep("BP", as.numeric(rowlength[1,1])), rep("Block", as.numeric(rowlength[1,1])), rep("positive", as.numeric(rowlength[1,1])), rep("with", as.numeric(rowlength[1,1])))
r33 <- cbind(rep("N", as.numeric(rowlength[3,1])), rep("None", as.numeric(rowlength[3,1])), rep("None", as.numeric(rowlength[3,1])), rep("without", as.numeric(rowlength[3,1])))

dataANOVA <- cbind.data.frame(rbind.data.frame(r1,r2,r3,r4,r5), start_end)
colnames(dataANOVA) <- data.frame("group", "frequency", "content","feedback", "before", "after")

dataANOVA2 <- cbind.data.frame(rbind.data.frame(r11,r22,r33,r44,r55), meanD)
colnames(dataANOVA2) <- data.frame("group", "frequency", "content","feedback", "before", "after")

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
  gather(key = "time", value = "error", after, before) %>%
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

ggplot(data = dataANOVA) +
  geom_boxplot(mapping=aes(group, error, colour=factor(time, levels=c("before","after")))) +
  theme(legend.position = "top", panel.background = element_rect(fill = "white", colour = "black")) +
  scale_colour_manual(values=c("dodgerblue3", "goldenrod2"))

ggplot(data = dataANOVA) +
  geom_boxplot(mapping=aes(frequency, error, colour=factor(time, levels=c("before","after")))) +
  theme(legend.position = "top", panel.background = element_rect(fill = "white", colour = "black")) +
  scale_colour_manual(values=c("dodgerblue3", "goldenrod2"))

ggplot(data = dataANOVA) +
  geom_boxplot(mapping=aes(content, error, colour=factor(time, levels=c("before","after")))) +
  theme(legend.position = "top", panel.background = element_rect(fill = "white", colour = "black")) +
  scale_colour_manual(values=c("dodgerblue3", "goldenrod2"))

ggplot(data = dataANOVA) +
  geom_boxplot(mapping=aes(feedback, error, colour=factor(time, levels=c("before","after")))) +
  theme(legend.position = "top", panel.background = element_rect(fill = "white", colour = "black")) +
  scale_colour_manual(values=c("dodgerblue3", "goldenrod2"))


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

write.csv(dataANOVA2, "ANOVA2.csv")
dataANOVA2 <- read.csv("ANOVA2.csv", header = TRUE)

data3 <- dataANOVA2 %>%
  gather(key = "time", value = "sd", after, before) %>%
  convert_as_factor(group, time)
data3 %>%
  group_by(group, time) %>%
  get_summary_stats(sd, type = "mean_sd")

interaction6 <- aov(sd ~ frequency*time, data = data3)
summary(interaction6)

