setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/absolute error")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")

file_list <- list.files(pattern = ".*fit.*.csv")
rowlength <- data.frame()
data <- data.frame()
rate <- vector()

for (i in 1:length(file_list)){
  I <- read.csv(file_list[i], header = TRUE)
  rate <- I[, 3]
  dim(rate) <- c(length(rate), 1)
  data <- rbind.data.frame(data, rate)
  rowlength <- rbind(rowlength, length(rate))
}



r5 <- cbind(rep("Trial true", as.numeric(rowlength[5,1])), rep("Trial", as.numeric(rowlength[5,1])), rep("True", as.numeric(rowlength[5,1])), rep("with", as.numeric(rowlength[5,1])))
r4 <- cbind(rep("Trial positive", as.numeric(rowlength[4,1])), rep("Trial", as.numeric(rowlength[4,1])), rep("positive", as.numeric(rowlength[4,1])), rep("with", as.numeric(rowlength[4,1])))
r2 <- cbind(rep("Block true", as.numeric(rowlength[2,1])), rep("Block", as.numeric(rowlength[2,1])), rep("True", as.numeric(rowlength[2,1])), rep("with", as.numeric(rowlength[2,1])))
r1 <- cbind(rep("Block positive", as.numeric(rowlength[1,1])), rep("Block", as.numeric(rowlength[1,1])), rep("positive", as.numeric(rowlength[1,1])), rep("with", as.numeric(rowlength[1,1])))
r3 <- cbind(rep("No feedback", as.numeric(rowlength[3,1])), rep("None", as.numeric(rowlength[3,1])), rep("None", as.numeric(rowlength[3,1])), rep("without", as.numeric(rowlength[3,1])))

dataANOVA <- cbind.data.frame(rbind.data.frame(r1,r2,r3,r4,r5), data)
colnames(dataANOVA) <- data.frame("group", "frequency", "content","feedback", "learningR")


one.way <- aov(learningR ~ group, data = dataANOVA)
summary(one.way)
one.way2 <- aov(learningR ~ feedback, data = dataANOVA)
summary(one.way2)
interaction <- aov(learningR ~ frequency*content, data = dataANOVA)
summary(interaction)

dataANOVA %>%
  group_by(group) %>%
  get_summary_stats(learningR, type = "mean_sd")
ggboxplot(dataANOVA, x = "group", y = "learningR")

dataANOVA %>%
  group_by(feedback) %>%
  get_summary_stats(learningR, type = "mean_sd")
ggboxplot(dataANOVA, x = "feedback", y = "learningR")
ggboxplot(dataANOVA, x = "content", y = "learningR")
ggboxplot(dataANOVA, x = "frequency", y = "learningR")

tukey.plot.aov <- aov(learningR ~ frequency:content, data = dataANOVA)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
par(mar=c(4.1, 12, 4.1, 2.1))
plot(tukey.plot.test, las = 1, mar = 0.1)

sink("linearANOVA.txt")
print(dataANOVA %>%
        group_by(group) %>%
        get_summary_stats(learningR, type = "mean_sd"))
print(dataANOVA %>%
        group_by(feedback) %>%
        get_summary_stats(learningR, type = "mean_sd"))
print(dataANOVA %>%
        group_by(frequency) %>%
        get_summary_stats(learningR, type = "mean_sd"))
print(dataANOVA %>%
        group_by(content) %>%
        get_summary_stats(learningR, type = "mean_sd"))
print(summary(one.way))
print(summary(one.way2))
print(summary(interaction))
sink()
