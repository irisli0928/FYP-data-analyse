setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/group/lowest comparison")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")

dataTT <- read.csv("TTtResult.csv", header = TRUE)
dataTP <- read.csv("TPtResult.csv", header = TRUE)
dataBT <- read.csv("BTtResult.csv", header = TRUE)
dataBP <- read.csv("BPtResult.csv", header = TRUE)
dataN <- read.csv("NtResult.csv", header = TRUE)

TT <- as.numeric(dataTT[ ,"t.value"])
TP <- as.numeric(dataTP[ ,"t.value"])
BT <- as.numeric(dataBT[ ,"t.value"])
BP <- as.numeric(dataBP[ ,"t.value"])
N <- as.numeric(dataN[ ,"t.value"])
dim(TT) <- c(length(TT), 1)
dim(TP) <- c(length(TP), 1)
dim(BT) <- c(length(BT), 1)
dim(BP) <- c(length(BP), 1)
dim(N) <- c(length(N), 1)

#create row names for dataset
r1 <- cbind(rep("TT", length(TT)), rep("Trial", length(TT)), rep("True", length(TT)))
r2 <- cbind(rep("TP", length(TP)), rep("Trial", length(TP)), rep("positive", length(TP)))
r3 <- cbind(rep("BT", length(BT)), rep("Block", length(BT)), rep("True", length(BT)))
r4 <- cbind(rep("BP", length(BP)), rep("Block", length(BP)), rep("positive", length(BP)))
r5 <- cbind(rep("N", length(N)), rep("None", length(N)), rep("None", length(N)))

#organize for ANOVA
dataANOVA <- cbind.data.frame(rbind.data.frame(r1,r2,r3,r4,r5), rbind(TT,TP,BT,BP,N))
colnames(dataANOVA) <- data.frame("group", "frequency", "content", "t")

#ANOVA
oneway <- aov(t ~ group, data = dataANOVA)
summary(oneway)
interaction <- aov(t ~ frequency*content, data = dataANOVA)
summary(interaction)

tukey.plot.aov<-aov(t ~ frequency:content, data = dataANOVA)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
par(mar=c(4.1, 12, 4.1, 2.1))
plot(tukey.plot.test, las = 1, mar = 0.1)

dataANOVA %>%
  group_by(group) %>%
  get_summary_stats(t, type = "mean_sd")
ggboxplot(dataANOVA, x = "group", y = "t")
