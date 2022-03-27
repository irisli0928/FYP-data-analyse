setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/group")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")


dataTTlog <- read.csv("TTtrialLog.csv", header = TRUE)
TTsignificant <-  vector()
for (i in 2:ncol(dataTTlog)){
  ID <- colnames(dataTTlog[i])
  if (dataTTlog[6, i] <= 0.05) {
    TTsignificant <- rbind(TTsignificant, dataTTlog[2, i])
  }
}
r1 <- cbind(rep("TT", nrow(TTsignificant)), rep("Trial", nrow(TTsignificant)), rep("True", nrow(TTsignificant)))

dataTPlog <- read.csv("TPtrialLog.csv", header = TRUE)
TPsignificant <- vector()
for (i in 2:ncol(dataTPlog)){
  ID <- colnames(dataTPlog[i])
  if (dataTPlog[6, i] <= 0.05) {
    TPsignificant <- rbind(TPsignificant, dataTPlog[2, i])
  }
}
r2 <- cbind(rep("TP", nrow(TPsignificant)), rep("Trial", nrow(TPsignificant)), rep("Positive", nrow(TPsignificant)))

dataBTlog <- read.csv("BTtrialLog.csv", header = TRUE)
BTsignificant <- vector()
for (i in 2:ncol(dataBTlog)){
  ID <- colnames(dataBTlog[i])
  if (dataBTlog[6, i] <= 0.05) {
    BTsignificant <- rbind(BTsignificant, dataTTlog[2, i])
  }
}
r3 <- cbind(rep("BT", nrow(BTsignificant)), rep("Block", nrow(BTsignificant)), rep("True", nrow(BTsignificant)))

dataBPlog <- read.csv("BPtrialLog.csv", header = TRUE)
BPsignificant <- vector()
for (i in 2:ncol(dataBPlog)){
  ID <- colnames(dataBPlog[i])
  if (dataBPlog[6, i] <= 0.05) {
    BPsignificant <- rbind(BPsignificant, dataTTlog[2, i])
  }
}
r4 <- cbind(rep("BP", nrow(BPsignificant)), rep("Block", nrow(BPsignificant)), rep("Positive", nrow(BPsignificant)))

dataNlog <- read.csv("NtrialLog.csv", header = TRUE)
Nsignificant <- vector()
for (i in 2:ncol(dataNlog)){
  ID <- colnames(dataNlog[i])
  if (dataNlog[6, i] <= 0.05) {
    Nsignificant <- rbind(Nsignificant, dataTTlog[2, i])
  }
}
r5 <- cbind(rep("N", nrow(Nsignificant)), rep("None", nrow(Nsignificant)), rep("None", nrow(Nsignificant)))

dataANOVA <- cbind.data.frame(rbind.data.frame(r1,r2,r3,r4,r5), rbind(TTsignificant,TPsignificant,BTsignificant,BPsignificant,Nsignificant))
cnames <- data.frame("group", "frequency", "feedback", "learningR")
colnames(dataANOVA) <- cnames

#ANOVA
oneway <- aov(learningR ~ group, data = dataANOVA)
summary(oneway)
interaction <- aov(learningR ~ frequency*feedback, data = dataANOVA)
summary(interaction)

tukey.plot.aov<-aov(learningR ~ frequency:feedback, data = dataANOVA)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
par(mar=c(4.1, 12, 4.1, 2.1))
plot(tukey.plot.test, las = 1, mar = 0.1)

dataANOVA %>%
  group_by(group) %>%
  get_summary_stats(learningR, type = "mean_sd")
ggboxplot(dataANOVA, x = "group", y = "learningR")


