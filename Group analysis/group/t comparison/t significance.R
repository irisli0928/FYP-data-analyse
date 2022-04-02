setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/group/t comparison")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")


dataTT <- read.csv("TTtResult.csv", header = TRUE)
TTsignificant <-  vector()
for (i in 1:nrow(dataTT)){
  ID <- dataTT[i, 2]
  if (dataTT[i, 6] >= 1) {
    TTsignificant <- rbind(TTsignificant, dataTT[i, 3])
  }
}
r1 <- cbind(rep("TT", nrow(TTsignificant)), rep("Trial", nrow(TTsignificant)), rep("True", nrow(TTsignificant)))

dataTP <- read.csv("TPtResult.csv", header = TRUE)
TPsignificant <- vector()
for (i in 1:nrow(dataTP)){
  ID <- dataTP[i, 2]
  if (dataTP[i, 6] >= 1) {
    TPsignificant <- rbind(TPsignificant, dataTP[i, 3])
  }
}
r2 <- cbind(rep("TP", nrow(TPsignificant)), rep("Trial", nrow(TPsignificant)), rep("Positive", nrow(TPsignificant)))

dataBT <- read.csv("BTtResult.csv", header = TRUE)
BTsignificant <- vector()
for (i in 1:nrow(dataBT)){
  ID <- dataBT[i, 2]
  if (dataBT[i, 6] >= 1) {
    BTsignificant <- rbind(BTsignificant, dataBT[i, 3])
  }
}
r3 <- cbind(rep("BT", nrow(BTsignificant)), rep("Block", nrow(BTsignificant)), rep("True", nrow(BTsignificant)))

dataBP <- read.csv("BPtResult.csv", header = TRUE)
BPsignificant <- vector()
for (i in 1:nrow(dataBP)){
  ID <- dataBP[i, 2]
  if (dataBP[i, 6] >= 1) {
    BPsignificant <- rbind(BPsignificant, dataBP[i, 3])
  }
}
r4 <- cbind(rep("BP", nrow(BPsignificant)), rep("Block", nrow(BPsignificant)), rep("Positive", nrow(BPsignificant)))

dataN <- read.csv("NtResult.csv", header = TRUE)
Nsignificant <- vector()
for (i in 1:nrow(dataN)){
  ID <- dataN[i, 2]
  if (dataN[i, 6] >= 1) {
    Nsignificant <- rbind(Nsignificant, dataN[i, 3])
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

t2 <- t.test(BPsignificant, TPsignificant)

