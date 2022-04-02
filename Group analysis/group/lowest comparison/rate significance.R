setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/group/lowest comparison")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")

dataTT <- read.csv("fitTT.csv", header = TRUE,)
dataTP <- read.csv("fitTP.csv", header = TRUE)
dataBT <- read.csv("fitBT.csv", header = TRUE)
dataBP <- read.csv("fitBP.csv", header = TRUE)
dataN <- read.csv("fitN.csv", header = TRUE)



TTsignificant <-  vector()
for (i in 1:nrow(dataTT)){
  ID <- dataTT[i, 2]
  if (dataTT[i, 8] <= 0.05) {
    TTsignificant <- rbind(TTsignificant, dataTT[i, 4])
  }
}
r1 <- cbind(rep("TT", nrow(TTsignificant)), rep("Trial", nrow(TTsignificant)), rep("True", nrow(TTsignificant)))


TPsignificant <- vector()
for (i in 1:nrow(dataTP)){
  ID <- dataTP[i, 2]
  if (dataTP[i, 8] <= 0.05) {
    TPsignificant <- rbind(TPsignificant, dataTP[i, 4])
  }
}
r2 <- cbind(rep("TP", nrow(TPsignificant)), rep("Trial", nrow(TPsignificant)), rep("Positive", nrow(TPsignificant)))


BTsignificant <- vector()
for (i in 1:nrow(dataBT)){
  ID <- dataBT[i, 2]
  if (dataBT[i, 8] <= 0.05) {
    BTsignificant <- rbind(BTsignificant, dataBT[i, 4])
  }
}
r3 <- cbind(rep("BT", nrow(BTsignificant)), rep("Block", nrow(BTsignificant)), rep("True", nrow(BTsignificant)))


BPsignificant <- vector()
for (i in 1:nrow(dataBP)){
  ID <- dataBP[i, 2]
  if (dataBP[i, 8] <= 0.05) {
    BPsignificant <- rbind(BPsignificant, dataBP[i, 4])
  }
}
r4 <- cbind(rep("BP", nrow(BPsignificant)), rep("Block", nrow(BPsignificant)), rep("Positive", nrow(BPsignificant)))


Nsignificant <- vector()
for (i in 1:nrow(dataN)){
  ID <- dataN[i, 2]
  if (dataN[i, 8] <= 0.05) {
    Nsignificant <- rbind(Nsignificant, dataN[i, 4])
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

sink("lowest sig comparison.txt")
print(summary(oneway))
print(summary(interaction))
print(dataANOVA %>%
        group_by(group) %>%
        get_summary_stats(learningR, type = "mean_sd"))
sink()

