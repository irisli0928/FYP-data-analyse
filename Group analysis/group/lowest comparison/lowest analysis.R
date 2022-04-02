setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/group/lowest comparison")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")

lengthTT <- read.csv("endpointTT.csv", header = TRUE, colClasses=c("NULL", NA))
lengthTP <- read.csv("endpointTP.csv", header = TRUE, colClasses=c("NULL", NA))
lengthBT <- read.csv("endpointBT.csv", header = TRUE, colClasses=c("NULL", NA))
lengthBP <- read.csv("endpointBP.csv", header = TRUE, colClasses=c("NULL", NA))
lengthN <- read.csv("endpointN.csv", header = TRUE, colClasses=c("NULL", NA))

cname1 <- cbind(rep("TT", nrow(lengthTT)), rep("Trial", nrow(lengthTT)), rep("True", nrow(lengthTT)))
cname2 <- cbind(rep("TP", nrow(lengthTP)), rep("Trial", nrow(lengthTP)), rep("Positive", nrow(lengthTP)))
cname3 <- cbind(rep("BT", nrow(lengthBT)), rep("Block", nrow(lengthBT)), rep("True", nrow(lengthBT)))
cname4 <- cbind(rep("BP", nrow(lengthBP)), rep("Block", nrow(lengthBP)), rep("Positive", nrow(lengthBP)))
cname5 <- cbind(rep("N", nrow(lengthN)), rep("None", nrow(lengthN)), rep("None", nrow(lengthN)))
datalength <- cbind(rbind(cname1,cname2,cname3,cname4,cname5), rbind(lengthTT,lengthTP,lengthBT,lengthBP,lengthN))
colnames(datalength) <- data.frame("group", "frequency", "content", "length")

one.way <- aov(length ~ group, data = datalength)
summary(one.way)
interaction <- aov(length ~ frequency*content, data=datalength)
summary(interaction)

tukey.plot.aov<-aov(length ~ frequency:content, data = datalength)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
par(mar=c(4.1, 12, 4.1, 2.1))
plot(tukey.plot.test, las = 1, mar = 0.1)

datalength %>%
  group_by(group) %>%
  get_summary_stats(length, type = "mean_sd")
ggboxplot(datalength, x = "group", y = "length")

sink("lowest point length.txt")
print(summary(one.way))
print(summary(interaction))
print(datalength %>%
        group_by(group) %>%
        get_summary_stats(length, type = "mean_sd"))
sink()


rateTT <- read.csv("fitTT.csv", header = TRUE,)
rateTP <- read.csv("fitTP.csv", header = TRUE)
rateBT <- read.csv("fitBT.csv", header = TRUE)
rateBP <- read.csv("fitBP.csv", header = TRUE)
rateN <- read.csv("fitN.csv", header = TRUE)

lograteTT <- as.numeric(rateTT[, "coeff.trial" ])
write.csv(lograteTT, "TT.csv")
lograteTT <- read.csv("TT.csv", header=TRUE, colClasses=c("NULL", NA))
lograteTP <- as.numeric(rateTP[, "coeff.trial" ])
write.csv(lograteTP, "TP.csv")
lograteTP <- read.csv("TP.csv", header=TRUE, colClasses=c("NULL", NA))
lograteBT <- as.numeric(rateBT[, "coeff.trial" ])
write.csv(lograteBT, "BT.csv")
lograteBT <- read.csv("BT.csv", header=TRUE, colClasses=c("NULL", NA))
lograteBP <- as.numeric(rateBP[, "coeff.trial",])
write.csv(lograteBP, "BP.csv")
lograteBP <- read.csv("BP.csv", header=TRUE, colClasses=c("NULL", NA))
lograteN <- as.numeric(rateN[, "coeff.trial" ])
write.csv(lograteN, "N.csv")
lograteN <- read.csv("N.csv", header=TRUE, colClasses=c("NULL", NA))

cname1 <- cbind(rep("TT", nrow(lograteTT)), rep("Trial", nrow(lograteTT)), rep("True", nrow(lograteTT)))
cname2 <- cbind(rep("TP", nrow(lograteTP)), rep("Trial", nrow(lograteTP)), rep("Positive", nrow(lograteTP)))
cname3 <- cbind(rep("BT", nrow(lograteBT)), rep("Block", nrow(lograteBT)), rep("True", nrow(lograteBT)))
cname4 <- cbind(rep("BP", nrow(lograteBP)), rep("Block", nrow(lograteBP)), rep("Positive", nrow(lograteBP)))
cname5 <- cbind(rep("N", nrow(lograteN)), rep("None", nrow(lograteN)), rep("None", nrow(lograteN)))
datarate <- data.frame(cbind(rbind(cname1,cname2,cname3,cname4,cname5), rbind(lograteTT,lograteTP,lograteBT,lograteBP,lograteN)))
colnames(datarate) <- data.frame("group", "frequency", "content", "rate")

one.way <- aov(rate ~ group, data = datarate)
summary(one.way)
interaction <- aov(rate ~ frequency*content, data=datarate)
summary(interaction)

tukey.plot.aov<-aov(rate ~ frequency:content, data = datarate)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
par(mar=c(4.1, 12, 4.1, 2.1))
plot(tukey.plot.test, las = 1, mar = 0.1)

datarate %>%
  group_by(group) %>%
  get_summary_stats(rate, type = "mean_sd")
ggboxplot(datarate, x = "group", y = "rate")

sink("lowest point rate.txt")
print(summary(one.way))
print(summary(interaction))
print(datarate %>%
        group_by(group) %>%
        get_summary_stats(rate, type = "mean_sd"))
sink()


