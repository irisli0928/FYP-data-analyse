setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/group")
library("ggplot2")
library("BayesFactor")
library("XLConnect")

dataTTlog <- read.csv("TTtrialLog.csv", header = TRUE)
dataTTlinear <- read.csv("TTtrialLinear.csv", header = TRUE)
dataTPlog <- read.csv("TPtrialLog.csv", header = TRUE)
dataTPlinear <- read.csv("TPtrialLinear.csv", header = TRUE)
dataBTlog <- read.csv("BTtrialLog.csv", header = TRUE)
dataBTlinear <- read.csv("BTtrialLinear.csv", header = TRUE)
dataBPlog <- read.csv("BPtrialLog.csv", header = TRUE)
dataBPlinear <- read.csv("BPtrialLinear.csv", header = TRUE)
dataNlog <- read.csv("NtrialLog.csv", header = TRUE)
dataNlinear <- read.csv("NtrialLinear.csv", header = TRUE)


rownames(dataTTlog) <- dataTTlog[, 1]
dataTTlog <- dataTTlog[, -1]
means <- rowMeans(dataTTlog, na.rm = TRUE)
dataTTlog <- cbind(dataTTlog, means)
rownames(dataTTlinear) <- dataTTlinear[, 1]
dataTTlinear <- dataTTlinear[, -1]
means <- rowMeans(dataTTlinear, na.rm = TRUE)
dataTTlinear <- cbind(dataTTlinear, means)

rownames(dataTPlog) <- dataTPlog[, 1]
dataTPlog <- dataTPlog[, -1]
means <- rowMeans(dataTPlog, na.rm = TRUE)
dataTPlog <- cbind(dataTPlog, means)
rownames(dataTPlinear) <- dataTPlinear[, 1]
dataTPlinear <- dataTPlinear[, -1]
means <- rowMeans(dataTPlinear, na.rm = TRUE)
dataTPlinear <- cbind(dataTPlinear, means)

rownames(dataBTlog) <- dataBTlog[, 1]
dataBTlog <- dataBTlog[, -1]
means <- rowMeans(dataBTlog, na.rm = TRUE)
dataBTlog <- cbind(dataBTlog, means)
rownames(dataBTlinear) <- dataBTlinear[, 1]
dataBTlinear <- dataBTlinear[, -1]
means <- rowMeans(dataBTlinear, na.rm = TRUE)
dataBTlinear <- cbind(dataBTlinear, means)

rownames(dataBPlog) <- dataBPlog[, 1]
dataBPlog <- dataBPlog[, -1]
means <- rowMeans(dataBPlog, na.rm = TRUE)
dataBPlog <- cbind(dataBPlog, means)
rownames(dataBPlinear) <- dataBPlinear[, 1]
dataBPlinear <- dataBPlinear[, -1]
means <- rowMeans(dataBPlinear, na.rm = TRUE)
dataBPlinear <- cbind(dataBPlinear, means)

rownames(dataNlog) <- dataNlog[, 1]
dataNlog <- dataNlog[, -1]
means <- rowMeans(dataNlog, na.rm = TRUE)
dataNlog <- cbind(dataNlog, means)
rownames(dataNlinear) <- dataNlinear[, 1]
dataNlinear <- dataNlinear[, -1]
means <- rowMeans(dataNlinear, na.rm = TRUE)
dataNlinear <- cbind(dataNlinear, means)


lograteTT <- as.numeric(dataTTlog["coeff trials", ])
loginterceptTT <- as.numeric(dataTTlog["coeff (Intercept)", ])
lograteTP <- as.numeric(dataTPlog["coeff trials", ])
loginterceptTP <- as.numeric(dataTPlog["coeff (Intercept)", ])
lograteBT <- as.numeric(dataBTlog["coeff trials", ])
loginterceptBT <- as.numeric(dataBTlog["coeff (Intercept)", ])
lograteBP <- as.numeric(dataBPlog["coeff trials", ])
loginterceptBP <- as.numeric(dataBPlog["coeff (Intercept)", ])
lograteN <- as.numeric(dataNlog["coeff trials", ])
loginterceptN <- as.numeric(dataNlog["coeff (Intercept)", ])
# hist(lograteTT) 
# hist(loginterceptTT)
# hist(lograteTP) 
# hist(loginterceptTP)
# hist(lograteBT)
# hist(loginterceptBT)
# hist(lograteBP)
# hist(loginterceptBP)
# hist(lograteN)
# hist(loginterceptN)

dataANOVAi <- data.frame(loginterceptTT = rep(NA, max(sapply(list(loginterceptTT, loginterceptTP, loginterceptBT, loginterceptBP, loginterceptN), length))))
dataANOVAi[1:length(loginterceptTT), 1] <- loginterceptTT
dataANOVAi[1:length(loginterceptTP), 2] <- loginterceptTP
dataANOVAi[1:length(loginterceptBT), 3] <- loginterceptBT
dataANOVAi[1:length(loginterceptBP), 4] <- loginterceptBP
dataANOVAi[1:length(loginterceptN), 5] <- loginterceptN
write.csv(dataANOVAi, "ANOVAi.csv", row.names=FALSE, na=" ")


#merging vectors with different length to create data frame for ANOVA
dataANOVA <- data.frame(lograteTT = rep(NA, max(sapply(list(lograteTT, lograteTP, lograteBT, lograteBP, lograteN), length))))
dataANOVA[1:length(lograteTT), 1] <- lograteTT
dataANOVA[1:length(lograteTP), 2] <- lograteTP
dataANOVA[1:length(lograteBT), 3] <- lograteBT
dataANOVA[1:length(lograteBP), 4] <- lograteBP
dataANOVA[1:length(lograteN), 5] <- lograteN
# dataANOVA <- list(lograteTT=lograteTT, lograteTP=lograteTP, lograteBT=lograteBT, lograteBP=lograteBP, lograteN=lograteN)
# attributes(dataANOVA) = list(names = names(dataANOVA),
#                              row.names=1:max(length(lograteTT), length(lograteTP), length(lograteBT), length(lograteBP), length(lograteN)), class='data.frame')
# dataANOVA <- data.frame(dataANOVA)
write.csv(dataANOVA, "ANOVA.csv", row.names=FALSE, na=" ")

# lograteTT <- dataTTlog["coeff trials", "means"]
# loginterceptTT <- dataTTlog["coeff (Intercept)", "means"]
# lograteTP <- dataTPlog["coeff trials", "means"]
# loginterceptTP <- dataTPlog["coeff (Intercept)", "means"]
# lograteBT <- dataBTlog["coeff trials", "means"]
# loginterceptBT <- dataBTlog["coeff (Intercept)", "means"]
# lograteBP <- dataBPlog["coeff trials", "means"]
# loginterceptBP <- dataBPlog["coeff (Intercept)", "means"]
# lograteN <- dataNlog["coeff trials", "means"]
# loginterceptN <- dataNlog["coeff (Intercept)", "means"]



linearrateTT <- dataTTlinear["coeff trial", "means"]
linearinterceptTT <- dataTTlinear["coeff (Intercept)", "means"]
linearrateTP <- dataTPlinear["coeff trial", "means"]
linearinterceptTP <- dataTPlinear["coeff (Intercept)", "means"]
linearrateBT <- dataBTlinear["coeff trial", "means"]
linearinterceptBT <- dataBTlinear["coeff (Intercept)", "means"]
linearrateBP <- dataBPlinear["coeff trial", "means"]
linearinterceptBP <- dataBPlinear["coeff (Intercept)", "means"]
linearrateN <- dataNlinear["coeff trial", "means"]
linearinterceptN <- dataNlinear["coeff (Intercept)", "means"]

