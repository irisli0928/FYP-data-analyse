setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/group")
library("ggplot2")
library("BayesFactor")
library("XLConnect")

dataTTlog <- read.csv("TTblockLog.csv", header = TRUE)
dataTTlinear <- read.csv("TTblockLinear.csv", header = TRUE)
dataTPlog <- read.csv("TPblockLog.csv", header = TRUE)
dataTPlinear <- read.csv("TPblockLinear.csv", header = TRUE)
dataBTlog <- read.csv("BTblockLog.csv", header = TRUE)
dataBTlinear <- read.csv("BTblockLinear.csv", header = TRUE)
dataBPlog <- read.csv("BPblockLog.csv", header = TRUE)
dataBPlinear <- read.csv("BPblockLinear.csv", header = TRUE)
dataNlog <- read.csv("NblockLog.csv", header = TRUE)
dataNlinear <- read.csv("NblockLinear.csv", header = TRUE)


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

lograteTT <- dataTTlog["coeff trial", "means"]
loginterceptTT <- dataTTlog["coeff (Intercept)", "means"]
lograteTP <- dataTPlog["coeff trial", "means"]
loginterceptTP <- dataTPlog["coeff (Intercept)", "means"]
lograteBT <- dataBTlog["coeff trial", "means"]
loginterceptBT <- dataBTlog["coeff (Intercept)", "means"]
lograteBP <- dataBPlog["coeff trial", "means"]
loginterceptBP <- dataBPlog["coeff (Intercept)", "means"]
lograteN <- dataNlog["coeff trial", "means"]
loginterceptN <- dataNlog["coeff (Intercept)", "means"]

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