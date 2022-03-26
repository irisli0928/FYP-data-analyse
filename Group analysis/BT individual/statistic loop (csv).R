setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/BT individual")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
data <- read.csv("individual data.csv", header = TRUE)

# logarithmic regression for trial average
dataTiralSave <- data.frame()#create a data frame for saving output
dataBlockSave <- data.frame()
for(i in 4:ncol(data)) {
  ID <- colnames(data[i]) #extract column name
  I <- data[ , i] #extract data to a single vector by column number
  blockma <- matrix(I, ncol = 15, byrow = TRUE) #create a matrix for calculating average of every n rows in the vector
  block_average <- apply(blockma, 1, mean) #calculating average
  block <- 1:12
  trial <- 1:180
  logI <- log(I)
  logB <- log(block_average)
  data1 <- data.frame(trial, block, block_average, logI, logB) #save all needed var into dataframe
  fitT <- lm(log(I) ~ trials, data=data)
  fitB <- lm(log(block_average) ~ block)
  bfT <- regressionBF(logI ~ trial, data=data1)
  bfB <- regressionBF(logB ~ block, data=data1)
  #save output in this loop, each number in a seperate column
  dataO <- cbind(ID, t(as.numeric(coefficients(fitT))),t(as.numeric(summary(fitT)$coefficients[, 2])), t(as.numeric(summary(fitT)$coefficients[, 4])), t(as.numeric(summary(fitT)$r.squared)), t(as.numeric(extractBF(bfT)$bf)))
  dataO1 <- cbind(ID, t(as.numeric(coefficients(fitB))),t(as.numeric(summary(fitB)$coefficients[, 2])), t(as.numeric(summary(fitB)$coefficients[, 4])), t(as.numeric(summary(fitB)$r.squared)), t(as.numeric(extractBF(bfB)$bf)))
  #save output in every loop into one data frame by adding rows
  dataTiralSave <- rbind(dataTiralSave, dataO)
  dataBlockSave <- rbind(dataBlockSave, dataO1)
}
names(dataTiralSave) <- c("ID", paste("coeff", names(coefficients(fitT))), paste("Std. Error", names(summary(fitT)$coefficients[, 2])), paste("P-value", names(summary(fitT)$coefficients[, 4])), "R-squared", "BayesFactor")
dataTiralSave <- as.data.frame(t(dataTiralSave)) #swith column and row
colnames(dataTiralSave) <- dataTiralSave[1,] #use first row as column name
dataTiralSave <- dataTiralSave[-1, ] #delete first row
write.csv(dataTiralSave, "BTtrialLog.csv", row.names = TRUE)
names(dataBlockSave) <- c("ID", paste("coeff", names(coefficients(fitB))), paste("Std. Error", names(summary(fitB)$coefficients[, 2])), paste("P-value", names(summary(fitB)$coefficients[, 4])), "R-squared", "BayesFactor")
dataBlockSave <- as.data.frame(t(dataBlockSave))
colnames(dataBlockSave) <- dataBlockSave[1,]
dataBlockSave <- dataBlockSave[-1, ] 
write.csv(dataBlockSave, "BTblocklog.csv", row.names = TRUE)


#linear regression
LinearSaveT <- data.frame()
LinearSaveB <- data.frame()
for(i in 4:ncol(data)){
  ID <- colnames(data[i])
  I <- data[ , i]
  trial <- 1:180
  blockma <- matrix(I, ncol = 15, byrow = TRUE)
  block_average <- apply(blockma, 1, mean)
  block <- 1:12
  data2 <- data.frame(I, trial, block, block_average)
  linearT <- lm(I~trial, data=data2)
  linearB <- lm(block_average~block, data=data2)
  bfLinearT <- regressionBF(I ~ trial, data=data2)
  bfLinearB <- regressionBF(block_average~block, data=data2)
  dataO <- cbind(ID, t(as.numeric(coefficients(linearT))), t(as.numeric(summary(linearT)$coefficients[, 2])), t(as.numeric(summary(linearT)$coefficients[, 4])), t(as.numeric(summary(linearT)$r.squared)), t(as.numeric(extractBF(bfLinearT)$bf)))
  dataO1 <- cbind(ID, t(as.numeric(coefficients(linearB))), t(as.numeric(summary(linearB)$coefficients[, 2])), t(as.numeric(summary(linearB)$coefficients[, 4])), t(as.numeric(summary(linearB)$r.squared)), t(as.numeric(extractBF(bfLinearB)$bf)))
  LinearSaveT <- rbind(LinearSaveT, dataO)
  LinearSaveB <- rbind(LinearSaveB, dataO1)
}
names(LinearSaveT) <- c("ID", paste("coeff", names(coefficients(linearT))), paste("Std. Error", names(summary(linearT)$coefficients[, 2])), paste("P-value", names(summary(linearT)$coefficients[, 4])), "R-squared", "BayesFactor")
LinearSaveT <- as.data.frame(t(LinearSaveT))
colnames(LinearSaveT) <- LinearSaveT[1,]
LinearSaveT <- LinearSaveT[-1, ] 
write.csv(LinearSaveT, "BTtrialLinear.csv", row.names = TRUE)
names(LinearSaveB) <- c("ID", paste("coeff", names(coefficients(linearB))), paste("Std. Error", names(summary(linearB)$coefficients[, 2])), paste("P-value", names(summary(linearB)$coefficients[, 4])), "R-squared", "BayesFactor")
LinearSaveB <- as.data.frame(t(LinearSaveB))
colnames(LinearSaveB) <- LinearSaveB[1,]
LinearSaveB <- LinearSaveB[-1, ] 
write.csv(LinearSaveB, "BTlockLinear.csv", row.names = TRUE)
