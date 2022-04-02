setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/BP individual")
library("ggplot2")
library("BayesFactor")
library("plyr")
data <- read.csv("individual data.csv", header = TRUE)


dataBlock <- data.frame(matrix(nrow=12))
lowest <- data.frame(matrix(nrow=12))
endpoint <- vector()
datafit <- data.frame()
tResult <- data.frame()
for(i in 4:ncol(data)) {
  ID <- colnames(data[i]) #extract column name
  I <- data[ , i] #extract data to a single vector by column number
  blockma <- matrix(I, ncol = 15, byrow = TRUE) #create a matrix for calculating average of every n rows in the vector
  block_average <- apply(blockma, 1, mean) #calculating average
  dim(block_average) <- c(length(block_average), 1)
  dataBlock <- cbind(dataBlock, block_average)
  #get the data from start to lowest point
  minB <- which.min(block_average) 
  endpoint <- rbind(endpoint, minB)
  lp <- I[1:(minB*15)]
  dim(lp) <- c(length(lp), 1)
  trial <- 1:length(lp)
  logT <- log(lp)
  dataS <- data.frame(trial,logT)
  fit <- lm(log(lp)~trial)
  bf <- regressionBF(logT~trial, data=dataS)
  if (length(lp)>15){
    Before <- head(lp, 15)
    After <- tail(lp, 15)
    ttest <- t.test(Before, After)
    tBF <- ttestBF(Before, After)
    #save output in this loop, each number in a seperate column
    datatest <- cbind(ID, ttest$statistic, ttest$p.value, ttest$stderr, t(as.numeric(extractBF(tBF)$bf)))
    #save output in every loop into one data frame by adding rows
    tResult <- rbind(tResult, datatest)
  }
  dataO <- cbind(ID, t(as.numeric(coefficients(fit))),t(as.numeric(summary(fit)$coefficients[, 2])), t(as.numeric(summary(fit)$coefficients[, 4])), t(as.numeric(summary(fit)$r.squared)), t(as.numeric(extractBF(bf)$bf)), length(lp))
  datafit <- rbind(datafit, dataO)
  length(lp) <- 180
  lowest <- cbind.data.frame(lowest, lp)
}
names(tResult) <- c("ID", "t-value", "p-value", "Std error", "bayes factor")
write.csv(tResult, "BPtResult.csv", row.names = TRUE)
dataBlock <- dataBlock[, -1]
lowest <- lowest[, -1]
names(datafit) <- c("ID", paste("coeff", names(coefficients(fit))), paste("Std. Error", names(summary(fit)$coefficients[, 2])), paste("P-value", names(summary(fit)$coefficients[, 4])), "R-squared", "BayesFactor", "length")
write.csv(datafit, "fitBP.csv")


dim(endpoint) <- c(length(endpoint), 1)
write.csv(endpoint, "endpointBP.csv")
write.csv(lowest, "lowestBP.csv", na="")



# name <- paste("v",i, sep = "")
# assign(name, block_average[1:minB])