setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/N individual")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
data <- read.csv("individual data.csv", header = TRUE)

# logrithmic regression for trial average
sink("fitT.txt") #creating recording file
for(i in 4:ncol(data)) {
  ID <- colnames(data[i]) #extract column name
  I <- data[ , i] #extract data to a single vector by column number
  blockma <- matrix(I, ncol = 15, byrow = TRUE) #create a matrix for calculating average of every n rows in the vector
  block_average <- apply(blockma, 1, mean) #calculating average
  block <- 1:12
  trial <- 1:180
  logI <- log(I)
  logB <- log(block_average)
  data1 <- data.frame(trial, block, block_average, logI, logB) #record all needed var into dataframe
  fitT <- lm(log(I) ~ trials, data=data)
  summary(fitT)
  bfT <- regressionBF(logI ~ trial, data=data1)
  extractBF(bfT)
  print(ID) #write all needed info into the file
  print(summary(fitT))
  print(extractBF(bfT))
}
sink() #clear the stack

# logrithmic regression for block average
sink("fitB.txt")
for(i in 4:ncol(data)) {
  ID <- colnames(data[i])
  I <- data[ , i]
  blockma <- matrix(I, ncol = 15, byrow = TRUE)
  block_average <- apply(blockma, 1, mean)
  block <- 1:12
  trial <- 1:180
  logI <- log(I)
  logB <- log(block_average)
  data1 <- data.frame(trial, block, block_average, logI, logB)
  fitB <- lm(log(block_average) ~ block)
  summary(fitB)
  bfB <- regressionBF(logB ~ block, data=data1)
  extractBF(bfB)
  print(ID)
  print(summary(fitB))
  print(extractBF(bfB))
}
sink()