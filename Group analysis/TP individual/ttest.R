setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/TP individual")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
data <- read.csv("individual data.csv", header = TRUE)

tResult <- data.frame()#create a data frame for saving output
for(i in 4:ncol(data)) {
  ID <- colnames(data[i]) #extract column name
  I <- data[ , i] #extract data to a single vector by column number
  Before <- head(I, 15)
  After <- tail(I, 15)
  ttest <- t.test(Before, After)
  tBF <- ttestBF(Before, After)
  #save output in this loop, each number in a seperate column
  dataO <- cbind(ID, ttest$statistic, ttest$p.value, ttest$stderr, t(as.numeric(extractBF(tBF)$bf)))
  #save output in every loop into one data frame by adding rows
  tResult <- rbind(tResult, dataO)
}
names(tResult) <- c("ID", "t-value", "p-value", "Std error", "bayes factor")
write.csv(tResult, "TPtResult.csv", row.names = TRUE)