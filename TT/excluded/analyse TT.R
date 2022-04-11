setwd("F:/Tools/Rworkspace/FYP-data-analysis/TT")

data <- read.csv("TT.csv", header = TRUE)

fitA <- lm(logA ~ block, data=data)
summary.lm(fitA)

fitR <- lm(logR ~ block, data=data)
summary.lm(fitR)

bf <- regressionBF(logA ~ block, data = data)
extractBF(bf)

plot(data$block, data$accuracy, log="y")
plot(data$block, data$rate, log="y")
