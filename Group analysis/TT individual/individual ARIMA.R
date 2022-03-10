setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/TT individual")
library("ggplot2")
library("BayesFactor")
library("tseries")
library("forecast")
data <- read.csv("individual data.csv", header = TRUE)

I <- data$X68995

# logrithmic regression for trials and block average
fitB <- lm(log(I) ~ blocks, data=data)
summary(fitB)
fitT <- lm(log(I) ~ trials, data=data)
summary(fitT)

plot.log <- ggplot(data,aes(trials, I)) + 
  geom_point() + 
  geom_smooth(method=lm, formula=y~log(x))
plot.log
plot.individual <- ggplot() + 
  geom_line(data,mapping = aes(trials, I)) +
  geom_point(data,mapping = aes(trials, I)) +
  labs(y="error") 
plot.individual

# ARIMA
# stationarity and differencing
plot(I)
ndiffs(I)
dind <- diff(I)
plot(dind)
ADF <- adf.test(dind)
ADF

#fitting
fitA <- auto.arima(I)
fitA
accuracy(fitA)

# residual diagnose
qqnorm(fit$residuals)
qqline(fit$residuals)
Box.test(fit$residuals, type="Ljung-Box")

