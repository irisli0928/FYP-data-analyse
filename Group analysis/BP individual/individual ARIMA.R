setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/BP individual")
library("ggplot2")
library("BayesFactor")
library("tseries")
library("forecast")
data <- read.csv("individual data.csv", header = TRUE)

I <- data$X68851

# ARIMA
# stationarity and differencing
plot.individual <- ggplot() + 
  geom_line(data,mapping = aes(trials, I)) +
  geom_point(data,mapping = aes(trials, I)) +
  labs(y="error") 
plot.individual
ndiffs(I)
dind <- diff(I)
trials1 <- 1:179
ggplot() + 
  geom_line(mapping = aes(trials1, dind)) +
  geom_point(mapping = aes(trials1, dind)) +
  labs(y="error_differencing")
ADF <- adf.test(dind)
ADF

#fitting``
#autocorrelation function
ggAcf(I, main="")
#partical autocorrelation function
ggPacf(I)
fitA <- auto.arima(I,approximation=FALSE)
fitA
accuracy(fitA)
fit2 <- Arima(I,order=c(0,1,0))
fit2

# residual diagnose
qqnorm(fitA$residuals)
qqline(fitA$residuals)
Box.test(fitA$residuals, type="Ljung-Box")