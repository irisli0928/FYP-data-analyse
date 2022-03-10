setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis")
library("ggplot2")
library("BayesFactor")
data <- read.csv("group average.csv", header = TRUE)

fitTT <- lm(log(TT)~blocks, data=data, method = "qr")
summary(fitTT)
plot.TT <- ggplot(data,aes(blocks, TT, group=four)) + 
  geom_point() + 
  ylim(0,0.5) + 
  geom_smooth(method=lm, formula=y~log(x))
plot.TT

fitTP <- lm(log(TP)~blocks, data=data)
summary(fitTP)
plot.TP <- ggplot(data,aes(blocks, TP, group=four)) + 
  geom_point() + 
  ylim(0,0.5) + 
  geom_smooth(method=lm, formula=y~log(x))
plot.TP

fitBT <- lm(log(BT)~blocks, data=data)
summary(fitBT)
plot.BT <- ggplot(data,aes(blocks, BT, group=four)) + 
  geom_point() +
  ylim(0,0.5) + 
  geom_smooth(method=lm, formula=y~log(x))
plot.BT

fitBP <- lm(log(BP)~blocks, data=data)
summary(fitBP)
plot.BP <- ggplot(data,aes(blocks, BP, group=four)) + 
  geom_point() + 
  ylim(0,0.5) +
  geom_smooth(method=lm, formula=y~log(x))
plot.BP

fitN <- lm(log(N)~blocks, data=data)
summary(fitN)
plot.N <- ggplot(data,aes(blocks, N, group=four)) + 
  geom_point() + 
  ylim(0,0.5) +
  geom_smooth(method=lm, formula=y~log(x))
plot.N
