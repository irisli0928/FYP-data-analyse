setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis")
library("ggplot2")
library("BayesFactor")
data <- read.csv("group average.csv", header = TRUE)

fitTT <- lm(log(TT)~blocks, data=data)
summary(fitTT)
plot.group <- ggplot() + 
  geom_line(data,mapping = aes(blocks, TT), colour="purple") +
  geom_point(data,mapping = aes(blocks, TT), colour="purple") +
  geom_line(data,mapping = aes(blocks, TP), colour="purple", linetype="dashed") +
  geom_point(data,mapping = aes(blocks, TP), colour="purple") +
  geom_line(data,mapping = aes(blocks, BT), colour="orange") +
  geom_point(data,mapping = aes(blocks, BT), colour="orange") +
  geom_line(data,mapping = aes(blocks, BP), colour="orange", linetype="dashed") +
  geom_point(data,mapping = aes(blocks, BP), colour="orange") +
  geom_line(data,mapping = aes(blocks, N), colour="black") +
  geom_point(data,mapping = aes(blocks, N), colour="black") +
  ylim(0,0.5) + xlim(1,12) +
  labs(y="error") 
plot.group <- plot.group + theme(panel.background = element_rect(fill="grey90", colour="grey50"))
plot.group + theme(axis.line = element_line(size = 3, colour = "grey80"))
plot.group + theme(axis.ticks = element_line(size = 1))



plot.group
