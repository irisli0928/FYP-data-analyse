setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/absolute error")
library("ggplot2")
library("BayesFactor")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")
library("rstatix")
library("MBESS")
library("Rmisc")
library("scales")

file_list <- list.files(pattern = ".*accuracy.*.csv")
data <- data.frame()
ID <- list("Block positive", "Block true", "No feedback", "Trial positive", "Trial true")

for (i in 1:length(file_list)){
  I <- read.csv(file_list[i], header = TRUE)
  group <- as.character(ID[i])
  block <- 1:12
  for (k in 1:ncol(I)){
    K <- I[, k]
    blockma <- matrix(K, ncol = 15, byrow = TRUE) #create a matrix for calculating average of every n rows in the vector
    block_average <- apply(blockma, 1, mean) #calculating average
    blockO <- cbind(rep(group, 12), block, block_average)
    data <- rbind.data.frame(data, blockO)
  }
}
names(data) <- c("group", "block", "block_average")

write.csv(data, "plotall.csv")
data <- read.csv("plotall.csv")[, -1]

datac <- summarySE(data, measurevar="block_average", groupvars=c("group","block"))
pd <- position_dodge(0.4) # move them .05 to the left and right

ggplot(datac, aes(x=block, y=block_average, colour=group)) + 
  geom_errorbar(aes(ymin=block_average-se, ymax=block_average+se), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd) +
  ylab("block error") +
  xlim(1,12) + ylim(7.5,20) +
  scale_x_continuous(breaks= seq(1, 12, 2)) +
  theme(panel.background = element_rect(fill="grey95", colour="white")) +
  scale_colour_manual(values=c("dodgerblue3", "goldenrod", "orchid3", "seagreen4", "firebrick"))
