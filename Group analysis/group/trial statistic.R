setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/group")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")

#ANOVA for learning rate
dataANOVA <- read.csv("ANOVA.csv", header=TRUE)
summary(dataANOVA)
oneway <- aov(learningR ~ group, data = dataANOVA)
summary(oneway)

twoway <- aov(learningR ~ frequency + feedback, data = dataANOVA)
summary(twoway)

interaction <- aov(learningR ~ frequency*feedback, data = dataANOVA)
summary(interaction)

blocking <- aov(learningR ~ frequency + feedback + group, data = dataANOVA)
summary(blocking)
#check homoscedasticity
par(mfrow=c(2,2))
plot(oneway)
plot(twoway)
plot(interaction)
plot(blocking)
par(mfrow=c(1,1))



#find the best fit
model.set <- list(oneway, twoway, interaction, blocking)
model.names <- c("oneway", "twoway", "interaction", "blocking")
aictab(model.set, modnames = model.names)

#tukey plot for learning rate
tukey.plot.aov<-aov(learningR ~ frequency:feedback, data = dataANOVA)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
par(mar=c(4.1, 12, 4.1, 2.1))
plot(tukey.plot.test, las = 1, mar = 0.1)
#box plot for learning rate
dataANOVA %>%
  group_by(group) %>%
  get_summary_stats(learningR, type = "mean_sd")
ggboxplot(dataANOVA, x = "group", y = "learningR")

sink("ANOVA.txt")
print("one-way ANOVA")
print(summary(oneway))
print("interaction")
print(summary(interaction))
print(dataANOVA %>%
        group_by(group) %>%
        get_summary_stats(learningR, type = "mean_sd"))
sink()


# #ANOVA for intercept
# dataANOVAi <- read.csv("ANOVAi.csv", header=TRUE)
# sink("ANOVAi.txt")
# # summary(dataANOVAi)
# oneway <- aov(intercept ~ group, data = dataANOVAi)
# # summary(oneway)
# print("one-way ANOVA")
# print(summary(oneway))
# 
# twoway <- aov(intercept ~ frequency + feedback, data = dataANOVAi)
# # summary(twoway)
# print("two-way ANOVA")
# print(summary(twoway))
# interaction <- aov(intercept ~ frequency*feedback, data = dataANOVAi)
# # summary(interaction)
# print("interaction")
# print(summary(interaction))
# 
# blocking <- aov(intercept ~ frequency + feedback + group, data = dataANOVAi)
# # summary(blocking)
# print("blocking")
# print(summary(blocking))
# sink()
# 
# tukey.plot.aov<-aov(intercept ~ frequency:feedback, data = dataANOVAi)
# tukey.plot.test<-TukeyHSD(tukey.plot.aov)
# par(mar=c(4.1, 12, 4.1, 2.1))
# plot(tukey.plot.test, las = 1, mar = 0.1)
# 
# 
# dataANOVAi %>%
#   group_by(group) %>%
#   get_summary_stats(intercept, type = "mean_sd")
# ggboxplot(dataANOVAi, x = "group", y = "intercept")
# 
