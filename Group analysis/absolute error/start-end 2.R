setwd("F:/Tools/Rworkspace/FYP-data-analysis/Group analysis/absolute error")
library("ggplot2")
library("BayesFactor")
library("XLConnect")
library("ggpubr")
library("tidyverse")
library("broom")
library("AICcmodavg")
library("rstatix")
library("MBESS")
library("rstatix")

file_list <- list.files(pattern = ".*accuracy.*.csv")
data <- data.frame()
start_end <- data.frame()
rowlength <- data.frame()



for (i in 1:length(file_list)){
  I <- read.csv(file_list[i], header = TRUE)
  ID <- file_list[i]
  Before <- head(I, 15)
  After <- tail(I, 15)
  Before <- colMeans(Before)
  After <- colMeans(After)
  ttest <- t.test(Before, After, paried=TRUE)
  dataO <- cbind(ID, ttest$statistic, ttest$p.value, ttest$stderr)
  data <- rbind.data.frame(data, dataO)
  afterO <- cbind(ID, Before, After)
  start_end <- rbind.data.frame(start_end, afterO)
  rowlength <- rbind(rowlength, nrow(afterO))
}
names(data) <- c("ID", "t-value", "p-value", "Std error")
write.csv(data, "start-endTtest2'.csv")

r55 <- cbind(rep("Trial true", as.numeric(rowlength[5,1])), rep("Trial", as.numeric(rowlength[5,1])), rep("True", as.numeric(rowlength[5,1])), rep("with", as.numeric(rowlength[5,1])))
r44 <- cbind(rep("Trial positive", as.numeric(rowlength[4,1])), rep("Trial", as.numeric(rowlength[4,1])), rep("positive", as.numeric(rowlength[4,1])), rep("with", as.numeric(rowlength[4,1])))
r22 <- cbind(rep("Block true", as.numeric(rowlength[2,1])), rep("Block", as.numeric(rowlength[2,1])), rep("True", as.numeric(rowlength[2,1])), rep("with", as.numeric(rowlength[2,1])))
r11 <- cbind(rep("Block positive", as.numeric(rowlength[1,1])), rep("Block", as.numeric(rowlength[1,1])), rep("positive", as.numeric(rowlength[1,1])), rep("with", as.numeric(rowlength[1,1])))
r33 <- cbind(rep("No feedback", as.numeric(rowlength[3,1])), rep("None", as.numeric(rowlength[3,1])), rep("None", as.numeric(rowlength[3,1])), rep("without", as.numeric(rowlength[3,1])))

start_end <- start_end[, -1]

dataANOVA <- cbind.data.frame(rbind.data.frame(r11,r22,r33,r44,r55), start_end)
colnames(dataANOVA) <- data.frame("group", "frequency", "content","feedback", "First", "Last")

write.csv(dataANOVA, "ANOVAmean2.csv")
dataANOVA <- read.csv("ANOVAmean2.csv", header = TRUE)

# orgnize data
dataANOVA <- dataANOVA %>%
  gather(key = "time", value = "error", First, Last) %>%
  convert_as_factor(group, time)
dataANOVA %>%
  group_by(group, time) %>%
  get_summary_stats(error, type = "mean_sd")

#check assumptions
dataANOVA %>%
  group_by(time, group) %>%
  identify_outliers(error)
dataANOVA %>%
  group_by(time, group) %>%
  shapiro_test(error)
ggqqplot(dataANOVA, "error", ggtheme = theme_bw()) +
  facet_grid(time ~ group)
one.way <- aov(error ~group, data = dataANOVA)
summary(one.way)


# mixed ANOVA
mixed.aov <- anova_test(
  data = dataANOVA, dv = error, wid = X,
  between = group, within = time
)
get_anova_table(mixed.aov)

mixed.aov2 <- anova_test(
  data = dataANOVA, dv = error, wid = X,
  between = frequency, within = time
)
get_anova_table(mixed.aov2)

mixed.aov3 <- anova_test(
  data = dataANOVA, dv = error, wid = X,
  between = content, within = time
)
get_anova_table(mixed.aov3)

mixed.aov4 <- anova_test(
  data = dataANOVA, dv = error, wid = X,
  between = feedback, within = time
)
get_anova_table(mixed.aov4)

# post hoc
pwc <- dataANOVA %>%
  group_by(group) %>%
  pairwise_t_test(error ~ time, p.adjust.method = "bonferroni")
pwc
pwc2 <- dataANOVA %>%
  group_by(time) %>%
  pairwise_t_test(error ~ group, p.adjust.method = "bonferroni")
pwc2

# box plot with p value
pwc <- pwc %>% add_xy_position(x = "group")
box.plot <- ggplot(data = dataANOVA, mapping=aes(group, error, colour=factor(time, levels=c("First","Last")))) +
  geom_boxplot(size=0.7) +
  theme(legend.position = "top", panel.background = element_rect(fill = "white", colour = "black", size=1)) +
  scale_colour_manual(values=c("firebrick", "dodgerblue3")) +
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(mixed.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  ) +
  xlab("Groups") + ylab("Error in adjustment") + 
  theme(axis.text=element_text(size=12),
         axis.title=element_text(size=13))
box.plot

# paired box plot
pwc <- pwc %>% add_xy_position(x = "time")
ggplot(data = dataANOVA, mapping=aes(reorder(time, -error), error)) +
  geom_boxplot(aes(colour=time), size=0.7) +
  geom_line(aes(group = X), colour="grey") + 
  geom_point(aes(colour=time), size = 1) + 
  facet_wrap(~ group) +
  theme(legend.position = "top", panel.background = element_rect(fill = "white", colour = "black", size=1)) +
  scale_colour_manual(values=c("firebrick", "dodgerblue3")) +
  stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(mixed.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  ) +
  xlab("Time") + ylab("Error in adjustment") + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13))

# bxp <- ggboxplot(dataANOVA, x = "group", y = "error",
#                  color = "time", palette = "jco")
# bxp

# other box plots
ggplot(data = dataANOVA) +
 
bxpheme(legend.position = "top", panel.background = element_rect(fill = "white", colour = "black")) +
  scale_colour_manual(values=c("dodgerblue3", "goldenrod2"))

ggplot(data = dataANOVA) +
  geom_boxplot(mapping=aes(content, error, colour=factor(time, levels=c("before","after")))) +
  theme(legend.position = "top", panel.background = element_rect(fill = "white", colour = "black")) +
  scale_colour_manual(values=c("dodgerblue3", "goldenrod2"))

ggplot(data = dataANOVA) +
  geom_boxplot(mapping=aes(feedback, error, colour=factor(time, levels=c("before","after")))) +
  theme(legend.position = "top", panel.background = element_rect(fill = "white", colour = "black")) +
  scale_colour_manual(values=c("dodgerblue3", "goldenrod2"))



interaction2 <- aov(error ~ group*time, data = dataANOVA)
summary(interaction2)
interaction3 <- aov(error ~ frequency*time, data = dataANOVA)
summary(interaction3)
interaction4 <- aov(error ~ content*time, data = dataANOVA)
summary(interaction4)
interaction5 <- aov(error ~ feedback*time, data = dataANOVA)
summary(interaction5)

blocking <- aov(error ~ time+content+frequency, data = dataANOVA)
summary(blocking)

dataANOVA %>%
  group_by(feedback, time) %>%
  get_summary_stats(error, type = "mean_sd")
ggboxplot(dataANOVA, x = "frequency", y = "error",
          color = "time", palette = "jco")
sink("meanComparisonANOVA2.txt")
print(dataANOVA %>%
        group_by(group, time) %>%
        get_summary_stats(error, type = "mean_sd"))
print(dataANOVA %>%
        group_by(frequency, time) %>%
        get_summary_stats(error, type = "mean_sd"))
print(dataANOVA %>%
        group_by(content, time) %>%
        get_summary_stats(error, type = "mean_sd"))
print(dataANOVA %>%
        group_by(feedback, time) %>%
        get_summary_stats(error, type = "mean_sd"))
print(get_anova_table(mixed.aov))
print(get_anova_table(mixed.aov2))
print(get_anova_table(mixed.aov3))
print(get_anova_table(mixed.aov4))
sink()
