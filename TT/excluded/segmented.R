setwd("F:/Tools/Rworkspace/FYP-data-analysis/TT")

data <- read.csv("69175.csv", header = TRUE)

fit <- lm(logE ~ trials, data=data)
summary(fit)
ggplot(data,aes(trials, error)) + geom_point() + stat_smooth(method="lm",formula=logE~trials)

segmented.fit <- segmented(fit, seg.blocks = ~trials, psi=15)
summary(segmented.fit)

p <- ggplot(data) + geom_line(aes(x = trials, y = logE))
p
my.seg <- segmented(fit, seg.Z = ~ trials, fixed.psi = list(trials = c(15, 12)))
slope(my.seg)
my.fitted <- fitted(my.seg)
my.model <- data.frame(trials = data$trials, error = my.fitted)
ggplot(my.model) + geom_line(aes(x = trials, y = error))
