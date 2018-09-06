# 
# Shawn Schwartz
# EEB 109L - Summer 2018
# TA: Tyler McCraney
#
# Email: shawnschwartz@ucla.edu
#

# Analyses Utilizing Monday Data

# set working directory
setwd("~/OneDrive/UCLA Undergraduate Degree/Summer 2018/Summer Session C - 2018/EE BIOL 109L/R-Projects/EEB109L-SandCrabs")

# load tidyverse library
library(tidyverse)

# read-in csv file data
Counts <- read.csv(file = "Counts.csv", header = TRUE)
Lengths <- read.csv(file = "Lengths.csv", header = TRUE)

# make scatterplot of adult and recruit sand crab counts
Adults <- Counts$Ovigerous + Counts$Non.ovigerous
Counts <- cbind(Counts, Adults)

plot(Counts$Adults, Counts$Recruits, xlab = "Adult count",
ylab = "Recruit count", main = "Sand crabs")

# fit linear regression model
AdultsRecruitsFit <- lm(Recruits ~ Adults, data = Counts)

summary(AdultsRecruitsFit)

# probability distribution of the residuals
plot(density(resid(AdultsRecruitsFit)), xlab = "Residuals", 
ylab = "Probability density", main = "Distribution of residuals")

plot(AdultsRecruitsFit, which = 2)

plot(Counts$Adults, Counts$Recruits, xlab = "Adult count", 
ylab = "Recruit count", main = "Sand crabs")

abline(AdultsRecruitsFit)

legend("bottomright", bty = "n", legend = paste("R-squared=", format(summary(AdultsRecruitsFit)$r.squared, digits = 4)))

# enhanced plotted figure with 95% confidence regions around regression line
ggplot(data = Counts) + 
  geom_point(mapping = aes(x = Adults, y = Recruits)) + 
  geom_smooth(mapping = aes(x = Adults, y = Recruits), method = lm) + 
  labs(x = "Adult count", y = "Recruit count", title = "Sand crabs", caption = "Figure 1: Significant positive relationship between adult and recruit sand crab counts at Dockweiler State 
       Beach (p-value: 0.02989, R-squared: 0.07563). Linear regression line includes 95% confidence regions.")

ggsave("Figure1.pdf")

# create and plot two size frequency histograms
ggplot(data = Lengths, aes(Carapace.length.mm, fill = Eggs)) + 
  geom_histogram(position = "dodge") + 
  labs(x = "Carapace length (mm)", y = "Count", title = "Sand crabs", caption = "Figure 2: Size frequency histograms of sand crabs at Dockweiler State Beach.")

ggsave("Figure2.pdf")

# estimate summary stats for carapace lengths
eggs <- filter(Lengths, Eggs == "Present")
no.eggs <- filter(Lengths, Eggs == "Absent")

xbarL.eggs <- mean(eggs$Carapace.length.mm)
xbarL.no.eggs <- mean(no.eggs$Carapace.length.mm)

sdL.eggs <- sd(eggs$Carapace.length.mm)
sdL.no.eggs <- sd(no.eggs$Carapace.length.mm)

n.eggs <- nrow(eggs)
n.no.eggs <- nrow(no.eggs)

seL.eggs <- sdL.eggs / sqrt(n.eggs)
seL.no.eggs <- sdL.no.eggs / sqrt(n.no.eggs)

boundL.eggs <- 1.96 * seL.eggs
boundL.no.eggs <- 1.96 * seL.no.eggs

lowL.eggs <- xbarL.eggs - boundL.eggs
lowL.no.eggs <- xbarL.no.eggs - boundL.no.eggs

upL.eggs <- xbarL.eggs + boundL.eggs
upL.no.eggs <- xbarL.no.eggs + boundL.no.eggs

egg.stats <- c(lowL.eggs, xbarL.eggs, upL.eggs)
no.egg.stats <- c(lowL.no.eggs, xbarL.no.eggs, upL.no.eggs)

stats <- rbind(egg.stats, no.egg.stats)

colnames(stats) <- c("2.5% length", "Mean length", "97.5% length")

rownames(stats) <- c("Eggs", "No eggs")

stats

# write final stats to csv file
write.csv(stats, "stats.csv")