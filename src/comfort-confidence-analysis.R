################################################################################ 
#
# Programmer: Victoria Albanese
# Assignment: Final Project
# Date: April 3, 2019
#
# Purpose: This file does analysis for our comfort & confidence results
# aka for how comfortable and confident each user was using the tool, reported
# in the prestudy survey before the user completed any tasks
#
################################################################################

# importing the data -----------------------------------------------------------

setwd("C:/Users/Victoria/Desktop/repos/HCI-Interface-Comparison/data")
df <- read.csv("pre-survey-quantitative-data.csv")


# Assessing normality ----------------------------------------------------------

by(df$Confidence, df$Photo.Editor, shapiro.test)
by(df$Comfort, df$Photo.Editor, shapiro.test)


# Assessing homegeneity of variance --------------------------------------------

library(car)

# Levene Test is for normally distrubuted data
leveneTest(df$Confidence, df$Photo.Editor)
# Fligner Test is for non normal data
fligner.test(df$Comfort, df$Photo.Editor)


# Show that the data has a correlation -----------------------------------------

# Pearson is for normally distributed data
cor.test(df$Confidence, as.numeric(df$Photo.Editor), method = "pearson")
# Kendal is for non normal data
cor.test(df$Comfort, as.numeric(df$Photo.Editor), method = "kendall")


# Show a difference in means ---------------------------------------------------

results <- aov(df$Confidence ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, df$Confidence, 
     main="Confidence Mean Comparisons", 
     ylab="Score ", pch=19)

kruskal.test(df$Comfort ~ df$Photo.Editor) 
plot(df$Photo.Editor, df$Comfort, 
     main="Comfort Mean Comparisons", 
     ylab="Score ", pch=19)


################################################################################
