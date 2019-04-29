################################################################################ 
#
# Programmer: Victoria Albanese
# Assignment: Final Project
# Date: April 3, 2019
#
# Purpose: This file does analysis for our self reported difficulty results
#
################################################################################

# importing the data -----------------------------------------------------------

setwd("C:/Users/Victoria/Desktop/repos/HCI-Interface-Comparison/data")
df <- read.csv("post-survey-quantitiative-data.csv")


# Assessing normality ----------------------------------------------------------

by(df$Task.1.Difficulty, df$Photo.Editor, shapiro.test)
by(df$Task.2.Difficulty, df$Photo.Editor, shapiro.test)


# Assessing homegeneity of variance --------------------------------------------

library(car)

fligner.test(df$Task.1.Difficulty, df$Photo.Editor)
fligner.test(df$Task.2.Difficulty, df$Photo.Editor)


# Show that the data has a correlation -----------------------------------------

cor.test(df$Task.1.Difficulty, as.numeric(df$Photo.Editor), method = "kendall")
cor.test(df$Task.2.Difficulty, as.numeric(df$Photo.Editor), method = "kendall")


# Show a difference in means ---------------------------------------------------

kruskal.test(df$Task.1.Difficulty ~ df$Photo.Editor) 
plot(df$Photo.Editor, df$Task.1.Difficulty, 
     main="Task 1 Difficulty Mean Comparisons", 
     ylab="Score ", pch=19)

kruskal.test(df$Task.2.Difficulty ~ df$Photo.Editor) 
plot(df$Photo.Editor, df$Task.2.Difficulty, 
     main="Task 2 Difficulty Mean Comparisons", 
     ylab="Score ", pch=19)


################################################################################
