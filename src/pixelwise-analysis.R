################################################################################ 
#
# Programmer: James Testa
# Assignment: Final Project
# Date: April 24, 2019
#
# Purpose: This file does analysis on the pixel wise accuracy values we
#  collected
#
################################################################################

# importing the data -----------------------------------------------------------

setwd("/Users/jamestesta/Desktop/HCI_Project/HCI-Interface-Comparison/data")
df <- read.csv("task-qualitative-data.csv")

#df
# Assessing normality ----------------------------------------------------------

by(as.numeric(df$Task.1.Accuracy), df$Photo.Editor, shapiro.test)
by(as.numeric(df$Task.2.Accuracy), df$Photo.Editor, shapiro.test)


# Assessing homegeneity of variance --------------------------------------------

library(car)

leveneTest(as.numeric(df$Task.1.Accuracy), df$Photo.Editor)
leveneTest(as.numeric(df$Task.2.Accuracy), df$Photo.Editor)


# Show that the data has a correlation -----------------------------------------

cor.test(as.numeric(df$Task.1.Accuracy), as.numeric(df$Photo.Editor), method = "pearson")
cor.test(as.numeric(df$Task.2.Accuracy), as.numeric(df$Photo.Editor), method = "pearson")


# Show a difference in means ---------------------------------------------------

results <- aov(as.numeric(df$Task.1.Accuracy) ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, as.numeric(df$Task.1.Accuracy), 
     main="Task 1 Pixelwise Accuracy Comparisons", 
     ylab="Percentage ", pch=19)

results <- aov(as.numeric(df$Task.2.Accuracy) ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, as.numeric(df$Task.2.Accuracy), 
     main="Task 1 Pixelwise Accuracy Comparisons", 
     ylab="Percentage ", pch=19)


################################################################################
