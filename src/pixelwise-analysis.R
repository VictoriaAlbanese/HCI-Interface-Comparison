################################################################################ 
#
# Programmer: James Testa
# Assignment: Final Project
# Date: April 24, 2019
#
# Purpose: This file does analysis for our comfort & confidence results
# aka for how comfortable and confident each user was using the tool, reported
# in the prestudy survey before the user completed any tasks
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
     ylab="Score ", pch=19)

results <- aov(as.numeric(df$Task.2.Accuracy) ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, as.numeric(df$Task.2.Accuracy), 
     main="Task 1 Pixelwise Accuracy Comparisons", 
     ylab="Score ", pch=19)


################################################################################
