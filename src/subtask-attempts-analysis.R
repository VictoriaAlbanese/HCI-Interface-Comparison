################################################################################ 
#
# Programmer: James Testa
# Assignment: Final Project
# Date: April 24, 2019
#
# Purpose: This file does analysis on the attempts at each 
#  sub task
#
################################################################################

# importing the data -----------------------------------------------------------

setwd("/Users/jamestesta/Desktop/HCI-Interface-Comparison/data")
df <- read.csv("task-qualitative-data.csv")

#df
# Assessing normality ----------------------------------------------------------

by(as.numeric(df$Task.2.Blurring...Attempts), df$Photo.Editor, shapiro.test)


# Assessing homegeneity of variance --------------------------------------------

library(car)

leveneTest(as.numeric(df$Task.2.Blurring...Attempts), df$Photo.Editor)

# Show that the data has a correlation -----------------------------------------

cor.test(df$Task.2.Blurring...Attempts, as.numeric(df$Photo.Editor), method = "pearson")

# Show a difference in means ---------------------------------------------------

results <- aov(as.numeric(df$Task.2.Blurring...Attempts) ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, as.numeric(df$Task.1.Total.Time), 
     main="Task 2 Blurring Attempts", 
     ylab="Seconds ", pch=19)

