################################################################################ 
#
# Programmer: James Testa
# Assignment: Final Project
# Date: April 24, 2019
#
# Purpose: This file does analysis on the tutorials at each 
#  sub task
#
################################################################################

# importing the data -----------------------------------------------------------

setwd("/Users/jamestesta/Desktop/HCI-Interface-Comparison/data")
df <- read.csv("task-qualitative-data.csv")

#df
# Assessing normality ----------------------------------------------------------

by(as.numeric(df$Task.2.Blurring...Tutorials), df$Photo.Editor, shapiro.test)

# Assessing homegeneity of variance --------------------------------------------

library(car)

leveneTest(as.numeric(df$Task.2.Blurring...Tutorials), df$Photo.Editor)

# Show that the data has a correlation -----------------------------------------

cor.test(df$Task.2.Blurring...Tutorials, as.numeric(df$Photo.Editor), method = "kendall")

# Show a difference in means ---------------------------------------------------

kruskal.test(df$Task.2.Blurring...Tutorials ~ df$Photo.Editor) 
plot(df$Photo.Editor, df$Task.1.Rotating.Time, 
     main="Task 1 Rotating Time", 
     ylab="Seconds ", pch=19)
