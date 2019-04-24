################################################################################ 
#
# Programmer: James Testa
# Assignment: Final Project
# Date: April 24, 2019
#
# Purpose: This file does analysis on the time, tutorials, and attempts at each 
#  sub task
#
################################################################################

# importing the data -----------------------------------------------------------

setwd("/Users/jamestesta/Desktop/HCI-Interface-Comparison/data")
df <- read.csv("task-qualitative-data.csv")

#df
# Assessing normality ----------------------------------------------------------

by(as.numeric(df$Task.1.Rotating.Time), df$Photo.Editor, shapiro.test)
by(as.numeric(df$Task.1.Texting.Time), df$Photo.Editor, shapiro.test)
by(as.numeric(df$Task.1.Total.Time), df$Photo.Editor, shapiro.test)
by(as.numeric(df$Task.2.BlurringTime), df$Photo.Editor, shapiro.test)
by(as.numeric(df$Task.2.Texting.Time), df$Photo.Editor, shapiro.test)
by(as.numeric(df$Task.2.Cropping.Time), df$Photo.Editor, shapiro.test)
by(as.numeric(df$Task.2.Total.Time), df$Photo.Editor, shapiro.test)


# Assessing homegeneity of variance --------------------------------------------

library(car)

leveneTest(as.numeric(df$Task.1.Rotating.Time), df$Photo.Editor)
leveneTest(as.numeric(df$Task.1.Texting.Time), df$Photo.Editor)
leveneTest(as.numeric(df$Task.1.Total.Time), df$Photo.Editor)
leveneTest(as.numeric(df$Task.2.BlurringTime), df$Photo.Editor)
leveneTest(as.numeric(df$Task.2.Texting.Time), df$Photo.Editor)
leveneTest(as.numeric(df$Task.2.Cropping.Time), df$Photo.Editor)
leveneTest(as.numeric(df$Task.2.Total.Time), df$Photo.Editor)

# Show that the data has a correlation -----------------------------------------

cor.test(df$Task.1.Rotating.Time, as.numeric(df$Photo.Editor), method = "kendall")
cor.test(df$Task.1.Texting.Time, as.numeric(df$Photo.Editor), method = "pearson")
cor.test(df$Task.1.Total.Time, as.numeric(df$Photo.Editor), method = "pearson")
cor.test(df$Task.2.BlurringTime, as.numeric(df$Photo.Editor), method = "pearson")
cor.test(df$Task.2.Texting.Time, as.numeric(df$Photo.Editor), method = "pearson")
cor.test(df$Task.2.Cropping.Time, as.numeric(df$Photo.Editor), method = "pearson")
cor.test(df$Task.2.Total.Time, as.numeric(df$Photo.Editor), method = "pearson")

# Show a difference in means ---------------------------------------------------

kruskal.test(df$Task.1.Rotating.Time ~ df$Photo.Editor) 
plot(df$Photo.Editor, df$Task.1.Rotating.Time, 
     main="Task 1 Rotating Time", 
     ylab="Seconds ", pch=19)

results <- aov(as.numeric(df$Task.1.Texting.Time) ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, as.numeric(df$Task.1.Texting.Time), 
     main="Task 1 Texting Time", 
     ylab="Seconds ", pch=19)

results <- aov(as.numeric(df$Task.1.Total.Time) ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, as.numeric(df$Task.1.Total.Time), 
     main="Task 1 Total Time", 
     ylab="Seconds ", pch=19)

results <- aov(as.numeric(df$Task.2.BlurringTime) ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, as.numeric(df$Task.2.BlurringTime), 
     main="Task 2 Blurring Time", 
     ylab="Seconds ", pch=19)

results <- aov(as.numeric(df$Task.2.Texting.Time) ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, as.numeric(df$Task.2.Texting.Time), 
     main="Task 2 Texting Time", 
     ylab="Seconds ", pch=19)

results <- aov(as.numeric(df$Task.2.Cropping.Time) ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, as.numeric(df$Task.2.Cropping.Time), 
     main="Task 2 Cropping Time", 
     ylab="Seconds ", pch=19)

results <- aov(as.numeric(df$Task.2.Total.Time) ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, as.numeric(df$Task.2.Total.Time), 
     main="Task 2 Total Time", 
     ylab="Seconds ", pch=19)
