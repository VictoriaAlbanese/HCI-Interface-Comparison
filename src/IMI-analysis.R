################################################################################ 
#
# Programmer: Victoria Albanese
# Assignment: Final Project
# Date: April 3, 2019
#
# Purpose: This file does analysis for our IMI results
#
################################################################################

# importing the data -----------------------------------------------------------

setwd("C:/Users/Victoria/Desktop/repos/HCI-Interface-Comparison/data")
df <- read.csv("post-survey-quantitiative-data.csv")


# Assessing normality ----------------------------------------------------------

by(df$Interest.Enjoyment.Score, df$Photo.Editor, shapiro.test)
by(df$Perceived.Competence.Score, df$Photo.Editor, shapiro.test)
by(df$Effort.Importance.Score, df$Photo.Editor, shapiro.test)
by(df$Pressure.Tension.Score, df$Photo.Editor, shapiro.test)
by(df$Value.Usefulness.Score, df$Photo.Editor, shapiro.test)


# Assessing homegeneity of variance --------------------------------------------

library(car)

leveneTest(df$Interest.Enjoyment.Score, df$Photo.Editor)
leveneTest(df$Perceived.Competence.Score, df$Photo.Editor)
leveneTest(df$Effort.Importance.Score, df$Photo.Editor)
leveneTest(df$Pressure.Tension.Score, df$Photo.Editor)
leveneTest(df$Value.Usefulness.Score, df$Photo.Editor)


# Show that the data has a correlation -----------------------------------------

cor.test(df$Interest.Enjoyment.Score, as.numeric(df$Photo.Editor), method = "pearson")
cor.test(df$Perceived.Competence.Score, as.numeric(df$Photo.Editor), method = "pearson")
cor.test(df$Effort.Importance.Score, as.numeric(df$Photo.Editor), method = "pearson")
cor.test(df$Pressure.Tension.Score, as.numeric(df$Photo.Editor), method = "pearson")
cor.test(df$Value.Usefulness.Score, as.numeric(df$Photo.Editor), method = "pearson")


# Show a difference in means ---------------------------------------------------

results <- aov(df$Interest.Enjoyment.Score ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, df$Interest.Enjoyment.Score, 
     main="Interest Enjoyment Score Mean Comparisons", 
     ylab="Score ", pch=19)

results <- aov(df$Perceived.Competence.Score ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, df$Perceived.Competence.Score, 
     main="Perceived Competence Score Mean Comparisons", 
     ylab="Score ", pch=19)

results <- aov(df$Effort.Importance.Score ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, df$Effort.Importance.Score, 
     main="Effort Importance Score Mean Comparisons", 
     ylab="Score ", pch=19)

results <- aov(df$Pressure.Tension.Score ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, df$Pressure.Tension.Score, 
     main="Pressure Tension Score Mean Comparisons", 
     ylab="Score ", pch=19)

results <- aov(df$Value.Usefulness.Score ~ df$Photo.Editor)
summary(results)
plot(df$Photo.Editor, df$Value.Usefulness.Score, 
     main="Value Usefulness Score Mean Comparisons", 
     ylab="Score ", pch=19)

################################################################################
