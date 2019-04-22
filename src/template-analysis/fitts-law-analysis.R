################################################################################ 
#
# Programmer: Victoria Albanese
# Assignment: Homework 3
# Date: April 3, 2019
#
# Purpose: This file does analysis for two hypothesis regarding fitts law
#
#             (1) there is a difference between the average times it takes to 
#                 click for different hands
#
#             (2) there is an effect of distance and target size on the time 
#                 it takes to click
#
################################################################################

# IMPORTING THE DATA -----------------------------------------------------------

setwd("C:/Users/Victoria/Desktop/repos/fitts-law-experiment/submit")

df <- data.frame(Timestamp=as.Date(character()),
                 Hand=factor(),
                 Trial=factor(),
                 Distance=numeric(),
                 Size=numeric(),
                 Time=numeric(),
                 Participant=factor())

for (participant in c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) {
  filename <- paste("P", toString(participant), "-log.csv", sep="")
  tempDF <- read.csv(filename, header = FALSE)
  colnames(tempDF) <- c("Timestamp", "Hand", "Trial", "Distance", "Size", "Time")
  tempDF$Participant <- rep(participant, nrow(tempDF))
  df <- rbind(df, tempDF)
}


# HYPOTHESIS (1) WORK ----------------------------------------------------------

# Assessing normality
by(df$Time, df$Hand, shapiro.test)
by(df$Time, df$Hand, qqnorm)

# Assessing homegeneity of variance
fligner.test(df$Time ~ df$Hand, data = df)

# Show that the data has a correlation
plot(df$Time, df$Hand, main="Clicking Time for Dominant vs Non-Dominant Hands", xlab="Time ", ylab="Hand ", pch=19)
cor.test(df$Time, as.numeric(df$Hand), method = "kendall")

# Show a difference in means
kruskal.test(df$Time ~ df$Hand) 
plot(df$Hand, df$Time, main="Clicking Time for Dominant vs Non-Dominant Hands", xlab="Hand ", ylab="Time ", pch=19)


# HYPOTHESIS (2) WORK ----------------------------------------------------------

# Assessing normality
by(df$Time, df$Size, shapiro.test)
by(df$Time, df$Distance, shapiro.test)
by(df$Time, df$Size, qqnorm)
by(df$Time, df$Distance, qqnorm)

# Verify that the data is parametric by proving homegeneity of variance
fligner.test(df$Time ~ df$Size, data = df)
fligner.test(df$Time ~ df$Distance, data = df)

# Show that the data has a correlation
plot(df$Time, df$Size, main="Clicking Time vs Target Size", xlab="Time ", ylab="Target Size ", pch=19)
plot(df$Time, df$Distance, main="Clicking Time vs Distance between Targets", xlab="Time ", ylab="Distance between Targets ", pch=19)
cor.test(df$Time, as.numeric(df$Size), method = "kendall")
cor.test(df$Time, as.numeric(df$Distance), method = "kendall")

# Show a difference in means
kruskal.test(df$Time ~ df$Size) 
kruskal.test(df$Time ~ df$Distance) 
boxplot(df$Time ~ df$Size, main="Clicking Time vs Target Size", xlab="Size ", ylab="Time ", pch=19)
boxplot(df$Time ~ df$Distance, main="Clicking Time vs Distance between Targets", xlab="Distance ", ylab="Time ", pch=19)


# EXPORTING THE DATA -----------------------------------------------------------

# For hypothesis (1)
handDF <- df[,c(2,6)]
write.csv(handDF,'Hand-time.csv')

# For hypothesis (2)
timeDF <- df[,c(4:5,6)]
write.csv(timeDF,'Fitts-time.csv')


################################################################################
