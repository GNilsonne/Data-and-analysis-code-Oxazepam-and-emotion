# Script to analyse ratings of unpleasantness in empathy experiment from the oxazepam and emotion project
# Gustav Nilsonne 2015-01-10

# Require packages
library(RCurl) # To read data from GitHub
library(nlme) # To build mixed-effects models
library(effects) # To get confidence intervals on estimates
library(RColorBrewer) # To get good diverging colors for graphs

# Define colors for later
col1 = brewer.pal(3, "Dark2")[1]
col2 = brewer.pal(3, "Dark2")[2]

# Read data
ratingsDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/ratedUnpleasantness.csv", ssl.verifypeer = FALSE)
ratingsData <- read.csv(text = ratingsDataURL)

demDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/demographics.csv", ssl.verifypeer = FALSE)
demData <- read.csv(text = demDataURL)

# Merge data, retain only included participants
ratingsData <- merge(ratingsData, demData, by = "Subject")
ratingsData <- ratingsData[ratingsData$Included_EP == TRUE, ]

# Put data columns in right formats
ratingsData$Subject <- as.factor(ratingsData$Subject)
ratingsData$Treatment <- as.factor(ratingsData$Treatment)
ratingsData$Stimulus <- as.factor(ratingsData$Stimulus)
ratingsData$Condition <- as.factor(ratingsData$Condition)

# Analyse data
# Include IRI-EC terms in model since we wish to control for baseline empathic propensity 

# Make a new column to specify an "Other High" contrast in modelling
ratingsData$OtherHigh <- as.numeric(ratingsData$Condition)*as.numeric(ratingsData$Stimulus)
ratingsData$OtherHigh[ratingsData$OtherHigh == 1] <- 1
ratingsData$OtherHigh[ratingsData$OtherHigh != 1] <- 0

# Make a regressor for predictor in other high condition only
ratingsData$IRI_EC_OtherHigh <- ratingsData$IRI_EC * ratingsData$OtherHigh

# Mean center the new regressor
ratingsData$IRI_EC_OtherHigh[ratingsData$IRI_EC_OtherHigh > 0 & !is.na(ratingsData$IRI_EC_OtherHigh)] <- ratingsData$IRI_EC_OtherHigh[ratingsData$IRI_EC_OtherHigh > 0 & !is.na(ratingsData$IRI_EC_OtherHigh)] - mean(ratingsData$IRI_EC_OtherHigh[ratingsData$IRI_EC_OtherHigh > 0 & !is.na(ratingsData$IRI_EC_OtherHigh)], na.rm = TRUE)

# Build model
lme1 <- lme(Unpleasantness ~ Treatment*(Stimulus*Condition) + Wave + IRI_EC + IRI_EC_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)

plot(lme1)
summary(lme1)
intervals(lme1)

# Make plots
eff1 <- effect("Treatment*Stimulus*Condition", lme1)

pdf("Fig_Unpleasantness1.pdf", width = 4, height = 4)
plot(c(eff1$fit[5], eff1$fit[7]),
     type = "b",
     frame.plot = F,
     ylab = "VAS",
     xlab = "Shock intensity",
     xaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0, 50),
     col = col2,
     main = "A. Rated unpleasantness, Self",
     pch = 16)
lines(c(1.1, 2.1), c(eff1$fit[6], eff1$fit[8]), type = "b", col = col1)
lines(c(1, 1), c(eff1$upper[5], eff1$lower[5]), col = col2)
lines(c(2, 2), c(eff1$upper[7], eff1$lower[7]), col = col2)
lines(c(1.1, 1.1), c(eff1$upper[6], eff1$lower[6]), col = col1)
lines(c(2.1, 2.1), c(eff1$upper[8], eff1$lower[8]), col = col1)
axis(1, at = c(1.05, 2.05), labels = c("High", "Low"))
legend("topright", col = c(col1, col2), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), bty = "n")
dev.off()

pdf("Fig_Unpleasantness2.pdf", width = 4, height = 4)
plot(c(eff1$fit[1], eff1$fit[3]),
     type = "b",
     frame.plot = F,
     ylab = "VAS",
     xlab = "Shock intensity",
     xaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0, 50),
     col = col2,
     main = "B. Rated unpleasantness, Other",
     pch = 16)
lines(c(1.1, 2.1), c(eff1$fit[2], eff1$fit[4]), type = "b", col = col1)
lines(c(1, 1), c(eff1$upper[1], eff1$lower[1]), col = col2)
lines(c(2, 2), c(eff1$upper[3], eff1$lower[3]), col = col2)
lines(c(1.1, 1.1), c(eff1$upper[2], eff1$lower[2]), col = col1)
lines(c(2.1, 2.1), c(eff1$upper[4], eff1$lower[4]), col = col1)
axis(1, at = c(1.05, 2.05), labels = c("High", "Low"))
dev.off()

# Compare plots to less custom-generated output for verification
plot(effect("Treatment*Stimulus*Condition", lme1))
