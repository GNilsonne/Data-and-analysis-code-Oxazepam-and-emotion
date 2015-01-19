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
SCRDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/SCR.csv", ssl.verifypeer = FALSE)
SCRData <- read.csv(text = SCRDataURL)

demDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/demographics.csv", ssl.verifypeer = FALSE)
demData <- read.csv(text = demDataURL)

# Merge data, retain only included participants
SCRData <- merge(SCRData, demData, by = "Subject")
SCRData <- SCRData[SCRData$Included_EP == TRUE, ]

# Put data columns in right formats
SCRData$Subject <- as.factor(SCRData$Subject)
SCRData$Treatment <- as.factor(SCRData$Treatment)
SCRData$Stimulus <- as.factor(SCRData$Stimulus)
SCRData$Condition <- as.factor(SCRData$Condition)

# Investigate normality of distribution of SCR:s
hist(SCRData$SCR)
SCRData$sqrtSCR <- sqrt(SCRData$SCR)
hist(SCRData$sqrtSCR)

# Analyse data
# Include IRI-EC terms in model since we wish to control for baseline empathic propensity 

# Make a new column to specify an "Other High" contrast in modelling
SCRData$OtherHigh <- as.numeric(SCRData$Condition)*as.numeric(SCRData$Stimulus)
SCRData$OtherHigh[SCRData$OtherHigh == 1] <- 1
SCRData$OtherHigh[SCRData$OtherHigh != 1] <- 0

# Make a regressor for predictor in other high condition only
SCRData$IRI_EC_OtherHigh <- SCRData$IRI_EC * SCRData$OtherHigh

# Mean center the new regressor
SCRData$IRI_EC_OtherHigh[SCRData$IRI_EC_OtherHigh > 0 & !is.na(SCRData$IRI_EC_OtherHigh)] <- SCRData$IRI_EC_OtherHigh[SCRData$IRI_EC_OtherHigh > 0 & !is.na(SCRData$IRI_EC_OtherHigh)] - mean(SCRData$IRI_EC_OtherHigh[SCRData$IRI_EC_OtherHigh > 0 & !is.na(SCRData$IRI_EC_OtherHigh)], na.rm = TRUE)

# Build model
lme1 <- lme(sqrtSCR ~ Treatment*(Stimulus*Condition) + Wave + IRI_EC + IRI_EC_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)

plot(lme1)
summary(lme1)
intervals(lme1)

# Make plots
eff1 <- effect("Treatment*Stimulus*Condition", lme1)

pdf("Fig_SCR1.pdf", width = 4, height = 4)
plot(c(eff1$fit[6], eff1$fit[8]),
     type = "b",
     frame.plot = F,
     ylab = expression(sqrt(Amplitude)),
     xlab = "Shock intensity",
     xaxt = "n",
     yaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0.05, 0.35),
     col = col1,
     main = "C. Skin conductance responses, Self"
     )
lines(c(1.1, 2.1), c(eff1$fit[5], eff1$fit[7]), type = "b", col = col2, pch = 16)
lines(c(1, 1), c(eff1$upper[6], eff1$lower[6]), col = col1)
lines(c(2, 2), c(eff1$upper[8], eff1$lower[8]), col = col1)
lines(c(1.1, 1.1), c(eff1$upper[5], eff1$lower[5]), col = col2)
lines(c(2.1, 2.1), c(eff1$upper[7], eff1$lower[7]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("High", "Low"))
axis(2, at = c(0.05, 0.15, 0.25, 0.35))
#legend("topright", col = c(col1, col2), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), bty = "n")
dev.off()

pdf("Fig_SCR2.pdf", width = 4, height = 4)
plot(c(eff1$fit[2], eff1$fit[4]),
     type = "b",
     frame.plot = F,
     ylab = expression(sqrt(Amplitude)),
     xlab = "Shock intensity",
     xaxt = "n",
     yaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0.05, 0.35),
     col = col1,
     main = "D. Skin conductance responses, Other"
     )
lines(c(1.1, 2.1), c(eff1$fit[1], eff1$fit[3]), type = "b", col = col2, pch = 16)
lines(c(1, 1), c(eff1$upper[2], eff1$lower[2]), col = col1)
lines(c(2, 2), c(eff1$upper[4], eff1$lower[4]), col = col1)
lines(c(1.1, 1.1), c(eff1$upper[1], eff1$lower[1]), col = col2)
lines(c(2.1, 2.1), c(eff1$upper[3], eff1$lower[3]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("High", "Low"))
axis(2, at = c(0.05, 0.15, 0.25, 0.35))
dev.off()

# Compare plots to less custom-generated output for verification
plot(effect("Treatment*Stimulus*Condition", lme1))
plot(effect("Stimulus*Condition", lme1))
plot(effect("IRI_EC_OtherHigh", lme1))
