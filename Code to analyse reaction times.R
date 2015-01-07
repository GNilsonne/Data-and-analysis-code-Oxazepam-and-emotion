# Script to analyse response times from the oxazepam and emotion project
# Gustav Nilsonne 2015-01-07

# Require packages
library(RCurl) # To read data from GitHub
library(nlme) # To build mixed-effects models
library(effects) # To get confidence intervals on estimates

# Read data
RTDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/Reaction-time-test/Reaction_time_data.csv", ssl.verifypeer = FALSE)
RTData <- read.csv(text = RTDataURL)

# Inverse transform
RTData$Inv_RT <- 1/RTData$ResponseTime_ms

# Verify normality of distribution
plot(density(RTData$ResponseTime_ms))
plot(density(RTData$Inv_RT))

# Test effects
RTData$FirstOrSecondTest <- as.factor(RTData$FirstOrSecondTest)
lme1 <- lme(Inv_RT ~ Treatment * FirstOrSecondTest + Wave, data = RTData, random = ~1|Subject)
summary(lme1)

# Inspect residuals
plot(lme1)

# Get estimates
intervals(lme1)
1/intervals(lme1)$fixed
1/intervals(lme1)$fixed[1, 2] - 1/(abs(intervals(lme1)$fixed) + abs(intervals(lme1)$fixed[1, 2]))

# Plot effects with transformation back to original scale
eff1 <- effect("Treatment * FirstOrSecondTest", lme1)

pdf("Fig_RT.pdf")
plot(1/eff1$fit[c(2, 4)],
     frame.plot = F,
     xaxt = "n",
     type = "b",
     xlab = "",
     ylab = "Reaction time (ms)",
     xlim = c(1, 2.1),
     ylim = c(300, 350),
     col = "blue")
lines(c(1.1,2.1), 1/eff1$fit[c(1, 3)], type = "b", col = "red", pch = 16)
lines(c(1, 1), c((1/eff1$upper[2]), (1/eff1$lower[2])), col = "blue")
lines(c(2, 2), c((1/eff1$upper[4]), (1/eff1$lower[4])), col = "blue")
lines(c(1.1, 1.1), c((1/eff1$upper[1]), (1/eff1$lower[1])), col = "red")
lines(c(2.1, 2.1), c((1/eff1$upper[3]), (1/eff1$lower[3])), col = "red")
axis(1, labels = c("First", "Second"), at = c(1.05, 2.05))
legend("topleft", col = c("blue", "red"), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), bty = "n")
dev.off()
