# Script to analyse ratings of unpleasantness in empathy experiment from the oxazepam and emotion project
# Gustav Nilsonne 2015-01-10

# Require packages
library(RCurl) # To read data from GitHub
library(nlme) # To build mixed-effects models
library(effects) # To get confidence intervals on estimates
library(RColorBrewer) # To get good colors for graphs
#library(lattice) # To make dotplots

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
lme1 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + IRI_EC + IRI_EC_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)

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

# Analyse other rating scales as predictors
# Since rating scales may be collinear, we include them one by one
# The procedure follows the same logic as above

# IRI-PT
SCRData$IRI_PT_OtherHigh <- SCRData$IRI_PT * SCRData$OtherHigh
SCRData$IRI_PT_OtherHigh[SCRData$IRI_PT_OtherHigh > 0 & !is.na(SCRData$IRI_PT_OtherHigh)] <- SCRData$IRI_PT_OtherHigh[SCRData$IRI_PT_OtherHigh > 0 & !is.na(SCRData$IRI_PT_OtherHigh)] - mean(SCRData$IRI_PT_OtherHigh[SCRData$IRI_PT_OtherHigh > 0 & !is.na(SCRData$IRI_PT_OtherHigh)], na.rm = TRUE)
lme2 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + IRI_PT + IRI_PT_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme2)
summary(lme2)
intervals(lme2)

# IRI-PD
SCRData$IRI_PD_OtherHigh <- SCRData$IRI_PD * SCRData$OtherHigh
SCRData$IRI_PD_OtherHigh[SCRData$IRI_PD_OtherHigh > 0 & !is.na(SCRData$IRI_PD_OtherHigh)] <- SCRData$IRI_PD_OtherHigh[SCRData$IRI_PD_OtherHigh > 0 & !is.na(SCRData$IRI_PD_OtherHigh)] - mean(SCRData$IRI_PD_OtherHigh[SCRData$IRI_PD_OtherHigh > 0 & !is.na(SCRData$IRI_PD_OtherHigh)], na.rm = TRUE)
lme3 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + IRI_PD + IRI_PD_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme3)
summary(lme3)
intervals(lme3)

# IRI-F
SCRData$IRI_F_OtherHigh <- SCRData$IRI_F * SCRData$OtherHigh
SCRData$IRI_F_OtherHigh[SCRData$IRI_F_OtherHigh > 0 & !is.na(SCRData$IRI_F_OtherHigh)] <- SCRData$IRI_F_OtherHigh[SCRData$IRI_F_OtherHigh > 0 & !is.na(SCRData$IRI_F_OtherHigh)] - mean(SCRData$IRI_F_OtherHigh[SCRData$IRI_F_OtherHigh > 0 & !is.na(SCRData$IRI_F_OtherHigh)], na.rm = TRUE)
lme4 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + IRI_F + IRI_F_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme4)
summary(lme4)
intervals(lme4)

# STAI-T
#SCRData$IRI_F_OtherHigh <- SCRData$IRI_F * SCRData$OtherHigh
#SCRData$IRI_F_OtherHigh[SCRData$IRI_F_OtherHigh > 0 & !is.na(SCRData$IRI_F_OtherHigh)] <- SCRData$IRI_F_OtherHigh[SCRData$IRI_F_OtherHigh > 0 & !is.na(SCRData$IRI_F_OtherHigh)] - mean(SCRData$IRI_F_OtherHigh[SCRData$IRI_F_OtherHigh > 0 & !is.na(SCRData$IRI_F_OtherHigh)], na.rm = TRUE)
#lme4 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + IRI_F + IRI_F_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
#plot(lme4)
#summary(lme4)
#intervals(lme4)

# TAS-20
SCRData$TAS.20_OtherHigh <- SCRData$TAS.20 * SCRData$OtherHigh
SCRData$TAS.20_OtherHigh[SCRData$TAS.20_OtherHigh > 0 & !is.na(SCRData$TAS.20_OtherHigh)] <- SCRData$TAS.20_OtherHigh[SCRData$TAS.20_OtherHigh > 0 & !is.na(SCRData$TAS.20_OtherHigh)] - mean(SCRData$TAS.20_OtherHigh[SCRData$TAS.20_OtherHigh > 0 & !is.na(SCRData$TAS.20_OtherHigh)], na.rm = TRUE)
lme6 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + TAS.20 + TAS.20_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme6)
summary(lme6)
intervals(lme6)

# Make plots to compare effects for different scales
col3 = brewer.pal(6, "Dark2")[3]
col4 = brewer.pal(6, "Dark2")[4]
col5 = brewer.pal(6, "Dark2")[5]

data_main <- data.frame(scale = "IRI-EC", beta = intervals(lme1)$fixed[6, 2], lower = intervals(lme1)$fixed[6, 1], upper = intervals(lme1)$fixed[6, 3], group = "IRI", col = col3)
data_main <- rbind(data_main, data.frame(scale = "IRI-PT", beta = intervals(lme2)$fixed[6, 2], lower = intervals(lme2)$fixed[6, 1], upper = intervals(lme2)$fixed[6, 3], group = "IRI", col = col3))
data_main <- rbind(data_main, data.frame(scale = "IRI-PD", beta = intervals(lme3)$fixed[6, 2], lower = intervals(lme3)$fixed[6, 1], upper = intervals(lme3)$fixed[6, 3], group = "IRI", col = col3))
data_main <- rbind(data_main, data.frame(scale = "IRI-F", beta = intervals(lme4)$fixed[6, 2], lower = intervals(lme4)$fixed[6, 1], upper = intervals(lme4)$fixed[6, 3], group = "IRI", col = col3))
data_main <- rbind(data_main, data.frame(scale = "TAS-20", beta = intervals(lme6)$fixed[6, 2], lower = intervals(lme6)$fixed[6, 1], upper = intervals(lme6)$fixed[6, 3], group = "TAS", col = col4))


par(bty = 'n') 
dotchart(data_main$beta, 
         labels = data_main$scale, 
         groups = data_main$groups, 
         xlim = c(-0.05, 0.05),
         main = "", 
         xlab = "Beta", 
         lcolor = "white", 
         color = c(col3, col3, col3, col3, col4),
         pch = 16)
abline(v = 0, lty = 2)
lines(c(data_main$lower[1], data_main$upper[1]), c(1, 1), col = col3)
lines(c(data_main$lower[2], data_main$upper[2]), c(2, 2), col = col3)
lines(c(data_main$lower[3], data_main$upper[3]), c(3, 3), col = col3)
lines(c(data_main$lower[4], data_main$upper[4]), c(4, 4), col = col3)
lines(c(data_main$lower[5], data_main$upper[5]), c(5, 5), col = col4)
lines(c(data_main$lower[6], data_main$upper[6]), c(6, 6), col = col3)
