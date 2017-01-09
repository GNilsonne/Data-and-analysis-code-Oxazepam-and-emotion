# Script to analyse ratings of unpleasantness in empathy experiment from the oxazepam and emotion project
# Gustav Nilsonne 2015-05-20

# Require packages
library(RCurl) # To read data from GitHub
library(nlme) # To build mixed-effects models
library(effects) # To get confidence intervals on estimates
library(RColorBrewer) # To get good diverging colors for graphs
library(latticeExtra) # To make dotcharts

# Define colors for later
col1 = brewer.pal(8, "Dark2")[1]
col2 = brewer.pal(8, "Dark2")[2]
col3 = brewer.pal(8, "Dark2")[3]
col4 = brewer.pal(8, "Dark2")[4]
col5 = brewer.pal(8, "Dark2")[5]
col6 = brewer.pal(8, "Dark2")[6]
col7 = brewer.pal(8, "Dark2")[7]
col8 = brewer.pal(8, "Dark2")[8]

# Read data
ratingsDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/Ratings.csv", ssl.verifypeer = FALSE)
ratingsData <- read.csv(text = ratingsDataURL)

demDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/demographics.csv", ssl.verifypeer = FALSE)
demData <- read.csv(text = demDataURL)

# Merge data, retain only included participants
ratingsData <- merge(ratingsData, demData, by = "Subject")
ratingsData <- ratingsData[ratingsData$Included_EP == TRUE, ]

# Put data columns in right formats
ratingsData$Subject <- as.factor(ratingsData$Subject)
ratingsData$Treatment <- relevel(ratingsData$Treatment, ref = "Placebo")
ratingsData$Stimulus <- relevel(ratingsData$Stimulus, ref = "Low")
ratingsData$Condition <- relevel(ratingsData$Condition, ref = "Self")

# Analyse data
# Include IRI-EC terms in model since we wish to control for baseline empathic propensity 

# Make a new column to specify an "Other High" contrast in modelling
ratingsData$OtherHigh <- as.numeric(ratingsData$Condition)*as.numeric(ratingsData$Stimulus)
ratingsData$OtherHigh[ratingsData$OtherHigh != 4] <- 0
ratingsData$OtherHigh[ratingsData$OtherHigh == 4] <- 1

# z-transform IRI-EC
ratingsData$IRI_EC_z <- scale(ratingsData$IRI_EC)

# Make a regressor for predictor in other high condition only
ratingsData$IRI_EC_z_OtherHigh <- ratingsData$IRI_EC_z * ratingsData$OtherHigh

# Mean center the new regressor
ratingsData$IRI_EC_z_OtherHigh[ratingsData$IRI_EC_z_OtherHigh > 0 & !is.na(ratingsData$IRI_EC_z_OtherHigh)] <- ratingsData$IRI_EC_z_OtherHigh[ratingsData$IRI_EC_z_OtherHigh > 0 & !is.na(ratingsData$IRI_EC_z_OtherHigh)] - mean(ratingsData$IRI_EC_z_OtherHigh[ratingsData$IRI_EC_z_OtherHigh > 0 & !is.na(ratingsData$IRI_EC_z_OtherHigh)], na.rm = TRUE)

# Build model
lme1 <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + Wave + IRI_EC_z + IRI_EC_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme1)
summary(lme1)
intervals(lme1)

# Post-hoc tests at reviewer's request
lme1b <- lme(Unpleasantness ~ Treatment*Stimulus + Wave + IRI_EC_z, data = ratingsData[ratingsData$Condition == "Self", ], random = ~1|Subject, na.action = na.omit)
plot(lme1b)
summary(lme1b)
intervals(lme1b)

lme1c <- lme(Unpleasantness ~ Treatment*Stimulus + Wave + IRI_EC_z + IRI_EC_z_OtherHigh, data = ratingsData[ratingsData$Condition == "Other", ], random = ~1|Subject, na.action = na.omit)
plot(lme1c)
summary(lme1c)
intervals(lme1c)

# Make plots
eff1 <- effect("Treatment*Stimulus*Condition", lme1)

pdf("Fig_Unpleasantness1.pdf", width = 4, height = 4)
plot(c(eff1$fit[1], eff1$fit[3]),
     type = "n",
     frame.plot = F,
     ylab = "VAS",
     xlab = "Shock intensity",
     xaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0, 50),
     col = col1,
     main = "A. Rated unpleasantness, Self")
lines(c(1.1, 2.1), c(eff1$fit[1], eff1$fit[3]), type = "b", col = col1)
lines(c(1, 2), c(eff1$fit[2], eff1$fit[4]), type = "b", col = col2, pch = 16)
lines(c(1.1, 1.1), c(eff1$upper[1], eff1$lower[1]), col = col1)
lines(c(2.1, 2.1), c(eff1$upper[3], eff1$lower[3]), col = col1)
lines(c(1, 1), c(eff1$upper[2], eff1$lower[2]), col = col2)
lines(c(2, 2), c(eff1$upper[4], eff1$lower[4]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("Low", "High"))
legend("topleft", col = c(col1, col2), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), bty = "n", lty = 1)
dev.off()

pdf("Fig_Unpleasantness2.pdf", width = 4, height = 4)
plot(c(eff1$fit[5], eff1$fit[7]),
     type = "n",
     frame.plot = F,
     ylab = "VAS",
     xlab = "Shock intensity",
     xaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0, 50),
     col = col1,
     main = "B. Rated unpleasantness, Other"
)
lines(c(1.1, 2.1), c(eff1$fit[5], eff1$fit[7]), type = "b", col = col1)
lines(c(1, 2), c(eff1$fit[6], eff1$fit[8]), type = "b", col = col2, pch = 16)
lines(c(1.1, 1.1), c(eff1$upper[5], eff1$lower[5]), col = col1)
lines(c(2.1, 2.1), c(eff1$upper[7], eff1$lower[7]), col = col1)
lines(c(1, 1), c(eff1$upper[6], eff1$lower[6]), col = col2)
lines(c(2, 2), c(eff1$upper[8], eff1$lower[8]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("Low", "High"))
dev.off()

# Compare plots to less custom-generated output for verification
plot(effect("Treatment*Stimulus*Condition", lme1))

# Analyse other rating scales as predictors
# Since rating scales may be collinear, we include them one by one
# The procedure follows the same logic as above

# IRI-PT
ratingsData$IRI_PT_z <- scale(ratingsData$IRI_PT)
ratingsData$IRI_PT_z_OtherHigh <- ratingsData$IRI_PT_z * ratingsData$OtherHigh
ratingsData$IRI_PT_z_OtherHigh[ratingsData$IRI_PT_z_OtherHigh > 0 & !is.na(ratingsData$IRI_PT_z_OtherHigh)] <- ratingsData$IRI_PT_z_OtherHigh[ratingsData$IRI_PT_z_OtherHigh > 0 & !is.na(ratingsData$IRI_PT_z_OtherHigh)] - mean(ratingsData$IRI_PT_z_OtherHigh[ratingsData$IRI_PT_z_OtherHigh > 0 & !is.na(ratingsData$IRI_PT_z_OtherHigh)], na.rm = TRUE)
lme2 <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + Wave + IRI_PT_z + IRI_PT_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme2)
summary(lme2)
intervals(lme2)

# IRI-PD
ratingsData$IRI_PD_z <- scale(ratingsData$IRI_PD)
ratingsData$IRI_PD_z_OtherHigh <- ratingsData$IRI_PD_z * ratingsData$OtherHigh
ratingsData$IRI_PD_z_OtherHigh[ratingsData$IRI_PD_z_OtherHigh > 0 & !is.na(ratingsData$IRI_PD_z_OtherHigh)] <- ratingsData$IRI_PD_z_OtherHigh[ratingsData$IRI_PD_z_OtherHigh > 0 & !is.na(ratingsData$IRI_PD_z_OtherHigh)] - mean(ratingsData$IRI_PD_z_OtherHigh[ratingsData$IRI_PD_z_OtherHigh > 0 & !is.na(ratingsData$IRI_PD_z_OtherHigh)], na.rm = TRUE)
lme3 <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + Wave + IRI_PD_z + IRI_PD_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme3)
summary(lme3)
intervals(lme3)

# IRI-F
ratingsData$IRI_F_z <- scale(ratingsData$IRI_F)
ratingsData$IRI_F_z_OtherHigh <- ratingsData$IRI_F_z * ratingsData$OtherHigh
ratingsData$IRI_F_z_OtherHigh[ratingsData$IRI_F_z_OtherHigh > 0 & !is.na(ratingsData$IRI_F_z_OtherHigh)] <- ratingsData$IRI_F_z_OtherHigh[ratingsData$IRI_F_z_OtherHigh > 0 & !is.na(ratingsData$IRI_F_z_OtherHigh)] - mean(ratingsData$IRI_F_z_OtherHigh[ratingsData$IRI_F_z_OtherHigh > 0 & !is.na(ratingsData$IRI_F_z_OtherHigh)], na.rm = TRUE)
lme4 <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + Wave + IRI_F_z + IRI_F_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme4)
summary(lme4)
intervals(lme4)

# STAI-T
ratingsData$STAI.T_z <- scale(ratingsData$STAI.T)
ratingsData$STAI.T_z_OtherHigh <- ratingsData$STAI.T_z * ratingsData$OtherHigh
ratingsData$STAI.T_z_OtherHigh[ratingsData$STAI.T_z_OtherHigh > 0 & !is.na(ratingsData$STAI.T_z_OtherHigh)] <- ratingsData$STAI.T_z_OtherHigh[ratingsData$STAI.T_z_OtherHigh > 0 & !is.na(ratingsData$STAI.T_z_OtherHigh)] - mean(ratingsData$STAI.T_z_OtherHigh[ratingsData$STAI.T_z_OtherHigh > 0 & !is.na(ratingsData$STAI.T_z_OtherHigh)], na.rm = TRUE)
lme5 <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + Wave + STAI.T_z + STAI.T_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme5)
summary(lme5)
intervals(lme5)

# TAS-20
ratingsData$TAS.20_z <- scale(ratingsData$TAS.20)
ratingsData$TAS.20_z_OtherHigh <- ratingsData$TAS.20_z * ratingsData$OtherHigh
ratingsData$TAS.20_z_OtherHigh[ratingsData$TAS.20_z_OtherHigh > 0 & !is.na(ratingsData$TAS.20_z_OtherHigh)] <- ratingsData$TAS.20_z_OtherHigh[ratingsData$TAS.20_z_OtherHigh > 0 & !is.na(ratingsData$TAS.20_z_OtherHigh)] - mean(ratingsData$TAS.20_z_OtherHigh[ratingsData$TAS.20_z_OtherHigh > 0 & !is.na(ratingsData$TAS.20_z_OtherHigh)], na.rm = TRUE)
lme6 <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + Wave + TAS.20_z + TAS.20_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme6)
summary(lme6)
intervals(lme6)

# PPI-R-SCI
ratingsData$PPI_SCI_z <- ratingsData$PPI_1_SCI_R
ratingsData$PPI_SCI_z[ratingsData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
ratingsData$PPI_SCI_z <- scale(ratingsData$PPI_SCI_z)
ratingsData$PPI_SCI_z_OtherHigh <- ratingsData$PPI_SCI_z * ratingsData$OtherHigh
ratingsData$PPI_SCI_z_OtherHigh[ratingsData$PPI_SCI_z_OtherHigh > 0 & !is.na(ratingsData$PPI_SCI_z_OtherHigh)] <- ratingsData$PPI_SCI_z_OtherHigh[ratingsData$PPI_SCI_z_OtherHigh > 0 & !is.na(ratingsData$PPI_SCI_z_OtherHigh)] - mean(ratingsData$PPI_SCI_z_OtherHigh[ratingsData$PPI_SCI_z_OtherHigh > 0 & !is.na(ratingsData$PPI_SCI_z_OtherHigh)], na.rm = TRUE)
lme7 <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + Wave + PPI_SCI_z + PPI_SCI_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme7)
summary(lme7)
intervals(lme7)

# PPI-R-FD
ratingsData$PPI_FD_z <- ratingsData$PPI_1_FD_R
ratingsData$PPI_FD_z[ratingsData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
ratingsData$PPI_FD_z <- scale(ratingsData$PPI_FD_z)
ratingsData$PPI_FD_z_OtherHigh <- ratingsData$PPI_FD_z * ratingsData$OtherHigh
ratingsData$PPI_FD_z_OtherHigh[ratingsData$PPI_FD_z_OtherHigh > 0 & !is.na(ratingsData$PPI_FD_z_OtherHigh)] <- ratingsData$PPI_FD_z_OtherHigh[ratingsData$PPI_FD_z_OtherHigh > 0 & !is.na(ratingsData$PPI_FD_z_OtherHigh)] - mean(ratingsData$PPI_FD_z_OtherHigh[ratingsData$PPI_FD_z_OtherHigh > 0 & !is.na(ratingsData$PPI_FD_z_OtherHigh)], na.rm = TRUE)
lme8 <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + Wave + PPI_FD_z + PPI_FD_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme8)
summary(lme8)
intervals(lme8)

# PPI-R-C
ratingsData$PPI_C_z <- ratingsData$PPI_1_C_R
ratingsData$PPI_C_z[ratingsData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
ratingsData$PPI_C_z <- scale(ratingsData$PPI_C_z)
ratingsData$PPI_C_z_OtherHigh <- ratingsData$PPI_C_z * ratingsData$OtherHigh
ratingsData$PPI_C_z_OtherHigh[ratingsData$PPI_C_z_OtherHigh > 0 & !is.na(ratingsData$PPI_C_z_OtherHigh)] <- ratingsData$PPI_C_z_OtherHigh[ratingsData$PPI_C_z_OtherHigh > 0 & !is.na(ratingsData$PPI_C_z_OtherHigh)] - mean(ratingsData$PPI_C_z_OtherHigh[ratingsData$PPI_C_z_OtherHigh > 0 & !is.na(ratingsData$PPI_C_z_OtherHigh)], na.rm = TRUE)
lme9 <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + Wave + PPI_C_z + PPI_C_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme9)
summary(lme9)
intervals(lme9)

# Make plots to compare effects for different scales
# Put data in new frame
data_main <- data.frame(scale = "PPI-R-C", beta = intervals(lme9)$fixed[6, 2], lower = intervals(lme9)$fixed[6, 1], upper = intervals(lme9)$fixed[6, 3], group = "PPI", p = round(summary(lme9)$tTable[6, 5], 3))
data_main <- rbind(data_main, data.frame(scale = "PPI-R-FD", beta = intervals(lme8)$fixed[6, 2], lower = intervals(lme8)$fixed[6, 1], upper = intervals(lme8)$fixed[6, 3], group = "PPI", p = round(summary(lme8)$tTable[6, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "PPI-R-SCI", beta = intervals(lme7)$fixed[6, 2], lower = intervals(lme7)$fixed[6, 1], upper = intervals(lme7)$fixed[6, 3], group = "PPI", p = round(summary(lme7)$tTable[6, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "TAS-20", beta = intervals(lme6)$fixed[6, 2], lower = intervals(lme6)$fixed[6, 1], upper = intervals(lme6)$fixed[6, 3], group = "TAS", p = round(summary(lme6)$tTable[6, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "STAI-T", beta = intervals(lme5)$fixed[6, 2], lower = intervals(lme5)$fixed[6, 1], upper = intervals(lme5)$fixed[6, 3], group = "STAI", p = round(summary(lme5)$tTable[6, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "IRI-F", beta = intervals(lme4)$fixed[6, 2], lower = intervals(lme4)$fixed[6, 1], upper = intervals(lme4)$fixed[6, 3], group = "IRI", p = round(summary(lme4)$tTable[6, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "IRI-PD", beta = intervals(lme3)$fixed[6, 2], lower = intervals(lme3)$fixed[6, 1], upper = intervals(lme3)$fixed[6, 3], group = "IRI", p = round(summary(lme3)$tTable[6, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "IRI-PT", beta = intervals(lme2)$fixed[6, 2], lower = intervals(lme2)$fixed[6, 1], upper = intervals(lme2)$fixed[6, 3], group = "IRI", p = round(summary(lme2)$tTable[6, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "IRI-EC", beta = intervals(lme1)$fixed[6, 2], lower = intervals(lme1)$fixed[6, 1], upper = intervals(lme1)$fixed[6, 3], group = "IRI", p = round(summary(lme1)$tTable[6, 5], 3)))

# Make plot
#pdf("Fig_Unpleasantness3.pdf", width = 4, height = 4)
#axis.L <- function(side, ..., line.col){
#  if (side %in% c("bottom", "left")) {
#    col <- trellis.par.get("axis.text")$col
#    axis.default(side, ..., line.col = col)
#    if (side == "bottom")
#      grid::grid.lines(y = 0)
#  }
#}

#sty <- list()
#sty$axis.line$col <- NA
#sty$strip.border$col <- NA
#sty$strip.background$col <- NA
#segplot(scale ~ lower + upper, data = data_main, 
#        centers = beta, 
#        lwd = 2,
#        draw.bands = FALSE,
#        col = c(col8, col8, col8, col3, col6, col5, col5, col5, col5),
#        par.settings = sty,
#        axis=axis.L,
#        xlab = "Beta, 95% CI",
#        main = "A. Rated unpleasantness",
#        panel = function (x,y,z,...){
#          panel.segplot(x,y,z,...)
#          panel.abline(v=0,lty=2)
#        })
#dev.off()

# Put data in new frame
data_emp <- data.frame(scale = "IRI-EC", beta = intervals(lme1)$fixed[7, 2], lower = intervals(lme1)$fixed[7, 1], upper = intervals(lme1)$fixed[7, 3], group = "IRI", p = round(summary(lme1)$tTable[7, 5], 3))
data_emp <- rbind(data_emp, data.frame(scale = "IRI-PT", beta = intervals(lme2)$fixed[7, 2], lower = intervals(lme2)$fixed[7, 1], upper = intervals(lme2)$fixed[7, 3], group = "IRI", p = round(summary(lme2)$tTable[7, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "IRI-PD", beta = intervals(lme3)$fixed[7, 2], lower = intervals(lme3)$fixed[7, 1], upper = intervals(lme3)$fixed[7, 3], group = "IRI", p = round(summary(lme3)$tTable[7, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "IRI-F", beta = intervals(lme4)$fixed[7, 2], lower = intervals(lme4)$fixed[7, 1], upper = intervals(lme4)$fixed[7, 3], group = "IRI", p = round(summary(lme4)$tTable[7, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "STAI-T", beta = intervals(lme5)$fixed[7, 2], lower = intervals(lme5)$fixed[7, 1], upper = intervals(lme5)$fixed[7, 3], group = "STAI", p = round(summary(lme5)$tTable[7, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "TAS-20", beta = intervals(lme6)$fixed[7, 2], lower = intervals(lme6)$fixed[7, 1], upper = intervals(lme6)$fixed[7, 3], group = "TAS", p = round(summary(lme6)$tTable[7, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "PPI-R-SCI", beta = intervals(lme7)$fixed[7, 2], lower = intervals(lme7)$fixed[7, 1], upper = intervals(lme7)$fixed[7, 3], group = "PPI", p = round(summary(lme7)$tTable[7, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "PPI-R-FD", beta = intervals(lme8)$fixed[7, 2], lower = intervals(lme8)$fixed[7, 1], upper = intervals(lme8)$fixed[7, 3], group = "PPI", p = round(summary(lme8)$tTable[7, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "PPI-R-C", beta = intervals(lme9)$fixed[7, 2], lower = intervals(lme9)$fixed[7, 1], upper = intervals(lme9)$fixed[7, 3], group = "PPI", p = round(summary(lme9)$tTable[7, 5], 3)))

# Make plot
pdf("Fig_Unpleasantness_PPIR2.pdf", width = 7, height = 5)
par(mar=c(5.1, 5.4, 4, 14))
plot(x = data_emp$beta, y = c(12:9, 7, 5, 3:1), xlab = expression(beta), ylab = "", frame.plot = F, xlim = c(min(data_emp$lower), max(data_emp$upper)), xaxt = "n", yaxt = "n")
title("A. Rated unpleasantness", line = 2)
abline(v = 0, col = "gray")
axis(1, at = c(-5, 0, 5), labels = c(-5, 0, 5))
points(x = data_emp$beta, y = c(12:9, 7, 5, 3:1), pch = 16)
arrows(data_emp$lower, c(12:9, 7, 5, 3:1), data_emp$upper, c(12:9, 7, 5, 3:1), length = 0, lwd = 1.5)
par(las=1)
mtext(side = 2, at = c(12:9, 7, 5, 3:1), text = c("IRI-EC", " IRI-PT", "IRI-PD", "IRI-F", "STAI-T", "TAS-20", "PPI-R-SCI", "PPI-R-FD", "PPI-R-C"), line = 1)
mtext(side = 4, at = 13, text = expression(beta), line = 1)
mtext(side = 4, at = c(12:9, 7, 5, 3:1), text = round(data_emp$beta, 2), line = 1)
mtext(side = 4, at = 13, text = "95 % CI", line = 4)
CI <- paste("[", round(data_emp$lower, 2), ", ", round(data_emp$upper, 2), "]", sep = "")
mtext(side = 4, at = c(12:9, 7, 5, 3:1), text = CI, line = 4)
mtext(side = 4, at = 13, text = expression(italic(p)), line = 10)
mtext(side = 4, at = c(12:9, 7, 5, 3:1), text = data_emp$p, line = 10)
dev.off()


# Analyse rated pain intensity
# No rating scales to be used as covariates here

# Build model
lme10 <- lme(Intensity ~ Treatment*Stimulus + Wave, data = ratingsData, random = ~1|Subject, na.action = na.omit)

plot(lme10)
summary(lme10)
intervals(lme10)

# Make plots
eff10 <- effect("Treatment*Stimulus", lme10)

pdf("Fig_Unpleasantness5.pdf", width = 4, height = 4)
plot(c(eff10$fit[2], eff10$fit[4]),
     type = "n",
     frame.plot = F,
     ylab = "VAS",
     xlab = "Shock intensity",
     xaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0, 60),
     col = col1,
     main = "C. Rated pain intensity, Self"
)
lines(c(1, 2), c(eff10$fit[2], eff10$fit[4]), type = "b", col = col2, pch = 16)
lines(c(1.1, 2.1), c(eff10$fit[1], eff10$fit[3]), type = "b", col = col1)
lines(c(1.1, 1.1), c(eff10$upper[1], eff10$lower[1]), col = col1)
lines(c(2.1, 2.1), c(eff10$upper[3], eff10$lower[3]), col = col1)
lines(c(1, 1), c(eff10$upper[2], eff10$lower[2]), col = col2)
lines(c(2, 2), c(eff10$upper[4], eff10$lower[4]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("Low", "HIgh"))
#legend("topright", col = c(col1, col2), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), bty = "n", lty = 1)
dev.off()


# Make new plots including tables of effects
data_main2 <- data_main[4:9, ]
data_main2 <- data_main2[rev(rownames(data_main2)),]

pdf("Fig_Unpleasantness6.pdf", width = 6, height = 3.2)
par(mar=c(5.1, 5, 4, 14))
plot(x = data_main2$beta, y = c(8:5, 3, 1), xlab = expression(beta), ylab = "", frame.plot = F, xlim = c(min(data_main2$lower), max(data_main2$upper)), xaxt = "n", yaxt = "n")
title("A. Rated Unpleasantness", line = 2)
abline(v = 0, col = "gray")
axis(1, at = c(-4, 0, 4), labels = c(-4, 0, 4))
points(x = data_main2$beta, y = c(8:5, 3, 1), pch = 16)
arrows(data_main2$lower, c(8:5, 3, 1), data_main2$upper, c(8:5, 3, 1), length = 0, lwd = 1.5)
par(las=1)
mtext(side = 2, at = c(8:5, 3, 1), text = c("IRI-EC", " IRI-PT", "IRI-PD", "IRI-F", "STAI-T", "TAS-20"), line = 1)
mtext(side = 4, at = 9, text = expression(beta), line = 1)
mtext(side = 4, at = c(8:5, 3, 1), text = round(data_main2$beta, 2), line = 1)
mtext(side = 4, at = 9, text = "95 % CI", line = 4)
CI <- paste("[", round(data_main2$lower, 2), ", ", round(data_main2$upper, 2), "]", sep = "")
mtext(side = 4, at = c(8:5, 3, 1), text = CI, line = 4)
mtext(side = 4, at = 9, text = expression(italic(p)), line = 10)
mtext(side = 4, at = c(8:5, 3, 1), text = data_main2$p, line = 10)
dev.off()


data_emp2 <- data_emp[4:9, ]
data_emp2 <- data_emp2[rev(rownames(data_emp2)),]

pdf("Fig_Unpleasantness7.pdf", width = 6, height = 3.2)
par(mar=c(5.1, 5, 4, 14))
plot(x = data_emp2$beta, y = c(8:5, 3, 1), xlab = expression(beta), ylab = "", frame.plot = F, xlim = c(min(data_emp2$lower), max(data_emp2$upper)), xaxt = "n", yaxt = "n")
title("A. Rated Unpleasantness", line = 2)
abline(v = 0, col = "gray")
axis(1, at = c(-6, 0, 6), labels = c(-6, 0, 6))
points(x = data_emp2$beta, y = c(8:5, 3, 1), pch = 16)
arrows(data_emp2$lower, c(8:5, 3, 1), data_emp2$upper, c(8:5, 3, 1), length = 0, lwd = 1.5)
par(las=1)
mtext(side = 2, at = c(8:5, 3, 1), text = c("IRI-EC", " IRI-PT", "IRI-PD", "IRI-F", "STAI-T", "TAS-20"), line = 1)
mtext(side = 4, at = 9, text = expression(beta), line = 1)
mtext(side = 4, at = c(8:5, 3, 1), text = round(data_emp2$beta, 2), line = 1)
mtext(side = 4, at = 9, text = "95 % CI", line = 4)
CI <- paste("[", round(data_emp2$lower, 2), ", ", round(data_emp2$upper, 2), "]", sep = "")
mtext(side = 4, at = c(8:5, 3, 1), text = CI, line = 4)
mtext(side = 4, at = 9, text = expression(italic(p)), line = 10)
mtext(side = 4, at = c(8:5, 3, 1), text = data_emp2$p, line = 10)
dev.off()

# Analyse waves 1 and 2 separately
# Make a new directory if needed
# dir.create("Result_tables”)
summary(lme1)
write.csv(summary(lme1)$tTable, file = "Result_tables/Rated_unpleasantness_Waves1and2.csv")

lmew1 <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh, data = ratingsData[ratingsData$Wave == 1, ], random = ~1|Subject, na.action = na.omit)
plot(lmew1)
summary(lmew1)
intervals(lmew1)
write.csv(summary(lmew1)$tTable, file = "Result_tables/Rated_unpleasantness_Wave1.csv")

lmew2 <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh, data = ratingsData[ratingsData$Wave == 2, ], random = ~1|Subject, na.action = na.omit)
plot(lmew2)
summary(lmew2)
intervals(lmew2)
write.csv(summary(lmew2)$tTable, file = "Result_tables/Rated_unpleasantness_Wave2.csv")

# Analyse effect of rated likability of confederate (wave 2 only)
hist(ratingsData$RatedSympathy)
plot(RatedSympathy ~ IRI_EC, data = ratingsData)
cor.test(ratingsData$RatedSympathy, ratingsData$IRI_EC)

ratingsData$RatedSympathy<- scale(ratingsData$RatedSympathy)
ratingsData$RatedSympathy_OtherHigh <- ratingsData$RatedSympathy * ratingsData$OtherHigh
ratingsData$RatedSympathy_OtherHigh[ratingsData$RatedSympathy_OtherHigh > 0 & !is.na(ratingsData$RatedSympathy_OtherHigh)] <- ratingsData$RatedSympathy_OtherHigh[ratingsData$RatedSympathy_OtherHigh > 0 & !is.na(ratingsData$RatedSympathy_OtherHigh)] - mean(ratingsData$RatedSympathy_OtherHigh[ratingsData$RatedSympathy_OtherHigh > 0 & !is.na(ratingsData$RatedSympathy_OtherHigh)], na.rm = TRUE)

lmew2b <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh + RatedSympathy + RatedSympathy_OtherHigh, data = ratingsData[ratingsData$Wave == 2, ], random = ~1|Subject, na.action = na.omit)
plot(lmew2b)
summary(lmew2b)
intervals(lmew2b)


# Plot effects for PPI-R
data_emp3 <- data_emp[1:3, ]
data_emp3 <- data_emp3[rev(rownames(data_emp3)),]

pdf("Fig_Unpleasantness9.pdf", width = 6, height = 3.2)
par(mar=c(5.1, 5, 4, 14))
plot(x = data_emp3$beta, y = c(1:3), xlab = expression(beta), ylab = "", frame.plot = F, xlim = c(min(data_emp2$lower), max(data_emp2$upper)), xaxt = "n", yaxt = "n")
title("Rated Unpleasantness", line = 2)
abline(v = 0, col = "gray")
axis(1, at = c(-6, 0, 6), labels = c(-6, 0, 6))
points(x = data_emp3$beta, y = c(1:3), pch = 16)
arrows(data_emp3$lower, c(1:3), data_emp3$upper, c(1:3), length = 0, lwd = 1.5)
par(las=1)
mtext(side = 2, at = c(1:3), text = c("SCI", "FD", "C"), line = 1)
mtext(side = 4, at = 3.3, text = expression(beta), line = 1)
mtext(side = 4, at = c(1:3), text = round(data_emp3$beta, 2), line = 1)
mtext(side = 4, at = 3.3, text = "95 % CI", line = 4)
CI <- paste("[", round(data_emp3$lower, 2), ", ", round(data_emp3$upper, 2), "]", sep = "")
mtext(side = 4, at = c(1:3), text = CI, line = 4)
mtext(side = 4, at = 3.3, text = expression(italic(p)), line = 10)
mtext(side = 4, at = c(1:3), text = data_emp3$p, line = 10)
dev.off()

# Make plots for effect of PPI-R
plot(effect("PPI_SCI_z", lme7), main = "")
plot(effect("PPI_SCI_z_OtherHigh", lme7), main = "")

plot(effect("PPI_FD_z", lme8), main = "")
plot(effect("PPI_FD_z_OtherHigh", lme8), main = "")

plot(effect("PPI_C_z", lme9), main = "")
plot(effect("PPI_C_z_OtherHigh", lme9), main = "")

# Calculate a response index for each participant and write
lme1b <- lme(Unpleasantness ~ Treatment*Stimulus*Condition + Wave, data = ratingsData, random = ~1|Subject, na.action = na.omit)
coef <- coef(lme1b)
coef$Subject <- row.names(coef)
coef <- merge(coef, demData[, c("Subject", "Treatment", "Wave")], by = "Subject")
coef$Unpleasantness <- coef$"(Intercept)" + coef$StimulusHigh + coef$ConditionOther + coef$"StimulusHigh:ConditionOther"
IndividualResponses <- coef[, c("Subject", "Unpleasantness")]
write.csv(IndividualResponses, file = "IndividualResponsesUnpleasantness.csv", row.names = FALSE)

# Calculate standardised regression coefficients for the PPI-R subscales
ratingsData$Unpleasantness_z <- scale(ratingsData$Unpleasantness)

# PPI-R-SCI
lme7b <- lme(Unpleasantness_z ~ Treatment*Stimulus*Condition + Wave + PPI_SCI_z + PPI_SCI_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme7b)
summary(lme7b)
intervals(lme7b)

# PPI-R-FD
lme8b <- lme(Unpleasantness_z ~ Treatment*Stimulus*Condition + Wave + PPI_FD_z + PPI_FD_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme8b)
summary(lme8b)
intervals(lme8b)

# PPI-R-C
lme9b <- lme(Unpleasantness_z ~ Treatment*Stimulus*Condition + Wave + PPI_C_z + PPI_C_z_OtherHigh, data = ratingsData, random = ~1|Subject, na.action = na.omit)
plot(lme9b)
summary(lme9b)
intervals(lme9b)
