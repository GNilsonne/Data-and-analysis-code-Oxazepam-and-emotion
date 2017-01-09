# Script to analyse ratings skin conductance responses in empathy experiment from the oxazepam and emotion project
# Gustav Nilsonne 2015-05-20

# Require packages
library(RCurl) # To read data from GitHub
library(nlme) # To build mixed-effects models
library(effects) # To get confidence intervals on estimates
library(RColorBrewer) # To get good colors for graphs
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
SCRDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/SCR.csv", ssl.verifypeer = FALSE)
SCRData <- read.csv(text = SCRDataURL)

demDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/demographics.csv", ssl.verifypeer = FALSE)
demData <- read.csv(text = demDataURL)

# Merge data, retain only included participants
SCRData <- merge(SCRData, demData, by = "Subject")
SCRData <- SCRData[SCRData$Included_EP == TRUE, ]

# Put data columns in right formats
SCRData$Subject <- as.factor(SCRData$Subject)
SCRData$Treatment <- relevel(SCRData$Treatment, ref = "Placebo")
SCRData$Stimulus <- relevel(SCRData$Stimulus, ref = "Low")
SCRData$Condition <- relevel(SCRData$Condition, ref = "Self")

# Investigate normality of distribution of SCR:s
hist(SCRData$SCR)
SCRData$sqrtSCR <- sqrt(SCRData$SCR)
hist(SCRData$sqrtSCR)

# Analyse data
# Include IRI-EC terms in model since we wish to control for baseline empathic propensity 

# Make a new column to specify an "Other High" contrast in modelling
SCRData$OtherHigh <- as.numeric(SCRData$Condition)*as.numeric(SCRData$Stimulus)
SCRData$OtherHigh[SCRData$OtherHigh != 4] <- 0
SCRData$OtherHigh[SCRData$OtherHigh == 4] <- 1

# z-transform IRI-EC
SCRData$IRI_EC_z <- scale(SCRData$IRI_EC)

# Make a regressor for predictor in other high condition only
SCRData$IRI_EC_z_OtherHigh <- SCRData$IRI_EC_z * SCRData$OtherHigh

# Mean center the new regressor
SCRData$IRI_EC_z_OtherHigh[SCRData$IRI_EC_z_OtherHigh > 0 & !is.na(SCRData$IRI_EC_z_OtherHigh)] <- SCRData$IRI_EC_z_OtherHigh[SCRData$IRI_EC_z_OtherHigh > 0 & !is.na(SCRData$IRI_EC_z_OtherHigh)] - mean(SCRData$IRI_EC_z_OtherHigh[SCRData$IRI_EC_z_OtherHigh > 0 & !is.na(SCRData$IRI_EC_z_OtherHigh)], na.rm = TRUE)

# Build model
lme1 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + IRI_EC_z + IRI_EC_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme1)
summary(lme1)
intervals(lme1)

# Post-hoc tests at reviewer's request
lme1b <- lme(sqrtSCR ~ Treatment*Stimulus + Wave + IRI_EC_z, data = SCRData[SCRData$Condition == "Self", ], random = ~1|Subject, na.action = na.omit)
plot(lme1b)
summary(lme1b)
intervals(lme1b)

lme1c <- lme(sqrtSCR ~ Treatment*Stimulus + Wave + IRI_EC_z + IRI_EC_z_OtherHigh, data = SCRData[SCRData$Condition == "Other", ], random = ~1|Subject, na.action = na.omit)
plot(lme1c)
summary(lme1c)
intervals(lme1c)

# Make plots
eff1 <- effect("Treatment*Stimulus*Condition", lme1)

pdf("Fig_SCR1.pdf", width = 4, height = 4)
plot(c(eff1$fit[1], eff1$fit[3]),
     type = "n",
     frame.plot = F,
     ylab = "",
     xlab = "Shock intensity",
     xaxt = "n",
     yaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0.05, 0.35),
     col = col1,
     main = "A. Skin conductance responses, Self"
)
mtext(expression(sqrt(Amplitude)), side = 2, las = 3, line = 2.5)
lines(c(1, 2), c(eff1$fit[2], eff1$fit[4]), type = "b", col = col2, pch = 16)
lines(c(1.1, 2.1), c(eff1$fit[1], eff1$fit[3]), type = "b", col = col1)
lines(c(1.1, 1.1), c(eff1$upper[1], eff1$lower[1]), col = col1)
lines(c(2.1, 2.1), c(eff1$upper[3], eff1$lower[3]), col = col1)
lines(c(1, 1), c(eff1$upper[2], eff1$lower[2]), col = col2)
lines(c(2, 2), c(eff1$upper[4], eff1$lower[4]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("Low", "High"))
axis(2, at = c(0.05, 0.15, 0.25, 0.35))
legend("topleft", col = c(col1, col2), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), bty = "n", lty = 1)
dev.off()

pdf("Fig_SCR2.pdf", width = 4, height = 4)
plot(c(eff1$fit[5], eff1$fit[7]),
     type = "n",
     frame.plot = F,
     ylab = "",
     xlab = "Shock intensity",
     xaxt = "n",
     yaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0.05, 0.35),
     col = col1,
     main = "B. Skin conductance responses, Other"
)
mtext(expression(sqrt(Amplitude)), side = 2, las = 3, line = 2.5)
lines(c(1, 2), c(eff1$fit[6], eff1$fit[8]), type = "b", col = col2, pch = 16)
lines(c(1.1, 2.1), c(eff1$fit[5], eff1$fit[7]), type = "b", col = col1)
lines(c(1.1, 1.1), c(eff1$upper[5], eff1$lower[5]), col = col1)
lines(c(2.1, 2.1), c(eff1$upper[7], eff1$lower[7]), col = col1)
lines(c(1, 1), c(eff1$upper[6], eff1$lower[6]), col = col2)
lines(c(2, 2), c(eff1$upper[8], eff1$lower[8]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("Low", "High"))
axis(2, at = c(0.05, 0.15, 0.25, 0.35))
dev.off()

# Compare plots to less custom-generated output for verification
plot(effect("Treatment*Stimulus*Condition", lme1))
plot(effect("Stimulus*Condition", lme1))
plot(effect("IRI_EC_z_OtherHigh", lme1))

# Analyse other rating scales as predictors
# Since rating scales may be collinear, we include them one by one
# The procedure follows the same logic as above

# IRI-PT
SCRData$IRI_PT_z <- scale(SCRData$IRI_PT)
SCRData$IRI_PT_z_OtherHigh <- SCRData$IRI_PT_z * SCRData$OtherHigh
SCRData$IRI_PT_z_OtherHigh[SCRData$IRI_PT_z_OtherHigh > 0 & !is.na(SCRData$IRI_PT_z_OtherHigh)] <- SCRData$IRI_PT_z_OtherHigh[SCRData$IRI_PT_z_OtherHigh > 0 & !is.na(SCRData$IRI_PT_z_OtherHigh)] - mean(SCRData$IRI_PT_z_OtherHigh[SCRData$IRI_PT_z_OtherHigh > 0 & !is.na(SCRData$IRI_PT_z_OtherHigh)], na.rm = TRUE)
lme2 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + IRI_PT_z + IRI_PT_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme2)
summary(lme2)
intervals(lme2)

# IRI-PD
SCRData$IRI_PD_z <- scale(SCRData$IRI_PD)
SCRData$IRI_PD_z_OtherHigh <- SCRData$IRI_PD_z * SCRData$OtherHigh
SCRData$IRI_PD_z_OtherHigh[SCRData$IRI_PD_z_OtherHigh > 0 & !is.na(SCRData$IRI_PD_z_OtherHigh)] <- SCRData$IRI_PD_z_OtherHigh[SCRData$IRI_PD_z_OtherHigh > 0 & !is.na(SCRData$IRI_PD_z_OtherHigh)] - mean(SCRData$IRI_PD_z_OtherHigh[SCRData$IRI_PD_z_OtherHigh > 0 & !is.na(SCRData$IRI_PD_z_OtherHigh)], na.rm = TRUE)
lme3 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + IRI_PD_z + IRI_PD_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme3)
summary(lme3)
intervals(lme3)

# IRI-F
SCRData$IRI_F_z <- scale(SCRData$IRI_F)
SCRData$IRI_F_z_OtherHigh <- SCRData$IRI_F_z * SCRData$OtherHigh
SCRData$IRI_F_z_OtherHigh[SCRData$IRI_F_z_OtherHigh > 0 & !is.na(SCRData$IRI_F_z_OtherHigh)] <- SCRData$IRI_F_z_OtherHigh[SCRData$IRI_F_z_OtherHigh > 0 & !is.na(SCRData$IRI_F_z_OtherHigh)] - mean(SCRData$IRI_F_z_OtherHigh[SCRData$IRI_F_z_OtherHigh > 0 & !is.na(SCRData$IRI_F_z_OtherHigh)], na.rm = TRUE)
lme4 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + IRI_F_z + IRI_F_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme4)
summary(lme4)
intervals(lme4)

# STAI-T
SCRData$STAI.T_z <- scale(SCRData$STAI.T)
SCRData$STAI.T_z_OtherHigh <- SCRData$STAI.T_z * SCRData$OtherHigh
SCRData$STAI.T_z_OtherHigh[SCRData$STAI.T_z_OtherHigh > 0 & !is.na(SCRData$STAI.T_z_OtherHigh)] <- SCRData$STAI.T_z_OtherHigh[SCRData$STAI.T_z_OtherHigh > 0 & !is.na(SCRData$STAI.T_z_OtherHigh)] - mean(SCRData$STAI.T_z_OtherHigh[SCRData$STAI.T_z_OtherHigh > 0 & !is.na(SCRData$STAI.T_z_OtherHigh)], na.rm = TRUE)
lme5 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + STAI.T_z + STAI.T_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme5)
summary(lme5)
intervals(lme5)

# TAS-20
SCRData$TAS.20_z <- scale(SCRData$TAS.20)
SCRData$TAS.20_z_OtherHigh <- SCRData$TAS.20_z * SCRData$OtherHigh
SCRData$TAS.20_z_OtherHigh[SCRData$TAS.20_z_OtherHigh > 0 & !is.na(SCRData$TAS.20_z_OtherHigh)] <- SCRData$TAS.20_z_OtherHigh[SCRData$TAS.20_z_OtherHigh > 0 & !is.na(SCRData$TAS.20_z_OtherHigh)] - mean(SCRData$TAS.20_z_OtherHigh[SCRData$TAS.20_z_OtherHigh > 0 & !is.na(SCRData$TAS.20_z_OtherHigh)], na.rm = TRUE)
lme6 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + TAS.20_z + TAS.20_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme6)
summary(lme6)
intervals(lme6)

# PPI-R-SCI
SCRData$PPI_SCI_z <- SCRData$PPI_1_SCI_R
SCRData$PPI_SCI_z[SCRData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
SCRData$PPI_SCI_z <- scale(SCRData$PPI_SCI_z)
SCRData$PPI_SCI_z_OtherHigh <- SCRData$PPI_SCI_z * SCRData$OtherHigh
SCRData$PPI_SCI_z_OtherHigh[SCRData$PPI_SCI_z_OtherHigh > 0 & !is.na(SCRData$PPI_SCI_z_OtherHigh)] <- SCRData$PPI_SCI_z_OtherHigh[SCRData$PPI_SCI_z_OtherHigh > 0 & !is.na(SCRData$PPI_SCI_z_OtherHigh)] - mean(SCRData$PPI_SCI_z_OtherHigh[SCRData$PPI_SCI_z_OtherHigh > 0 & !is.na(SCRData$PPI_SCI_z_OtherHigh)], na.rm = TRUE)
lme7 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + PPI_SCI_z + PPI_SCI_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme7)
summary(lme7)
intervals(lme7)

# PPI-R-FD
SCRData$PPI_FD_z <- SCRData$PPI_1_FD_R
SCRData$PPI_FD_z[SCRData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
SCRData$PPI_FD_z <- scale(SCRData$PPI_FD_z)
SCRData$PPI_FD_z_OtherHigh <- SCRData$PPI_FD_z * SCRData$OtherHigh
SCRData$PPI_FD_z_OtherHigh[SCRData$PPI_FD_z_OtherHigh > 0 & !is.na(SCRData$PPI_FD_z_OtherHigh)] <- SCRData$PPI_FD_z_OtherHigh[SCRData$PPI_FD_z_OtherHigh > 0 & !is.na(SCRData$PPI_FD_z_OtherHigh)] - mean(SCRData$PPI_FD_z_OtherHigh[SCRData$PPI_FD_z_OtherHigh > 0 & !is.na(SCRData$PPI_FD_z_OtherHigh)], na.rm = TRUE)
lme8 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + PPI_FD_z + PPI_FD_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme8)
summary(lme8)
intervals(lme8)

# PPI-R-C
SCRData$PPI_C_z <- SCRData$PPI_1_C_R
SCRData$PPI_C_z[SCRData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
SCRData$PPI_C_z <- scale(SCRData$PPI_C_z)
SCRData$PPI_C_z_OtherHigh <- SCRData$PPI_C_z * SCRData$OtherHigh
SCRData$PPI_C_z_OtherHigh[SCRData$PPI_C_z_OtherHigh > 0 & !is.na(SCRData$PPI_C_z_OtherHigh)] <- SCRData$PPI_C_z_OtherHigh[SCRData$PPI_C_z_OtherHigh > 0 & !is.na(SCRData$PPI_C_z_OtherHigh)] - mean(SCRData$PPI_C_z_OtherHigh[SCRData$PPI_C_z_OtherHigh > 0 & !is.na(SCRData$PPI_C_z_OtherHigh)], na.rm = TRUE)
lme9 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave + PPI_C_z + PPI_C_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
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
pdf("Fig_SCR3.pdf", width = 4, height = 4)
axis.L <- function(side, ..., line.col){
  if (side %in% c("bottom", "left")) {
    col <- trellis.par.get("axis.text")$col
    axis.default(side, ..., line.col = col)
  if (side == "bottom")
    grid::grid.lines(y = 0)
  }
}

sty <- list()
sty$axis.line$col <- NA
sty$strip.border$col <- NA
sty$strip.background$col <- NA
segplot(scale ~ lower + upper, data = data_main, 
        centers = beta, 
        lwd = 2,
        draw.bands = FALSE,
        col = c(col8, col8, col8, col3, col6, col5, col5, col5, col5),
        par.settings = sty,
        axis=axis.L,
        xlab = "Beta, 95% CI",
        main = "B. Skin conductance responses",
        panel = function (x,y,z,...){
          panel.segplot(x,y,z,...)
          panel.abline(v=0,lty=2)
        })
dev.off()

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
pdf("Fig_SCR_PPIR2.pdf", width = 7, height = 5)
par(mar=c(5.1, 5.4, 4, 14))
plot(x = data_emp$beta, y = c(12:9, 7, 5, 3:1), xlab = expression(beta), ylab = "", frame.plot = F, xlim = c(min(data_emp$lower), max(data_emp$upper)), xaxt = "n", yaxt = "n")
title("B. Skin conductance responses", line = 2)
abline(v = 0, col = "gray")
axis(1, at = c(-0.03, 0, 0.03), labels = c(-0.03, 0, 0.03))
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


# Make new plots including tables of effects
data_main2 <- data_main[4:9, ]
data_main2 <- data_main2[rev(rownames(data_main2)),]

pdf("Fig_SCR5.pdf", width = 6, height = 3.2)
par(mar=c(5.1, 5, 4, 14))
plot(x = data_main2$beta, y = c(8:5, 3, 1), xlab = expression(beta), ylab = "", frame.plot = F, xlim = c(min(data_main2$lower), max(data_main2$upper)), xaxt = "n", yaxt = "n")
title("B. Skin Conductance Responses", line = 2)
axis(1, at = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02))
abline(v = 0, col = "gray")
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

pdf("Fig_SCR6.pdf", width = 6, height = 3.2)
par(mar=c(5.1, 5, 4, 14))
plot(x = data_emp2$beta, y = c(8:5, 3, 1), xlab = expression(beta), ylab = "", frame.plot = F, xlim = c(min(data_emp2$lower), max(data_emp2$upper)), xaxt = "n", yaxt = "n")
title("B. Skin Conductance Responses", line = 2)
axis(1, at = c(-0.03, 0, 0.03), labels = c(-0.03, 0, 0.03))
abline(v = 0, col = "gray")
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
write.csv(summary(lme1)$tTable, file = "Result_tables/SCR_Waves1and2.csv")

lmew1 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh, data = SCRData[SCRData$Wave == 1, ], random = ~1|Subject, na.action = na.omit)
plot(lmew1)
summary(lmew1)
intervals(lmew1)
write.csv(summary(lmew1)$tTable, file = "Result_tables/SCR_Wave1.csv")

lmew2 <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh, data = SCRData[SCRData$Wave == 2, ], random = ~1|Subject, na.action = na.omit)
plot(lmew2)
summary(lmew2)
intervals(lmew2)
write.csv(summary(lmew2)$tTable, file = "Result_tables/SCR_Wave2.csv")

# Analyse effect of rated likability of confederate (wave 2 only)
SCRData$RatedSympathy<- scale(SCRData$RatedSympathy)
SCRData$RatedSympathy_OtherHigh <- SCRData$RatedSympathy * SCRData$OtherHigh
SCRData$RatedSympathy_OtherHigh[SCRData$RatedSympathy_OtherHigh > 0 & !is.na(SCRData$RatedSympathy_OtherHigh)] <- SCRData$RatedSympathy_OtherHigh[SCRData$RatedSympathy_OtherHigh > 0 & !is.na(SCRData$RatedSympathy_OtherHigh)] - mean(SCRData$RatedSympathy_OtherHigh[SCRData$RatedSympathy_OtherHigh > 0 & !is.na(SCRData$RatedSympathy_OtherHigh)], na.rm = TRUE)

lmew2b <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh + RatedSympathy + RatedSympathy_OtherHigh, data = SCRData[SCRData$Wave == 2, ], random = ~1|Subject, na.action = na.omit)
plot(lmew2b)
summary(lmew2b)
intervals(lmew2b)

# Make plots for effect of PPI-R
plot(effect("PPI_SCI_z", lme7), main = "")
plot(effect("PPI_SCI_z_OtherHigh", lme7), main = "")

plot(effect("PPI_FD_z", lme8), main = "")
plot(effect("PPI_FD_z_OtherHigh", lme8), main = "")

plot(effect("PPI_C_z", lme9), main = "")
plot(effect("PPI_C_z_OtherHigh", lme9), main = "")

# Calculate a response index for each participant and write
lme1b <- lme(sqrtSCR ~ Treatment*Stimulus*Condition + Wave, data = SCRData, random = ~1|Subject, na.action = na.omit)
coef <- coef(lme1b)
coef$Subject <- row.names(coef)
coef <- merge(coef, demData[, c("Subject", "Treatment", "Wave")], by = "Subject")
coef$sqrtSCR <- coef$"(Intercept)" + coef$StimulusHigh + coef$ConditionOther + coef$"StimulusHigh:ConditionOther"
IndividualResponses <- coef[, c("Subject", "sqrtSCR")]
write.csv(IndividualResponses, file = "IndividualResponsesSCR.csv", row.names = FALSE)

# Calculate standardised regression coefficients for the PPI-R subscales
SCRData$sqrtSCR_z <- scale(SCRData$sqrtSCR)

# PPI-R-SCI
lme7b <- lme(sqrtSCR_z ~ Treatment*Stimulus*Condition + Wave + PPI_SCI_z + PPI_SCI_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme7b)
summary(lme7b)
intervals(lme7b)

# PPI-R-FD
lme8b <- lme(sqrtSCR_z ~ Treatment*Stimulus*Condition + Wave + PPI_FD_z + PPI_FD_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme8b)
summary(lme8b)
intervals(lme8b)

# PPI-R-C
lme9b <- lme(sqrtSCR_z ~ Treatment*Stimulus*Condition + Wave + PPI_C_z + PPI_C_z_OtherHigh, data = SCRData, random = ~1|Subject, na.action = na.omit)
plot(lme9b)
summary(lme9b)
intervals(lme9b)
