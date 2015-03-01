# Script to analyse ratings of unpleasantness in empathy experiment from the oxazepam and emotion project
# Gustav Nilsonne 2015-02-01

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
ratingsData$Treatment <- as.factor(ratingsData$Treatment)
ratingsData$Stimulus <- as.factor(ratingsData$Stimulus)
ratingsData$Condition <- as.factor(ratingsData$Condition)

# Analyse data
# Include IRI-EC terms in model since we wish to control for baseline empathic propensity 

# Make a new column to specify an "Other High" contrast in modelling
ratingsData$OtherHigh <- as.numeric(ratingsData$Condition)*as.numeric(ratingsData$Stimulus)
ratingsData$OtherHigh[ratingsData$OtherHigh == 1] <- 1
ratingsData$OtherHigh[ratingsData$OtherHigh != 1] <- 0

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

# Make plots
eff1 <- effect("Treatment*Stimulus*Condition", lme1)

pdf("Fig_Unpleasantness1.pdf", width = 4, height = 4)
plot(c(eff1$fit[6], eff1$fit[8]),
     type = "b",
     frame.plot = F,
     ylab = "VAS",
     xlab = "Shock intensity",
     xaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0, 50),
     col = col1,
     main = "A. Rated unpleasantness, Self"
     )
lines(c(1.1, 2.1), c(eff1$fit[5], eff1$fit[7]), type = "b", col = col2, pch = 16)
lines(c(1, 1), c(eff1$upper[6], eff1$lower[6]), col = col1)
lines(c(2, 2), c(eff1$upper[8], eff1$lower[8]), col = col1)
lines(c(1.1, 1.1), c(eff1$upper[5], eff1$lower[5]), col = col2)
lines(c(2.1, 2.1), c(eff1$upper[7], eff1$lower[7]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("High", "Low"))
legend("topright", col = c(col1, col2), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), bty = "n", lty = 1)
dev.off()

pdf("Fig_Unpleasantness2.pdf", width = 4, height = 4)
plot(c(eff1$fit[2], eff1$fit[4]),
     type = "b",
     frame.plot = F,
     ylab = "VAS",
     xlab = "Shock intensity",
     xaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0, 50),
     col = col1,
     main = "B. Rated unpleasantness, Other")
lines(c(1.1, 2.1), c(eff1$fit[1], eff1$fit[3]), type = "b", col = col2, pch = 16)
lines(c(1, 1), c(eff1$upper[2], eff1$lower[2]), col = col1)
lines(c(2, 2), c(eff1$upper[4], eff1$lower[4]), col = col1)
lines(c(1.1, 1.1), c(eff1$upper[1], eff1$lower[1]), col = col2)
lines(c(2.1, 2.1), c(eff1$upper[3], eff1$lower[3]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("High", "Low"))
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
data_main <- data.frame(scale = "PPI-R-C", beta = intervals(lme9)$fixed[6, 2], lower = intervals(lme9)$fixed[6, 1], upper = intervals(lme9)$fixed[6, 3], group = "PPI")
data_main <- rbind(data_main, data.frame(scale = "PPI-R-FD", beta = intervals(lme8)$fixed[6, 2], lower = intervals(lme8)$fixed[6, 1], upper = intervals(lme8)$fixed[6, 3], group = "PPI"))
data_main <- rbind(data_main, data.frame(scale = "PPI-R-SCI", beta = intervals(lme7)$fixed[6, 2], lower = intervals(lme7)$fixed[6, 1], upper = intervals(lme7)$fixed[6, 3], group = "PPI"))
data_main <- rbind(data_main, data.frame(scale = "TAS-20", beta = intervals(lme6)$fixed[6, 2], lower = intervals(lme6)$fixed[6, 1], upper = intervals(lme6)$fixed[6, 3], group = "TAS"))
data_main <- rbind(data_main, data.frame(scale = "STAI-T", beta = intervals(lme5)$fixed[6, 2], lower = intervals(lme5)$fixed[6, 1], upper = intervals(lme5)$fixed[6, 3], group = "STAI"))
data_main <- rbind(data_main, data.frame(scale = "IRI-F", beta = intervals(lme4)$fixed[6, 2], lower = intervals(lme4)$fixed[6, 1], upper = intervals(lme4)$fixed[6, 3], group = "IRI"))
data_main <- rbind(data_main, data.frame(scale = "IRI-PD", beta = intervals(lme3)$fixed[6, 2], lower = intervals(lme3)$fixed[6, 1], upper = intervals(lme3)$fixed[6, 3], group = "IRI"))
data_main <- rbind(data_main, data.frame(scale = "IRI-PT", beta = intervals(lme2)$fixed[6, 2], lower = intervals(lme2)$fixed[6, 1], upper = intervals(lme2)$fixed[6, 3], group = "IRI"))
data_main <- rbind(data_main, data.frame(scale = "IRI-EC", beta = intervals(lme1)$fixed[6, 2], lower = intervals(lme1)$fixed[6, 1], upper = intervals(lme1)$fixed[6, 3], group = "IRI"))

# Make plot
pdf("Fig_Unpleasantness3.pdf", width = 4, height = 4)
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
        main = "A. Rated unpleasantness",
        panel = function (x,y,z,...){
          panel.segplot(x,y,z,...)
          panel.abline(v=0,lty=2)
        })
dev.off()

# Put data in new frame
data_emp <- data.frame(scale = "PPI-R-C", beta = intervals(lme9)$fixed[7, 2], lower = intervals(lme9)$fixed[7, 1], upper = intervals(lme9)$fixed[7, 3], group = "PPI")
data_emp <- rbind(data_emp, data.frame(scale = "PPI-R-FD", beta = intervals(lme8)$fixed[7, 2], lower = intervals(lme8)$fixed[7, 1], upper = intervals(lme8)$fixed[7, 3], group = "PPI"))
data_emp <- rbind(data_emp, data.frame(scale = "PPI-R-SCI", beta = intervals(lme7)$fixed[7, 2], lower = intervals(lme7)$fixed[7, 1], upper = intervals(lme7)$fixed[7, 3], group = "PPI"))
data_emp <- rbind(data_emp, data.frame(scale = "TAS-20", beta = intervals(lme6)$fixed[7, 2], lower = intervals(lme6)$fixed[7, 1], upper = intervals(lme6)$fixed[7, 3], group = "TAS"))
data_emp <- rbind(data_emp, data.frame(scale = "STAI-T", beta = intervals(lme5)$fixed[7, 2], lower = intervals(lme5)$fixed[7, 1], upper = intervals(lme5)$fixed[7, 3], group = "STAI"))
data_emp <- rbind(data_emp, data.frame(scale = "IRI-F", beta = intervals(lme4)$fixed[7, 2], lower = intervals(lme4)$fixed[7, 1], upper = intervals(lme4)$fixed[7, 3], group = "IRI"))
data_emp <- rbind(data_emp, data.frame(scale = "IRI-PD", beta = intervals(lme3)$fixed[7, 2], lower = intervals(lme3)$fixed[7, 1], upper = intervals(lme3)$fixed[7, 3], group = "IRI"))
data_emp <- rbind(data_emp, data.frame(scale = "IRI-PT", beta = intervals(lme2)$fixed[7, 2], lower = intervals(lme2)$fixed[7, 1], upper = intervals(lme2)$fixed[7, 3], group = "IRI"))
data_emp <- rbind(data_emp, data.frame(scale = "IRI-EC", beta = intervals(lme1)$fixed[7, 2], lower = intervals(lme1)$fixed[7, 1], upper = intervals(lme1)$fixed[7, 3], group = "IRI"))

# Make plot
pdf("Fig_Unpleasantness4.pdf", width = 4, height = 4)
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
segplot(scale ~ lower + upper, data = data_emp, 
        centers = beta, 
        lwd = 2,
        draw.bands = FALSE,
        col = c(col8, col8, col8, col3, col6, col5, col5, col5, col5),
        par.settings = sty,
        axis=axis.L,
        xlab = "Beta, 95% CI",
        main = "A. Rated unpleasantness",
        panel = function (x,y,z,...){
          panel.segplot(x,y,z,...)
          panel.abline(v=0,lty=2)
        })
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
     type = "b",
     frame.plot = F,
     ylab = "VAS",
     xlab = "Shock intensity",
     xaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0, 60),
     col = col1,
     main = "C. Rated pain intensity, Self"
)
lines(c(1.1, 2.1), c(eff10$fit[1], eff10$fit[3]), type = "b", col = col2, pch = 16)
lines(c(1, 1), c(eff10$upper[2], eff10$lower[2]), col = col1)
lines(c(2, 2), c(eff10$upper[4], eff10$lower[4]), col = col1)
lines(c(1.1, 1.1), c(eff10$upper[1], eff10$lower[1]), col = col2)
lines(c(2.1, 2.1), c(eff10$upper[3], eff10$lower[3]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("High", "Low"))
#legend("topright", col = c(col1, col2), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), bty = "n", lty = 1)
dev.off()

