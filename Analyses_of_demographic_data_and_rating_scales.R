# Script to analyse demographic data and rating scales from the oxazepam and emotion project
# Gustav Nilsonne 2015-01-09

# Require packages
library(RCurl) # To read data from GitHub
library(nlme) # To build mixed-effects models
library(effects) # To get confidence intervals on estimates

# Read data
demDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/demographics.csv", ssl.verifypeer = FALSE)
demData <- read.csv(text = demDataURL)

# Descriptive analyses, n per group
length(demData$Subject[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Placebo"])
length(demData$Subject[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"])
length(demData$Subject[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Placebo"])
length(demData$Subject[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"])

# Descriptive analyses, IRI
mean(demData$IRI_EC[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$IRI_EC[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$IRI_EC[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$IRI_EC[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
mean(demData$IRI_EC[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$IRI_EC[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$IRI_EC[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$IRI_EC[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)

mean(demData$IRI_PT[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$IRI_PT[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$IRI_PT[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$IRI_PT[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
mean(demData$IRI_PT[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$IRI_PT[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$IRI_PT[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$IRI_PT[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)

mean(demData$IRI_PD[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$IRI_PD[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$IRI_PD[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$IRI_PD[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
mean(demData$IRI_PD[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$IRI_PD[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$IRI_PD[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$IRI_PD[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)

mean(demData$IRI_F[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$IRI_F[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$IRI_F[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$IRI_F[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
mean(demData$IRI_F[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$IRI_F[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$IRI_F[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$IRI_F[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"], na.rm = T)

# Analyse effect of oxazepam on rated state anxiety
#Make dataframe for mixed-effects model
STAISData <- rbind(demData[, c("Subject", "Treatment", "Wave", "Included_EP", "STAI.S", "STAI.S.Scrambled")], demData[, c("Subject", "Treatment", "Wave", "Included_EP", "STAI.S", "STAI.S.Scrambled")]) 
STAISData <- STAISData[STAISData$Included_EP == T, ] # Remove participants not included in this experiment
STAISData$FirstOrSecond <- c(rep.int(1, 0.5*length(STAISData$Subject)), rep.int(2, 0.5*length(STAISData$Subject)))
STAISData$STAIS <- NA # Make new column for STAI-S rating, then fill it with values for the first and second ratings, respectively
STAISData$STAIS[STAISData$FirstOrSecond == 1] <- STAISData$STAI.S[STAISData$FirstOrSecond == 1]
STAISData$STAIS[STAISData$FirstOrSecond == 2] <- STAISData$STAI.S.Scrambled[STAISData$FirstOrSecond == 2]

lme1 <- lme(STAIS ~ Treatment * FirstOrSecond + Wave, data = STAISData, random = ~1|Subject, na.action = na.omit)
summary(lme1)

# Inspect residuals
plot(lme1)

# Get estimates
intervals(lme1)

# Plot effects
eff1 <- effect("Treatment * FirstOrSecond", lme1)

pdf("Fig_STAIS.pdf")
plot(eff1$fit[c(2, 4)],
     frame.plot = F,
     xaxt = "n",
     type = "b",
     xlab = "",
     ylab = "STAI-S",
     xlim = c(1, 2.1),
     ylim = c(33, 38),
     col = "blue")
lines(c(1.1,2.1), eff1$fit[c(1, 3)], type = "b", col = "red", pch = 16)
lines(c(1, 1), c((eff1$upper[2]), (eff1$lower[2])), col = "blue")
lines(c(2, 2), c((eff1$upper[4]), (eff1$lower[4])), col = "blue")
lines(c(1.1, 1.1), c((eff1$upper[1]), (eff1$lower[1])), col = "red")
lines(c(2.1, 2.1), c((eff1$upper[3]), (eff1$lower[3])), col = "red")
axis(1, labels = c("First", "Second"), at = c(1.05, 2.05))
#legend("top", col = c("blue", "red"), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), bty = "n")
dev.off()

