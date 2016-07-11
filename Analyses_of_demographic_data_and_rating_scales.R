# Script to analyse demographic data and rating scales from the oxazepam and emotion project
# Gustav Nilsonne 2015-01-09

# Require packages
library(RCurl) # To read data from GitHub
library(nlme) # To build mixed-effects models
library(effects) # To get confidence intervals on estimates
library(RColorBrewer) # To get good diverging colors for graphs

# Define colors for later
col1 = brewer.pal(3, "Dark2")[1]
col2 = brewer.pal(3, "Dark2")[2]

add.alpha <- function(col, alpha=1){ ## Function to add an alpha value to a colour, from: http://www.magesblog.com/2013/04/how-to-change-alpha-value-of-colours-in.html
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

col3 <- add.alpha(col1, alpha = 0.2)

# Read data
demDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/demographics.csv", ssl.verifypeer = FALSE)
demData <- read.csv(text = demDataURL)

# Descriptive analyses, n per group
length(demData$Subject[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"])
length(demData$Subject[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"])
length(demData$Subject[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"])
length(demData$Subject[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"])

length(demData$Subject[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Placebo"])
length(demData$Subject[demData$Wave == 1 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"])
length(demData$Subject[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Placebo"])
length(demData$Subject[demData$Wave == 2 & demData$Included_EP == 1 & demData$Treatment == "Oxazepam"])

length(demData$Subject[demData$Wave == 1 & demData$Included_FMOV == 1 & demData$Treatment == "Placebo"])
length(demData$Subject[demData$Wave == 1 & demData$Included_FMOV == 1 & demData$Treatment == "Oxazepam"])
length(demData$Subject[demData$Wave == 2 & demData$Included_FMOV == 1 & demData$Treatment == "Placebo"])
length(demData$Subject[demData$Wave == 2 & demData$Included_FMOV == 1 & demData$Treatment == "Oxazepam"])

# Descriptive analyses, IRI
IRI1 <- mean(demData$IRI_EC[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI2 <- sd(demData$IRI_EC[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI3 <- mean(demData$IRI_EC[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI4 <- sd(demData$IRI_EC[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI5 <- mean(demData$IRI_EC[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI6 <- sd(demData$IRI_EC[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI7 <- mean(demData$IRI_EC[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI8 <- sd(demData$IRI_EC[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

IRI9 <- mean(demData$IRI_PT[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI10 <- sd(demData$IRI_PT[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI11 <- mean(demData$IRI_PT[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI12 <- sd(demData$IRI_PT[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI13 <- mean(demData$IRI_PT[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI14 <- sd(demData$IRI_PT[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI15 <- mean(demData$IRI_PT[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI16 <- sd(demData$IRI_PT[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

IRI17 <- mean(demData$IRI_PD[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI18 <- sd(demData$IRI_PD[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI19 <- mean(demData$IRI_PD[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI20 <- sd(demData$IRI_PD[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI21 <- mean(demData$IRI_PD[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI22 <- sd(demData$IRI_PD[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI23 <- mean(demData$IRI_PD[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI24 <- sd(demData$IRI_PD[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

IRI25 <- mean(demData$IRI_F[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI26 <- sd(demData$IRI_F[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI27 <- mean(demData$IRI_F[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI28 <- sd(demData$IRI_F[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI29 <- mean(demData$IRI_F[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI30 <- sd(demData$IRI_F[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
IRI31 <- mean(demData$IRI_F[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
IRI32 <- sd(demData$IRI_F[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

# Descriptive analyses, TAS-20
TAS1 <- mean(demData$TAS.20[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
TAS2 <- sd(demData$TAS.20[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
TAS3 <- mean(demData$TAS.20[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
TAS4 <- sd(demData$TAS.20[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
TAS5 <- mean(demData$TAS.20[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
TAS6 <- sd(demData$TAS.20[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
TAS7 <- mean(demData$TAS.20[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
TAS8 <- sd(demData$TAS.20[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

mean(demData$Difficulty.identifying.feelings[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$Difficulty.identifying.feelings[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$Difficulty.identifying.feelings[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$Difficulty.identifying.feelings[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
mean(demData$Difficulty.identifying.feelings[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$Difficulty.identifying.feelings[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$Difficulty.identifying.feelings[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$Difficulty.identifying.feelings[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

mean(demData$Difficulty.describing.feelings[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$Difficulty.describing.feelings[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$Difficulty.describing.feelings[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$Difficulty.describing.feelings[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
mean(demData$Difficulty.describing.feelings[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$Difficulty.describing.feelings[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$Difficulty.describing.feelings[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$Difficulty.describing.feelings[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

mean(demData$Externally.oriented.thinking[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$Externally.oriented.thinking[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$Externally.oriented.thinking[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$Externally.oriented.thinking[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
mean(demData$Externally.oriented.thinking[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
sd(demData$Externally.oriented.thinking[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
mean(demData$Externally.oriented.thinking[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
sd(demData$Externally.oriented.thinking[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

# Descriptive analyses, STAI-T
STAI1 <- mean(demData$STAI.T[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
STAI2 <- sd(demData$STAI.T[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
STAI3 <- mean(demData$STAI.T[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
STAI4 <- sd(demData$STAI.T[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
STAI5 <- mean(demData$STAI.T[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
STAI6 <- sd(demData$STAI.T[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
STAI7 <- mean(demData$STAI.T[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
STAI8 <- sd(demData$STAI.T[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

# Descriptive analyses, PPI-R
PPI1 <- mean(demData$PPI_1_SCI_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI2 <- sd(demData$PPI_1_SCI_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI3 <- mean(demData$PPI_1_SCI_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
PPI4 <- sd(demData$PPI_1_SCI_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
PPI5 <- mean(demData$PPI_1_SCI_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI6 <- sd(demData$PPI_1_SCI_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI7 <- mean(demData$PPI_1_SCI_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
PPI8 <- sd(demData$PPI_1_SCI_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

PPI9 <- mean(demData$PPI_1_FD_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI10 <- sd(demData$PPI_1_FD_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI11 <- mean(demData$PPI_1_FD_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
PPI12 <- sd(demData$PPI_1_FD_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
PPI13 <- mean(demData$PPI_1_FD_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI14 <- sd(demData$PPI_1_FD_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI15 <- mean(demData$PPI_1_FD_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
PPI16 <- sd(demData$PPI_1_FD_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

PPI17 <- mean(demData$PPI_1_C_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI18 <- sd(demData$PPI_1_C_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI19 <- mean(demData$PPI_1_C_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
PPI20 <- sd(demData$PPI_1_C_R[demData$Wave == 1 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
PPI21 <- mean(demData$PPI_1_C_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI22 <- sd(demData$PPI_1_C_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Placebo"], na.rm = T)
PPI23 <- mean(demData$PPI_1_C_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)
PPI24 <- sd(demData$PPI_1_C_R[demData$Wave == 2 & (demData$Included_EP == 1 | demData$Included_FMOV == 1) & demData$Treatment == "Oxazepam"], na.rm = T)

# Print table with results from above lines
outtable <- data.frame(var = paste("IRI-EC", " & ", round(IRI1, 2), " (", round(IRI2, 2), ") & ", 
                                                    round(IRI3, 2), " (", round(IRI4, 2), ") & ",
                                                    round(IRI5, 2), " (", round(IRI6, 2), ") & ",
                                                    round(IRI7, 2), " (", round(IRI8, 2), ")", sep = ""))
outtable <- rbind(outtable, data.frame(var = paste("IRI-PT", " & ", round(IRI9, 2), " (", round(IRI10, 2), ") & ", 
                                                   round(IRI11, 2), " (", round(IRI12, 2), ") & ",
                                                   round(IRI13, 2), " (", round(IRI14, 2), ") & ",
                                                   round(IRI15, 2), " (", round(IRI16, 2), ")", sep = "")))
outtable <- rbind(outtable, data.frame(var = paste("IRI-PD", " & ", round(IRI17, 2), " (", round(IRI18, 2), ") & ", 
                                                   round(IRI19, 2), " (", round(IRI20, 2), ") & ",
                                                   round(IRI21, 2), " (", round(IRI22, 2), ") & ",
                                                   round(IRI23, 2), " (", round(IRI24, 2), ")", sep = "")))
outtable <- rbind(outtable, data.frame(var = paste("IRI-PT", " & ", round(IRI25, 2), " (", round(IRI26, 2), ") & ", 
                                                   round(IRI27, 2), " (", round(IRI28, 2), ") & ",
                                                   round(IRI29, 2), " (", round(IRI30, 2), ") & ",
                                                   round(IRI31, 2), " (", round(IRI32, 2), ")", sep = "")))
outtable <- rbind(outtable, data.frame(var = paste("STAI-T", " & ", round(STAI1, 1), " (", round(STAI2, 1), ") & ", 
                                                   round(STAI3, 1), " (", round(STAI4, 1), ") & ",
                                                   round(STAI5, 1), " (", round(STAI6, 1), ") & ",
                                                   round(STAI7, 1), " (", round(STAI8, 1), ")", sep = "")))
outtable <- rbind(outtable, data.frame(var = paste("TAS-20", " & ", round(TAS1, 1), " (", round(TAS2, 1), ") & ", 
                                                   round(TAS3, 1), " (", round(TAS4, 1), ") & ",
                                                   round(TAS5, 1), " (", round(TAS6, 1), ") & ",
                                                   round(TAS7, 1), " (", round(TAS8, 1), ")", sep = "")))
outtable <- rbind(outtable, data.frame(var = paste("PPI-R-SCI", " & ", round(PPI1, 1), " (", round(PPI2, 1), ") & ", 
                                                   round(PPI3, 1), " (", round(PPI4, 1), ") & ",
                                                   round(PPI5, 1), " (", round(PPI6, 1), ") & ",
                                                   round(PPI7, 1), " (", round(PPI8, 1), ")", sep = "")))
outtable <- rbind(outtable, data.frame(var = paste("PPI-R-FD", " & ", round(PPI9, 1), " (", round(PPI10, 1), ") & ", 
                                                   round(PPI11, 1), " (", round(PPI12, 1), ") & ",
                                                   round(PPI13, 1), " (", round(PPI14, 1), ") & ",
                                                   round(PPI15, 1), " (", round(PPI16, 1), ")", sep = "")))
outtable <- rbind(outtable, data.frame(var = paste("PPI-R-C", " & ", round(PPI17, 1), " (", round(PPI18, 1), ") & ", 
                                                   round(PPI19, 1), " (", round(PPI20, 1), ") & ",
                                                   round(PPI21, 1), " (", round(PPI22, 1), ") & ",
                                                   round(PPI23, 1), " (", round(PPI24, 1), ")", sep = "")))
outtable

# IRI, test-retest
demData$IRIdiff <- demData$IRI_retest_EC - demData$IRI_EC
mean(demData$IRIdiff[demData$Included_EP == TRUE], na.rm = T)
sd(demData$IRIdiff[demData$Included_EP == TRUE], na.rm = T)

demData$IRIdiff2 <- demData$IRI_scrambled_EC - demData$IRI_EC
t.test(IRIdiff2 ~ Treatment, data = demData[demData$Included_EP == TRUE, ])

# Analyse effect of oxazepam on rated state anxiety
# Make dataframe for mixed-effects model
STAISData <- rbind(demData[, c("Subject", "Treatment", "Wave", "Included_EP", "Included_FMOV", "STAI.S", "STAI.S.Scrambled")], demData[, c("Subject", "Treatment", "Wave", "Included_EP", "Included_FMOV", "STAI.S", "STAI.S.Scrambled")]) 
STAISData <- STAISData[STAISData$Included_EP == T | STAISData$Included_FMOV == T, ] # Remove participants not included in this experiment
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

pdf("Fig_STAIS.pdf", width = 4, height = 4)
plot(eff1$fit[c(2, 4)],
     frame.plot = F,
     xaxt = "n",
     yaxt = "n",
     type = "b",
     xlab = "",
     ylab = "STAI-S score",
     xlim = c(1, 2.1),
     ylim = c(32, 38),
     col = col1,
     main = "B. State anxiety")
lines(c(1.1,2.1), eff1$fit[c(1, 3)], type = "b", col = col2, pch = 16)
lines(c(1, 1), c((eff1$upper[2]), (eff1$lower[2])), col = col1)
lines(c(2, 2), c((eff1$upper[4]), (eff1$lower[4])), col = col1)
lines(c(1.1, 1.1), c((eff1$upper[1]), (eff1$lower[1])), col = col2)
lines(c(2.1, 2.1), c((eff1$upper[3]), (eff1$lower[3])), col = col2)
axis(1, labels = c("Before", "After"), at = c(1.05, 2.05))
axis(2, at = c(32, 34, 36, 38))
#legend("top", col = c("blue", "red"), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), bty = "n")
dev.off()


# Analyse effect of oxazepam on pain thresholds
# Make dataframe for mixed-effects model
VASData <- rbind(demData[, c("Subject", "Treatment", "Wave", "Included_EP", "VAS80_before", "VAS80_after")], demData[, c("Subject", "Treatment", "Wave", "Included_EP", "VAS80_before", "VAS80_after")]) 
VASData <- VASData[VASData$Included_EP == T, ] # Remove participants not included in this experiment
VASData$FirstOrSecond <- c(rep.int(1, 0.5*length(VASData$Subject)), rep.int(2, 0.5*length(VASData$Subject)))
VASData$VAS80 <- NA # Make new column for STAI-S rating, then fill it with values for the first and second ratings, respectively
VASData$VAS80[VASData$FirstOrSecond == 1] <- VASData$VAS80_before[VASData$FirstOrSecond == 1]
VASData$VAS80[VASData$FirstOrSecond == 2] <- VASData$VAS80_after[VASData$FirstOrSecond == 2]

lme2 <- lme(VAS80 ~ Treatment * FirstOrSecond + Wave, data = VASData, random = ~1|Subject, na.action = na.omit)
summary(lme2)

# Inspect residuals
plot(lme2)

# Get estimates
intervals(lme2)

# Plot effects
eff2 <- effect("Treatment * FirstOrSecond", lme2)

pdf("Fig_VAS.pdf", width = 4, height = 4)
plot(eff2$fit[c(2, 4)],
     frame.plot = F,
     xaxt = "n",
     type = "b",
     xlab = "",
     ylab = "Volts required for VAS 80",
     xlim = c(1, 2.1),
     ylim = c(65, 85),
     col = col1,
     main = "C. Pain thresholds")
lines(c(1.1,2.1), eff2$fit[c(1, 3)], type = "b", col = col2, pch = 16)
lines(c(1, 1), c((eff2$upper[2]), (eff2$lower[2])), col = col1)
lines(c(2, 2), c((eff2$upper[4]), (eff2$lower[4])), col = col1)
lines(c(1.1, 1.1), c((eff2$upper[1]), (eff2$lower[1])), col = col2)
lines(c(2.1, 2.1), c((eff2$upper[3]), (eff2$lower[3])), col = col2)
axis(1, labels = c("Before", "After"), at = c(1.05, 2.05))
#legend("top", col = c("blue", "red"), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), bty = "n")
dev.off()


# Analyse participants' guesses of treatment group membership
demData$Guessed.group <- factor(demData$Guessed.group, levels = c("Placebo", "Likely_placebo", "Equivocal", "Likely_oxa", "Oxazepam"), ordered = TRUE)
demData$Guessed.group[demData$Included_EP == 0 & demData$Included_FMOV == 0] <- NA

pdf("Fig_Blinding.pdf", width = 4, height = 4)
barplot(t(matrix(c(table(demData$Guessed.group[demData$Treatment == "Placebo"]), table(demData$Guessed.group[demData$Treatment == "Oxazepam"])), nr = 5)), 
        beside = TRUE, 
        names.arg = c("Placebo", "", "Equivocal", "", "Oxazepam"), 
        xlab = "Guessed group",
        ylab = "n",
        yaxt = "n",
        col = c(col3, col2),
        border = c(col1, col2),
        lwd = 5,
        main = "D. Efficacy of blinding")
axis(2, at = c(0, 2, 4, 6))
legend(c(0, 6.4), legend = c("Placebo", "Oxazepam"), fill = c(col3, col2), border = c(col1, col2), bty = "n")
dev.off()

test1 <- wilcox.test(as.numeric(Guessed.group) ~ Treatment, data = demData, alternative = "greater", paired = F, conf.int = T)
test1
