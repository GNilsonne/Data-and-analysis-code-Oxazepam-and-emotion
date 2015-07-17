# Script to analyse EMG in the EP experiment of the Oxazepam project 2011
# Gustav Nilsonne 2015-05-21

# Require packages
require(RCurl) # To read data from GitHub
require(reshape2)
require(RColorBrewer)
require(nlme)
require(effects)
require(latticeExtra) # To make dotcharts

# Define colors for later
col1 = brewer.pal(8, "Dark2")[1]
col2 = brewer.pal(8, "Dark2")[2]
col3 = brewer.pal(8, "Dark2")[3]
col4 = brewer.pal(8, "Dark2")[4]
col5 = brewer.pal(8, "Dark2")[5]
col6 = brewer.pal(8, "Dark2")[6]
col7 = brewer.pal(8, "Dark2")[7]
col8 = brewer.pal(8, "Dark2")[8]

# Define function to read data
fun_readEMGdata <- function(X){
  
  # Read data file
  if(X == 9){
    X <- "09"
  }
  EMGDataURL <- getURL(paste("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/EP/", X, "_EP_preprocessed.txt", sep = ""), ssl.verifypeer = FALSE)
  data <- read.table(text = EMGDataURL, skip = 31, header = FALSE)
  if(length(data) == 12){
    names(data) <- c("SCR", "Raw_EMG_Zyg", "Raw_EMG_corr", "Rating", "EMG_corr", "ShockRef", "ShockPres", "SelfHigh", "SelfLow", "OtherHigh", "OtherLow", "NoShockPres")
  } else if(length(data) == 13){
    names(data) <- c("minutes", "SCR", "Raw_EMG_Zyg", "Raw_EMG_corr", "Rating", "EMG_corr", "ShockRef", "ShockPres", "SelfHigh", "SelfLow", "OtherHigh", "OtherLow", "NoShockPres")
  } else if(length(data) == 14){
    names(data) <- c("minutes", "SCR", "Raw_EMG_Zyg", "Raw_EMG_corr", "Rating", "EKG", "EMG_corr", "ShockRef", "ShockPres", "SelfHigh", "SelfLow", "OtherHigh", "OtherLow", "NoShockPres")
  } 
  
  # Find event onsets
  data$SelfHighDiff <- c(0, diff(data$SelfHigh, lag = 1))
  data$SelfHighOnsets <- data$SelfHighDiff == -5
  data$SelfLowDiff <- c(0, diff(data$SelfLow, lag = 1))
  data$SelfLowOnsets <- data$SelfLowDiff == -5
  data$OtherHighDiff <- c(0, diff(data$OtherHigh, lag = 1))
  data$OtherHighOnsets <- data$OtherHighDiff == -5
  data$OtherLowDiff <- c(0, diff(data$OtherLow, lag = 1))
  data$OtherLowOnsets <- data$OtherLowDiff == -5
  
  # Add time vector. Sampling was at 10 millisecond intervals.
  data$millisecond <- 10*(1:length(data$SCR))
  
  # Get EMG data
  
  # Define data for each event  
  SelfHighIndex <- which(data$SelfHighOnsets == TRUE)
  data$SelfHighWindow <- FALSE
  for(i in SelfHighIndex){
    if(length(data[, 1]) > (i + 600)){
      data$SelfHighWindow[(i - 200):(i + 600)] <- TRUE
    } else {
      this_length <- length(data[, 1]) - i
      data$SelfHighWindow[(i - 200):(i + this_length)] <- TRUE
    }
  }
  SelfLowIndex <- which(data$SelfLowOnsets == TRUE)
  data$SelfLowWindow <- FALSE
  for(i in SelfLowIndex){
      data$SelfLowWindow[(i - 200):(i + 600)] <- TRUE
  }
  OtherHighIndex <- which(data$OtherHighOnsets == TRUE)
  data$OtherHighWindow <- FALSE
  for(i in OtherHighIndex){
    data$OtherHighWindow[(i - 200):(i + 600)] <- TRUE
  }
  OtherLowIndex <- which(data$OtherLowOnsets == TRUE)
  data$OtherLowWindow <- FALSE
  for(i in OtherLowIndex){
    data$OtherLowWindow[(i - 200):(i + 600)] <- TRUE
  }
  
  # Extract data, downsample
  if(X != 83){ # Participant 83 had a few data points missing at the end
    SelfHighEMG <- matrix(data$EMG_corr[data$SelfHighWindow == TRUE], ncol = length(SelfHighIndex))
      } else if(X == 83){
    data_temp <- data$EMG_corr[data$SelfHighWindow == TRUE]
    data_temp[7028:7209] <- NA
    SelfHighEMG <- matrix(data_temp, ncol = 9)
  }
  SelfHighEMGlowess <- apply(SelfHighEMG, 2, lowess, f = 0.01)
  SelfHighEMGlowess <- matrix(unlist(SelfHighEMGlowess), byrow = F, ncol = length(SelfHighEMGlowess))[802:1602, ]
  
  plot(SelfHighEMG[, 1], type = "l", col = "gray", frame.plot = F, main = X)
  lines(SelfHighEMGlowess[, 1])
  lines(y = SelfHighEMGlowess[, 1][seq(1, NROW(SelfHighEMGlowess[, 1]), by=10)], x = c(1:801)[seq(1, NROW(SelfHighEMGlowess[, 1]), by=10)], col = "red", type = "o")
  
  SelfHighEMGdownsampled <- SelfHighEMG[seq(1, NROW(SelfHighEMGlowess), by=10), ]
  
  SelfLowEMG <- matrix(data$EMG_corr[data$SelfLowWindow == TRUE], ncol = length(SelfLowIndex))
  SelfLowEMGlowess <- apply(SelfLowEMG, 2, lowess, f = 0.01)
  SelfLowEMGlowess <- matrix(unlist(SelfLowEMGlowess), byrow = F, ncol = length(SelfLowEMGlowess))[802:1602, ]
  SelfLowEMGdownsampled <- SelfLowEMG[seq(1, NROW(SelfLowEMGlowess), by=10), ]
  
  OtherHighEMG <- matrix(data$EMG_corr[data$OtherHighWindow == TRUE], ncol = length(OtherHighIndex))
  OtherHighEMGlowess <- apply(OtherHighEMG, 2, lowess, f = 0.01)
  OtherHighEMGlowess <- matrix(unlist(OtherHighEMGlowess), byrow = F, ncol = length(OtherHighEMGlowess))[802:1602, ]
  OtherHighEMGdownsampled <- OtherHighEMG[seq(1, NROW(OtherHighEMGlowess), by=10), ]
  
  OtherLowEMG <- matrix(data$EMG_corr[data$OtherLowWindow == TRUE], ncol = length(OtherLowIndex))
  OtherLowEMGlowess <- apply(OtherLowEMG, 2, lowess, f = 0.01)
  OtherLowEMGlowess <- matrix(unlist(OtherLowEMGlowess), byrow = F, ncol = length(OtherLowEMGlowess))[802:1602, ]
  OtherLowEMGdownsampled <- OtherLowEMG[seq(1, NROW(OtherLowEMGlowess), by=10), ]

  # Make a list for each participant and return it as output
  EMGData <- list(as.integer(X), SelfHighEMGdownsampled, SelfLowEMGdownsampled, OtherHighEMGdownsampled, OtherLowEMGdownsampled)
  return(EMGData)
}

# Read files
# First demographic data etc
demDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/demographics.csv", ssl.verifypeer = FALSE)
demData <- read.csv(text = demDataURL)
# Then the Acqknowledge logfiles containing EMG data
IncludedSubjects <- demData$Subject[demData$Included_EP == T]
Excluded <- c(36, 70) # These participants can be included in the experiment but not on the EMG measure, because they had facial tics.
IncludedSubjects <- IncludedSubjects[!IncludedSubjects %in% Excluded]
EMGDataList <- lapply(IncludedSubjects, FUN = fun_readEMGdata)

# Move all data to one big frame
EMGData <- data.frame()

for(i in 1:length(EMGDataList)){
  if(!is.null(EMGDataList[i][[1]])){
    SelfHighData <- melt(EMGDataList[i][[1]][[2]], na.rm = F)
    names(SelfHighData) <- c("time_0.1s", "event_no", "EMG_corr")
    SelfHighData$Condition <- "Self"
    SelfHighData$Stimulus <- "High"
    
    OtherHighData <- melt(EMGDataList[i][[1]][[3]], na.rm = F)
    names(OtherHighData) <- c("time_0.1s", "event_no", "EMG_corr")
    OtherHighData$Condition <- "Other"
    OtherHighData$Stimulus <- "High"
    
    SelfLowData <- melt(EMGDataList[i][[1]][[4]], na.rm = F)
    names(SelfLowData) <- c("time_0.1s", "event_no", "EMG_corr")
    SelfLowData$Condition <- "Self"
    SelfLowData$Stimulus <- "Low"
    
    OtherLowData <- melt(EMGDataList[i][[1]][[5]], na.rm = F)
    names(OtherLowData) <- c("time_0.1s", "event_no", "EMG_corr")
    OtherLowData$Condition <- "Other"
    OtherLowData$Stimulus <- "Low"
    
    EMGData_temp <- rbind(SelfHighData, OtherHighData, SelfLowData, OtherLowData)
    EMGData_temp$subject <- IncludedSubjects[i]
  }
  EMGData <- rbind(EMGData, EMGData_temp)
}

# Check how many data points have been removed
num_na <- sum(is.na(EMGData$EMG_corr))
frac_na <- num_na/length(EMGData$EMG_corr)

# Convert time scale and set 0 to event onset
EMGData$time_s <- EMGData$time_0.1s/10
EMGData$time_s <- EMGData$time_s -2
  
# Make spaghetti plots
#plot(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Self" & Stimulus == "High"), frame.plot = F, type = 'n', main = "Self high", ylim = c(0, 0.04))
#for(i in unique(EMGData$subject)){
#  for( j in unique(EMGData$event_no[EMGData$subject == i])){
#    lines(EMG_corr ~ time_s, data = EMGData[EMGData$subject == i & EMGData$event_no == j & EMGData$Condition == "Self" & EMGData$Stimulus == "High", ], col = col4)
#  }
#}
  
#plot(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Self" & Stimulus == "Low"), frame.plot = F, type = 'n', main = "Self low", ylim = c(0, 0.04))
#for(i in unique(EMGData$subject)){
#  for( j in unique(EMGData$event_no[EMGData$subject == i])){
#    lines(EMG_corr ~ time_s, data = EMGData[EMGData$subject == i & EMGData$event_no == j & EMGData$Condition == "Self" & EMGData$Stimulus == "Low", ], col = col7)
#  }
#}

#plot(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Other" & Stimulus == "High"), frame.plot = F, type = 'n', main = "Other high", ylim = c(0, 0.04))
#for(i in unique(EMGData$subject)){
#  for( j in unique(EMGData$event_no[EMGData$subject == i])){
#    lines(EMG_corr ~ time_s, data = EMGData[EMGData$subject == i & EMGData$event_no == j & EMGData$Condition == "Other" & EMGData$Stimulus == "High", ], col = col4)
#  }
#}

#plot(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Other" & Stimulus == "Low"), frame.plot = F, type = 'n', main = "Other low", ylim = c(0, 0.04))
#for(i in unique(EMGData$subject)){
#  for( j in unique(EMGData$event_no[EMGData$subject == i])){
#    lines(EMG_corr ~ time_s, data = EMGData[EMGData$subject == i & EMGData$event_no == j & EMGData$Condition == "Other" & EMGData$Stimulus == "Low", ], col = col7)
#  }
#}

MeanEMGSelfHigh <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Self" & Stimulus == "High"), mean)
MeanEMGSelfLow <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Self" & Stimulus == "Low"), mean)
MeanEMGOtherHigh <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Other" & Stimulus == "High"), mean)
MeanEMGOtherLow <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Other" & Stimulus == "Low"), mean)

MeanEMGSelfHighWave1 <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Self" & Stimulus == "High" & subject %in% demData$Subject[demData$Wave == 1]), mean)
MeanEMGSelfLowWave1 <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Self" & Stimulus == "Low" & subject %in% demData$Subject[demData$Wave == 1]), mean)
MeanEMGOtherHighWave1 <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Other" & Stimulus == "High" & subject %in% demData$Subject[demData$Wave == 1]), mean)
MeanEMGOtherLowWave1 <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Other" & Stimulus == "Low" & subject %in% demData$Subject[demData$Wave == 1]), mean)

MeanEMGSelfHighWave2 <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Self" & Stimulus == "High" & subject %in% demData$Subject[demData$Wave == 2]), mean)
MeanEMGSelfLowWave2 <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Self" & Stimulus == "Low" & subject %in% demData$Subject[demData$Wave == 2]), mean)
MeanEMGOtherHighWave2 <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Other" & Stimulus == "High" & subject %in% demData$Subject[demData$Wave == 2]), mean)
MeanEMGOtherLowWave2 <- aggregate(EMG_corr ~ time_s, data = subset(EMGData, Condition == "Other" & Stimulus == "Low" & subject %in% demData$Subject[demData$Wave == 2]), mean)

plot(MeanEMGSelfHigh, type = "l", col = col4, frame.plot = F, main = "Corrugator EMG, self condition", ylim = c(0.0002, 0.0011))
lines(MeanEMGSelfLow, col = col7)
legend("topleft", lty = 1, col = c(col4, col7), legend = c("High", "Low"), bty = "n")
#abline(v = c(0, 0.5), lty = 2)

plot(MeanEMGSelfHighWave1, type = "l", col = col4, frame.plot = F, main = "Corrugator EMG, self condition, Wave 1", ylim = c(0.0002, 0.0011))
lines(MeanEMGSelfLowWave1, col = col7)
legend("topleft", lty = 1, col = c(col4, col7), legend = c("High", "Low"), bty = "n")
abline(v = c(0, 3, 4), lty = 2)

plot(MeanEMGOtherHighWave1, type = "l", col = col4, frame.plot = F, main = "Corrugator EMG, other condition, Wave 1", ylim = c(0.0002, 0.0011))
lines(MeanEMGOtherLowWave1, col = col7)
legend("topleft", lty = 1, col = c(col4, col7), legend = c("High", "Low"), bty = "n")
abline(v = c(0, 3, 4), lty = 2)

plot(MeanEMGSelfHighWave2, type = "l", col = col4, frame.plot = F, main = "Corrugator EMG, self condition, Wave 2", ylim = c(0.0002, 0.0011))
lines(MeanEMGSelfLowWave2, col = col7)
legend("topleft", lty = 1, col = c(col4, col7), legend = c("High", "Low"), bty = "n")
abline(v = c(0, 0.5), lty = 2)

plot(MeanEMGOtherHighWave2, type = "l", col = col4, frame.plot = F, main = "Corrugator EMG, other condition, Wave 2", ylim = c(0.0002, 0.0011))
lines(MeanEMGOtherLowWave2, col = col7)
legend("topleft", lty = 1, col = c(col4, col7), legend = c("High", "Low"), bty = "n")
abline(v = c(0, 0.5), lty = 2)

# Make figures
pdf("Fig_EMG1.pdf", width = 4, height = 4)
plot(MeanEMGSelfHighWave1, type = "n", col = col4, frame.plot = F, main = "A. Corrugator EMG, Self, Wave 1", ylim = c(0.0002, 0.0011), xlab = "Time (s)", ylab = "EMG (mV)", xaxt = "n", yaxt = "n")
rect(3, 0.000001, 5, 1, col = "gray88", border = NA)
abline(v = c(0, 3, 4), lty = 3)
axis(1)
axis(2, at = c(0.0002, 0.001))
lines(MeanEMGSelfHighWave1, col = col4, lwd = 2)
lines(MeanEMGSelfLowWave1, col = col7, lty = 5, lwd = 2)
legend("topleft", lty = c(1, 5), lwd = 2, col = c(col4, col7), legend = c("High shock", "Low shock"), bg = "white", cex = 0.8)
dev.off()

pdf("Fig_EMG2.pdf", width = 4, height = 4)
plot(MeanEMGOtherHighWave1, type = "n", col = col4, frame.plot = F, main = "B. Corrugator EMG, Other, Wave 1", ylim = c(0.0002, 0.0011), xlab = "Time (s)", ylab = "EMG (mV)", xaxt = "n", yaxt = "n")
rect(3, 0.000001, 5, 1, col = "gray88", border = NA)
abline(v = c(0, 3, 4), lty = 3)
axis(1)
axis(2, at = c(0.0002, 0.001))
lines(MeanEMGOtherHighWave1, col = col4, lwd = 2)
lines(MeanEMGOtherLowWave1, col = col7, lty = 5, lwd = 2)
dev.off()

pdf("Fig_EMG3.pdf", width = 4, height = 4)
plot(MeanEMGSelfHighWave2, type = "n", col = col4, frame.plot = F, main = "C. Corrugator EMG, Self, Wave 2", ylim = c(0.0002, 0.0011), xlab = "Time (s)", ylab = "EMG (mV)", xaxt = "n", yaxt = "n")
rect(0, 0.000001, 2, 1, col = "gray88", border = NA)
abline(v = c(0, 0.5), lty = 3)
axis(1)
axis(2, at = c(0.0002, 0.001))
lines(MeanEMGSelfHighWave2, col = col4, lwd = 2)
lines(MeanEMGSelfLowWave2, col = col7, lty = 5, lwd = 2)
dev.off()

pdf("Fig_EMG4.pdf", width = 4, height = 4)
plot(MeanEMGOtherHighWave2, type = "n", col = col4, frame.plot = F, main = "D. Corrugator EMG, Other, Wave 2", ylim = c(0.0002, 0.0011), xlab = "Time (s)", ylab = "EMG (mV)", xaxt = "n", yaxt = "n")
rect(0, 0.000001, 2, 1, col = "gray88", border = NA)
abline(v = c(0, 0.5), lty = 3)
axis(1)
axis(2, at = c(0.0002, 0.001))
lines(MeanEMGOtherHighWave2, col = col4, lwd = 2)
lines(MeanEMGOtherLowWave2, col = col7, lty = 5, lwd = 2)
dev.off()


# Since data have varying baselines between conditions, we index each response to its baseline from -2 to 0 seconds
#EMGData$hr_index <- NA
#for(i in unique(EMGData$subject)){
#  for(j in unique(EMGData$event_no[EMGData$subject == i])){
#    for(k in c("Self", "Other")){
#      for(l in c("High", "Low")){
#        EMGData$hr_index[EMGData$subject == i & EMGData$event_no == j & EMGData$Condition == k & EMGData$Stimulus == l] <- EMGData$hr_bpm[EMGData$subject == i & EMGData$event_no == j & EMGData$Condition == k & EMGData$Stimulus == l]/mean(EMGData$hr_bpm[EMGData$subject == i & EMGData$event_no == j & EMGData$Condition == k & EMGData$Stimulus == l][1:20])
#      }
#    }
#  }
#}

#MeanHRIndexSelfHigh <- aggregate(hr_index ~ time_s, data = subset(EMGData, Condition == "Self" & Stimulus == "High"), mean)
#MeanHRIndexSelfLow <- aggregate(hr_index ~ time_s, data = subset(EMGData, Condition == "Self" & Stimulus == "Low"), mean)
#MeanHRIndexOtherHigh <- aggregate(hr_index ~ time_s, data = subset(EMGData, Condition == "Other" & Stimulus == "High"), mean)
#MeanHRIndexOtherLow <- aggregate(hr_index ~ time_s, data = subset(EMGData, Condition == "Other" & Stimulus == "Low"), mean)

#pdf("Fig_HR1.pdf", width = 4, height = 4)
#plot(MeanHRIndexSelfHigh, type = "n", col = col4, frame.plot = F, main = "I. Heart rate, Self", ylim = c(0.98, 1.16), xlab = "Time, s", ylab = "Heart rate, normalised ratio", xaxt = "n")
#abline(v = c(0, 0.5), lty = 3)
#rect(2.5, 0, 5, 1.16, col = "gray88", border = NA)
#axis(1)
#lines(MeanHRIndexSelfHigh, col = col4, lwd = 2)
#lines(MeanHRIndexSelfLow, col = col7, lty = 5, lwd = 2)
#legend("topright", lty = c(1, 5), lwd = 2, col = c(col4, col7), legend = c("High", "Low"), bg = "white")
#dev.off()

#pdf("Fig_HR2.pdf", width = 4, height = 4)
#plot(MeanHRIndexOtherHigh, type = "n", col = col4, frame.plot = F, main = "J. Heart rate, Other", ylim = c(0.98, 1.16), xlab = "Time, s", ylab = "Heart rate, normalised ratio")
#abline(v = c(0, 0.5), lty = 3)
#rect(2.5, 0, 5, 1.16, col = "gray88", border = NA)
#lines(MeanHRIndexOtherHigh, col = col4, lwd = 2)
#lines(MeanHRIndexOtherLow, col = col7, lty = 5, lwd = 2)
#dev.off()

# Average data over interval for each event for statistical modelling
EMGEventData <- data.frame()
for(i in unique(EMGData$subject)){
  for(j in unique(EMGData$event_no[EMGData$subject == i])){
    for(k in c("Self", "Other")){
      for(l in c("High", "Low")){
        if(i %in% demData$Subject[demData$Wave == 1]){
          EMG_corr_mean <- mean(EMGData$EMG_corr[EMGData$subject == i & EMGData$event_no == j & EMGData$Condition == k & EMGData$Stimulus == l][50:70])
          EMGEventData <- rbind(EMGEventData, data.frame(i, j, k, l, EMG_corr_mean))
        } else if(i %in% demData$Subject[demData$Wave == 2]){
          EMG_corr_mean <- mean(EMGData$EMG_corr[EMGData$subject == i & EMGData$event_no == j & EMGData$Condition == k & EMGData$Stimulus == l][20:40])
          EMGEventData <- rbind(EMGEventData, data.frame(i, j, k, l, EMG_corr_mean))
        }
      }
    }
  }
}
names(EMGEventData) <- c("Subject", "event_no", "Condition", "Stimulus", "EMG_corr_mean")

EMGEventData$EMG_corr_mean <- log(EMGEventData$EMG_corr_mean)

# Analyse data
# Include IRI-EC terms in model since we wish to control for baseline empathic propensity 
EMGEventData <- merge(EMGEventData, demData, by = "Subject")
EMGEventData$Subject <- as.factor(EMGEventData$Subject)
EMGEventData$Treatment <- relevel(EMGEventData$Treatment, ref = "Placebo")
EMGEventData$Stimulus <- relevel(EMGEventData$Stimulus, ref = "Low")
EMGEventData$Condition <- relevel(EMGEventData$Condition, ref = "Self")

# Make a new column to specify an "Other High" contrast in modelling
EMGEventData$OtherHigh <- as.numeric(EMGEventData$Condition)*as.numeric(EMGEventData$Stimulus)
EMGEventData$OtherHigh[EMGEventData$OtherHigh != 4] <- 0
EMGEventData$OtherHigh[EMGEventData$OtherHigh == 4] <- 1

# z-transform IRI-EC
EMGEventData$IRI_EC_z <- scale(EMGEventData$IRI_EC)

# Make a regressor for predictor in other high condition only
EMGEventData$IRI_EC_z_OtherHigh <- EMGEventData$IRI_EC_z * EMGEventData$OtherHigh

# Mean center the new regressor
EMGEventData$IRI_EC_z_OtherHigh[EMGEventData$IRI_EC_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_EC_z_OtherHigh)] <- EMGEventData$IRI_EC_z_OtherHigh[EMGEventData$IRI_EC_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_EC_z_OtherHigh)] - mean(EMGEventData$IRI_EC_z_OtherHigh[EMGEventData$IRI_EC_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_EC_z_OtherHigh)], na.rm = TRUE)

# Build model
lme1 <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh + Wave, data = EMGEventData, random = ~1|Subject, na.action = na.omit)

plot(lme1)
summary(lme1)
intervals(lme1)

eff1 <- effect("Treatment*Stimulus*Condition", lme1)

pdf("Fig_EMG5.pdf", width = 4, height = 4)
plot(c(eff1$fit[1], eff1$fit[3]),
     type = "n",
     frame.plot = F,
     ylab = "log mean EMG",
     xlab = "Shock intensity",
     xaxt = "n",
     yaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(-8.8, -7.7),
     col = col1,
     main = "E. Corrugator EMG, Self"
)
lines(c(1, 2), c(eff1$fit[2], eff1$fit[4]), type = "b", col = col2, pch = 16)
lines(c(1.1, 2.1), c(eff1$fit[1], eff1$fit[3]), type = "b", col = col1)
lines(c(1.1, 1.1), c(eff1$upper[1], eff1$lower[1]), col = col1)
lines(c(2.1, 2.1), c(eff1$upper[3], eff1$lower[3]), col = col1)
lines(c(1, 1), c(eff1$upper[2], eff1$lower[2]), col = col2)
lines(c(2, 2), c(eff1$upper[4], eff1$lower[4]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("Low", "High"))
axis(2, at = c(-8.7, -8.2, -7.7))
legend("topleft", col = c(col1, col2), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), lty = 1, bty = "n")
dev.off()

pdf("Fig_EMG6.pdf", width = 4, height = 4)
plot(c(eff1$fit[5], eff1$fit[7]),
     type = "n",
     frame.plot = F,
     ylab = "log mean EMG",
     xlab = "Shock intensity",
     xaxt = "n",
     yaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(-8.8, -7.7),
     col = col1,
     main = "F. Corrugator EMG, Other"
)
lines(c(1, 2), c(eff1$fit[6], eff1$fit[8]), type = "b", col = col2, pch = 16)
lines(c(1.1, 2.1), c(eff1$fit[5], eff1$fit[7]), type = "b", col = col1)
lines(c(1.1, 1.1), c(eff1$upper[5], eff1$lower[5]), col = col1)
lines(c(2.1, 2.1), c(eff1$upper[7], eff1$lower[7]), col = col1)
lines(c(1, 1), c(eff1$upper[6], eff1$lower[6]), col = col2)
lines(c(2, 2), c(eff1$upper[8], eff1$lower[8]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("Low", "High"))
axis(2, at = c(-8.7, -8.2, -7.7))
dev.off()

# Compare plots to less custom-generated output for verification
plot(effect("Treatment*Stimulus*Condition", lme1))
plot(effect("Stimulus*Condition", lme1))
plot(effect("IRI_EC_z_OtherHigh", lme1))

# Analyse other rating scales as predictors
# Since rating scales may be collinear, we include them one by one
# The procedure follows the same logic as above

# IRI-PT
EMGEventData$IRI_PT_z <- scale(EMGEventData$IRI_PT)
EMGEventData$IRI_PT_z_OtherHigh <- EMGEventData$IRI_PT_z * EMGEventData$OtherHigh
EMGEventData$IRI_PT_z_OtherHigh[EMGEventData$IRI_PT_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_PT_z_OtherHigh)] <- EMGEventData$IRI_PT_z_OtherHigh[EMGEventData$IRI_PT_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_PT_z_OtherHigh)] - mean(EMGEventData$IRI_PT_z_OtherHigh[EMGEventData$IRI_PT_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_PT_z_OtherHigh)], na.rm = TRUE)
lme2 <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + IRI_PT_z + IRI_PT_z_OtherHigh + Wave, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
plot(lme2)
summary(lme2)
intervals(lme2)

# IRI-PD
EMGEventData$IRI_PD_z <- scale(EMGEventData$IRI_PD)
EMGEventData$IRI_PD_z_OtherHigh <- EMGEventData$IRI_PD_z * EMGEventData$OtherHigh
EMGEventData$IRI_PD_z_OtherHigh[EMGEventData$IRI_PD_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_PD_z_OtherHigh)] <- EMGEventData$IRI_PD_z_OtherHigh[EMGEventData$IRI_PD_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_PD_z_OtherHigh)] - mean(EMGEventData$IRI_PD_z_OtherHigh[EMGEventData$IRI_PD_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_PD_z_OtherHigh)], na.rm = TRUE)
lme3 <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + IRI_PD_z + IRI_PD_z_OtherHigh + Wave, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
plot(lme3)
summary(lme3)
intervals(lme3)

# IRI-F
EMGEventData$IRI_F_z <- scale(EMGEventData$IRI_F)
EMGEventData$IRI_F_z_OtherHigh <- EMGEventData$IRI_F_z * EMGEventData$OtherHigh
EMGEventData$IRI_F_z_OtherHigh[EMGEventData$IRI_F_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_F_z_OtherHigh)] <- EMGEventData$IRI_F_z_OtherHigh[EMGEventData$IRI_F_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_F_z_OtherHigh)] - mean(EMGEventData$IRI_F_z_OtherHigh[EMGEventData$IRI_F_z_OtherHigh > 0 & !is.na(EMGEventData$IRI_F_z_OtherHigh)], na.rm = TRUE)
lme4 <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + IRI_F_z + IRI_F_z_OtherHigh + Wave, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
plot(lme4)
summary(lme4)
intervals(lme4)

# STAI-T
EMGEventData$STAI.T_z <- scale(EMGEventData$STAI.T)
EMGEventData$STAI.T_z_OtherHigh <- EMGEventData$STAI.T_z * EMGEventData$OtherHigh
EMGEventData$STAI.T_z_OtherHigh[EMGEventData$STAI.T_z_OtherHigh > 0 & !is.na(EMGEventData$STAI.T_z_OtherHigh)] <- EMGEventData$STAI.T_z_OtherHigh[EMGEventData$STAI.T_z_OtherHigh > 0 & !is.na(EMGEventData$STAI.T_z_OtherHigh)] - mean(EMGEventData$STAI.T_z_OtherHigh[EMGEventData$STAI.T_z_OtherHigh > 0 & !is.na(EMGEventData$STAI.T_z_OtherHigh)], na.rm = TRUE)
lme5 <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + STAI.T_z + STAI.T_z_OtherHigh + Wave, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
plot(lme5)
summary(lme5)
intervals(lme5)

# TAS-20
EMGEventData$TAS.20_z <- scale(EMGEventData$TAS.20)
EMGEventData$TAS.20_z_OtherHigh <- EMGEventData$TAS.20_z * EMGEventData$OtherHigh
EMGEventData$TAS.20_z_OtherHigh[EMGEventData$TAS.20_z_OtherHigh > 0 & !is.na(EMGEventData$TAS.20_z_OtherHigh)] <- EMGEventData$TAS.20_z_OtherHigh[EMGEventData$TAS.20_z_OtherHigh > 0 & !is.na(EMGEventData$TAS.20_z_OtherHigh)] - mean(EMGEventData$TAS.20_z_OtherHigh[EMGEventData$TAS.20_z_OtherHigh > 0 & !is.na(EMGEventData$TAS.20_z_OtherHigh)], na.rm = TRUE)
lme6 <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + TAS.20_z + TAS.20_z_OtherHigh + Wave, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
plot(lme6)
summary(lme6)
intervals(lme6)

# PPI-R-SCI
EMGEventData$PPI_SCI_z <- EMGEventData$PPI_1_SCI_R
EMGEventData$PPI_SCI_z[EMGEventData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
EMGEventData$PPI_SCI_z <- scale(EMGEventData$PPI_SCI_z)
EMGEventData$PPI_SCI_z_OtherHigh <- EMGEventData$PPI_SCI_z * EMGEventData$OtherHigh
EMGEventData$PPI_SCI_z_OtherHigh[EMGEventData$PPI_SCI_z_OtherHigh > 0 & !is.na(EMGEventData$PPI_SCI_z_OtherHigh)] <- EMGEventData$PPI_SCI_z_OtherHigh[EMGEventData$PPI_SCI_z_OtherHigh > 0 & !is.na(EMGEventData$PPI_SCI_z_OtherHigh)] - mean(EMGEventData$PPI_SCI_z_OtherHigh[EMGEventData$PPI_SCI_z_OtherHigh > 0 & !is.na(EMGEventData$PPI_SCI_z_OtherHigh)], na.rm = TRUE)
lme7 <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + PPI_SCI_z + PPI_SCI_z_OtherHigh + Wave, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
plot(lme7)
summary(lme7)
intervals(lme7)

# PPI-R-FD
EMGEventData$PPI_FD_z <- EMGEventData$PPI_1_FD_R
EMGEventData$PPI_FD_z[EMGEventData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
EMGEventData$PPI_FD_z <- scale(EMGEventData$PPI_FD_z)
EMGEventData$PPI_FD_z_OtherHigh <- EMGEventData$PPI_FD_z * EMGEventData$OtherHigh
EMGEventData$PPI_FD_z_OtherHigh[EMGEventData$PPI_FD_z_OtherHigh > 0 & !is.na(EMGEventData$PPI_FD_z_OtherHigh)] <- EMGEventData$PPI_FD_z_OtherHigh[EMGEventData$PPI_FD_z_OtherHigh > 0 & !is.na(EMGEventData$PPI_FD_z_OtherHigh)] - mean(EMGEventData$PPI_FD_z_OtherHigh[EMGEventData$PPI_FD_z_OtherHigh > 0 & !is.na(EMGEventData$PPI_FD_z_OtherHigh)], na.rm = TRUE)
lme8 <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + PPI_FD_z + PPI_FD_z_OtherHigh + Wave, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
plot(lme8)
summary(lme8)
intervals(lme8)

# PPI-R-C
EMGEventData$PPI_C_z <- EMGEventData$PPI_1_C_R
EMGEventData$PPI_C_z[EMGEventData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
EMGEventData$PPI_C_z <- scale(EMGEventData$PPI_C_z)
EMGEventData$PPI_C_z_OtherHigh <- EMGEventData$PPI_C_z * EMGEventData$OtherHigh
EMGEventData$PPI_C_z_OtherHigh[EMGEventData$PPI_C_z_OtherHigh > 0 & !is.na(EMGEventData$PPI_C_z_OtherHigh)] <- EMGEventData$PPI_C_z_OtherHigh[EMGEventData$PPI_C_z_OtherHigh > 0 & !is.na(EMGEventData$PPI_C_z_OtherHigh)] - mean(EMGEventData$PPI_C_z_OtherHigh[EMGEventData$PPI_C_z_OtherHigh > 0 & !is.na(EMGEventData$PPI_C_z_OtherHigh)], na.rm = TRUE)
lme9 <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + PPI_C_z + PPI_C_z_OtherHigh + Wave, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
plot(lme9)
summary(lme9)
intervals(lme9)

# Make plots to compare effects for different scales
# Put data in new frame
data_main <- data.frame(scale = "PPI-R-C", beta = intervals(lme9)$fixed[5, 2], lower = intervals(lme9)$fixed[5, 1], upper = intervals(lme9)$fixed[5, 3], group = "PPI", p = round(summary(lme9)$tTable[5, 5], 3))
data_main <- rbind(data_main, data.frame(scale = "PPI-R-FD", beta = intervals(lme8)$fixed[5, 2], lower = intervals(lme8)$fixed[5, 1], upper = intervals(lme8)$fixed[5, 3], group = "PPI", p = round(summary(lme8)$tTable[5, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "PPI-R-SCI", beta = intervals(lme7)$fixed[5, 2], lower = intervals(lme7)$fixed[5, 1], upper = intervals(lme7)$fixed[5, 3], group = "PPI", p = round(summary(lme7)$tTable[5, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "TAS-20", beta = intervals(lme6)$fixed[5, 2], lower = intervals(lme6)$fixed[5, 1], upper = intervals(lme6)$fixed[5, 3], group = "TAS", p = round(summary(lme6)$tTable[5, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "STAI-T", beta = intervals(lme5)$fixed[5, 2], lower = intervals(lme5)$fixed[5, 1], upper = intervals(lme5)$fixed[5, 3], group = "STAI", p = round(summary(lme5)$tTable[5, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "IRI-F", beta = intervals(lme4)$fixed[5, 2], lower = intervals(lme4)$fixed[5, 1], upper = intervals(lme4)$fixed[5, 3], group = "IRI", p = round(summary(lme4)$tTable[5, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "IRI-PD", beta = intervals(lme3)$fixed[5, 2], lower = intervals(lme3)$fixed[5, 1], upper = intervals(lme3)$fixed[5, 3], group = "IRI", p = round(summary(lme3)$tTable[5, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "IRI-PT", beta = intervals(lme2)$fixed[5, 2], lower = intervals(lme2)$fixed[5, 1], upper = intervals(lme2)$fixed[5, 3], group = "IRI", p = round(summary(lme2)$tTable[5, 5], 3)))
data_main <- rbind(data_main, data.frame(scale = "IRI-EC", beta = intervals(lme1)$fixed[5, 2], lower = intervals(lme1)$fixed[5, 1], upper = intervals(lme1)$fixed[5, 3], group = "IRI", p = round(summary(lme1)$tTable[5, 5], 3)))

# Make plot
#pdf("Fig_EMG7.pdf", width = 4, height = 4)
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
#        main = "D. Corrugator EMG",
#        panel = function (x,y,z,...){
#          panel.segplot(x,y,z,...)
#          panel.abline(v=0,lty=2)
#        })
#dev.off()

# Put data in new frame
data_emp <- data.frame(scale = "PPI-R-C", beta = intervals(lme9)$fixed[6, 2], lower = intervals(lme9)$fixed[6, 1], upper = intervals(lme9)$fixed[6, 3], group = "PPI", p = round(summary(lme9)$tTable[6, 5], 3))
data_emp <- rbind(data_emp, data.frame(scale = "PPI-R-FD", beta = intervals(lme8)$fixed[6, 2], lower = intervals(lme8)$fixed[6, 1], upper = intervals(lme8)$fixed[6, 3], group = "PPI", p = round(summary(lme8)$tTable[6, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "PPI-R-SCI", beta = intervals(lme7)$fixed[6, 2], lower = intervals(lme7)$fixed[6, 1], upper = intervals(lme7)$fixed[6, 3], group = "PPI", p = round(summary(lme7)$tTable[6, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "TAS-20", beta = intervals(lme6)$fixed[6, 2], lower = intervals(lme6)$fixed[6, 1], upper = intervals(lme6)$fixed[6, 3], group = "TAS", p = round(summary(lme6)$tTable[6, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "STAI-T", beta = intervals(lme5)$fixed[6, 2], lower = intervals(lme5)$fixed[6, 1], upper = intervals(lme5)$fixed[6, 3], group = "STAI", p = round(summary(lme5)$tTable[6, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "IRI-F", beta = intervals(lme4)$fixed[6, 2], lower = intervals(lme4)$fixed[6, 1], upper = intervals(lme4)$fixed[6, 3], group = "IRI", p = round(summary(lme4)$tTable[6, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "IRI-PD", beta = intervals(lme3)$fixed[6, 2], lower = intervals(lme3)$fixed[6, 1], upper = intervals(lme3)$fixed[6, 3], group = "IRI", p = round(summary(lme3)$tTable[6, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "IRI-PT", beta = intervals(lme2)$fixed[6, 2], lower = intervals(lme2)$fixed[6, 1], upper = intervals(lme2)$fixed[6, 3], group = "IRI", p = round(summary(lme2)$tTable[6, 5], 3)))
data_emp <- rbind(data_emp, data.frame(scale = "IRI-EC", beta = intervals(lme1)$fixed[6, 2], lower = intervals(lme1)$fixed[6, 1], upper = intervals(lme1)$fixed[6, 3], group = "IRI", p = round(summary(lme1)$tTable[6, 5], 3)))

# Make plot
#pdf("Fig_EMG8.pdf", width = 4, height = 4)
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
#segplot(scale ~ lower + upper, data = data_emp, 
#        centers = beta, 
#        lwd = 2,
#        draw.bands = FALSE,
#        col = c(col8, col8, col8, col3, col6, col5, col5, col5, col5),
#        par.settings = sty,
#        axis=axis.L,
#        xlab = "Beta, 95% CI",
#        main = "D. Corrugator EMG",
#        panel = function (x,y,z,...){
#          panel.segplot(x,y,z,...)
#          panel.abline(v=0,lty=2)
#        })
#dev.off()


# Make new plots including tables of effects
data_main2 <- data_main[4:9, ]
data_main2 <- data_main2[rev(rownames(data_main2)),]

pdf("Fig_EMG10.pdf", width = 6, height = 3.2)
par(mar=c(5.1, 5, 4, 14))
plot(x = data_main2$beta, y = c(8:5, 3, 1), xlab = expression(beta), ylab = "", frame.plot = F, xlim = c(min(data_main2$lower), max(data_main2$upper)), xaxt = "n", yaxt = "n")
title("D. Corrugator EMG", line = 2)
abline(v = 0, col = "gray")
axis(1, at = c(-0.2, 0, 0.2), labels = c(-0.2, 0, 0.2))
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
  
pdf("Fig_EMG11.pdf", width = 6, height = 3.2)
par(mar=c(5.1, 5, 4, 14))
plot(x = data_emp2$beta, y = c(8:5, 3, 1), xlab = expression(beta), ylab = "", frame.plot = F, xlim = c(min(data_emp2$lower), max(data_emp2$upper)), xaxt = "n", yaxt = "n")
title("D. Corrugator EMG", line = 2)
abline(v = 0, col = "gray")
axis(1, at = c(-0.1, 0, 0.1), labels = c(-0.1, 0, 0.1))
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
summary(lme1)
write.csv(summary(lme1)$tTable, file = "Result_tables/EMG_Waves1and2.csv")

lmew1 <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh, data = EMGEventData[EMGEventData$Wave == 1, ], random = ~1|Subject, na.action = na.omit)
plot(lmew1)
summary(lmew1)
intervals(lmew1)
write.csv(summary(lmew1)$tTable, file = "Result_tables/EMG_Wave1.csv")

lmew2 <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh, data = EMGEventData[EMGEventData$Wave == 2, ], random = ~1|Subject, na.action = na.omit)
plot(lmew2)
summary(lmew2)
intervals(lmew2)
write.csv(summary(lmew2)$tTable, file = "Result_tables/EMG_Wave2.csv")

# Analyse effect of rated likability of confederate (wave 2 only)
EMGEventData$RatedSympathy<- scale(EMGEventData$RatedSympathy)
EMGEventData$RatedSympathy_OtherHigh <- EMGEventData$RatedSympathy * EMGEventData$OtherHigh
EMGEventData$RatedSympathy_OtherHigh[EMGEventData$RatedSympathy_OtherHigh > 0 & !is.na(EMGEventData$RatedSympathy_OtherHigh)] <- EMGEventData$RatedSympathy_OtherHigh[EMGEventData$RatedSympathy_OtherHigh > 0 & !is.na(EMGEventData$RatedSympathy_OtherHigh)] - mean(EMGEventData$RatedSympathy_OtherHigh[EMGEventData$RatedSympathy_OtherHigh > 0 & !is.na(EMGEventData$RatedSympathy_OtherHigh)], na.rm = TRUE)

lmew2b <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh + RatedSympathy + RatedSympathy_OtherHigh, data = EMGEventData[EMGEventData$Wave == 2, ], random = ~1|Subject, na.action = na.omit)
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
lme1b <- lme(EMG_corr_mean ~ Treatment*Stimulus*Condition + Wave, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
coef <- coef(lme1b)
coef$Subject <- row.names(coef)
coef <- merge(coef, demData[, c("Subject", "Treatment", "Wave")], by = "Subject")
coef$EMG_corr_mean <- coef$"(Intercept)" + coef$StimulusHigh + coef$ConditionOther + coef$"StimulusHigh:ConditionOther"
IndividualResponses <- coef[, c("Subject", "EMG_corr_mean")]
write.csv(IndividualResponses, file = "IndividualResponsesEMG.csv", row.names = FALSE)

# Calculate standardised regression coefficients for the PPI-R subscales
EMGEventData$EMG_corr_mean_z <- scale(EMGEventData$EMG_corr_mean)

# PPI-R-SCI
lme7b <- lme(EMG_corr_mean_z ~ Treatment*Stimulus*Condition + Wave + PPI_SCI_z + PPI_SCI_z_OtherHigh, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
plot(lme7b)
summary(lme7b)
intervals(lme7b)

# PPI-R-FD
lme8b <- lme(EMG_corr_mean_z ~ Treatment*Stimulus*Condition + Wave + PPI_FD_z + PPI_FD_z_OtherHigh, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
plot(lme8b)
summary(lme8b)
intervals(lme8b)

# PPI-R-C
lme9b <- lme(EMG_corr_mean_z ~ Treatment*Stimulus*Condition + Wave + PPI_C_z + PPI_C_z_OtherHigh, data = EMGEventData, random = ~1|Subject, na.action = na.omit)
plot(lme9b)
summary(lme9b)
intervals(lme9b)
