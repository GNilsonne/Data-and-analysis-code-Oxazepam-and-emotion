# Script to analyse heart rate in the EP experiment of the Oxazepam project 2011
# Gustav Nilsonne 2015-05-21

# Require packages
require(RCurl) # To read data from GitHub
require(quantmod) # To find peaks for heartbeats
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
fun_readEPdata <- function(X){
  
  # Read data file
  EPDataURL <- getURL(paste("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/EP/", X, "_EP_preprocessed.txt", sep = ""), ssl.verifypeer = FALSE)
  data <- read.table(text = EPDataURL, skip = 31, header = FALSE)
  if(length(data) == 13){
    names(data) <- c("minutes", "SCR", "Raw_EMG_Zyg", "Raw_EMG_corr", "Rating", "EMG_corr", "ShockRef", "ShockPres", "SelfHigh", "SelfLow", "OtherHigh", "OtherLow", "NoShockPres")
  } else {
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
  data$millisecond <- 10*(1:length(data$minute))
  
  # Determine heart rate
  if(length(data$EKG) > 0){
    HeartBeats <- findPeaks(data$EKG[data$EKG > 0.22], thresh = 0.0001) # This threshold seems to work all right, should be sanity checked
    HeartBeatTimes <- data$millisecond[data$EKG > 0.22][HeartBeats]
    HeartBeatIntervals <- (c(HeartBeatTimes, 0) - c(0, HeartBeatTimes))/1000 # Convert to seconds
    
    HeartBeatTimes <- HeartBeatTimes[HeartBeatIntervals > 0.25] # Remove heartbeats that were unrealisticaly close to the last one, they are artefacts of the signal or of the peak finding algorithm
    HeartBeatIntervals <- (c(HeartBeatTimes, 0) - c(0, HeartBeatTimes))/1000 # Convert to seconds
    
    HeartRate <- 60/HeartBeatIntervals
    #plot(HeartRate, ylim = c(0, 100))
    
    plot(EKG ~ millisecond, data = data[2000:6000, ], type = "l", main = X, frame.plot = F, col = "gray")
    points(HeartBeatTimes, rep(0.5, length(HeartBeatTimes)), col = "red", pch = "|")
    
    #plot(data$EKG[1:2000], type = "l")
    #points(HeartBeats, rep(1, length(HeartBeats)), col = "red", pch = "|")
    
    data$HeartRate <- NA
    index <- data$millisecond %in% HeartBeatTimes
    HeartBeatLocations <- which(index == T)
    for(i in 1:(length(HeartBeatLocations)-1)){
      data$HeartRate[HeartBeatLocations[i]:HeartBeatLocations[i+1]] <- HeartRate[i]
    }
    
    data$HeartRate[data$HeartRate < 40] <- NA # Heart rate below 40 is thought to arise from missing beats, set to NA 
    data$HeartRate[data$HeartRate > 200] <- NA
    #plot(data$HeartRate, type = "l")
    
    # Define data for each event  
    SelfHighIndex <- which(data$SelfHighOnsets == TRUE)
    data$SelfHighWindow <- FALSE
    for(i in SelfHighIndex){
      if(length(data$SelfHighWindow) > (SelfHighIndex[length(SelfHighIndex)] + 600)){
        data$SelfHighWindow[(i - 200):(i + 600)] <- TRUE
      } else {
        this_length <- length(data$SelfHighWindow) - SelfHighIndex[length(SelfHighIndex)]
        data$SelfHighWindow[(i - 200):(i + this_length)] <- TRUE
      }
    }
    SelfLowIndex <- which(data$SelfLowOnsets == TRUE)
    data$SelfLowWindow <- FALSE
    for(i in SelfLowIndex){
      if(length(data$SelfLowWindow) > (SelfLowIndex[length(SelfLowIndex)] + 600)){
        data$SelfLowWindow[(i - 200):(i + 600)] <- TRUE
      } else {
        this_length <- length(data$SelfLowWindow) - SelfLowIndex[length(SelfLowIndex)]
        data$SelfLowWindow[(i - 200):(i + this_length)] <- TRUE
      }
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
    SelfHighHeartRate <- matrix(data$HeartRate[data$SelfHighWindow == TRUE], ncol = length(SelfHighIndex))
    SelfHighHeartRate <- SelfHighHeartRate[seq(1, NROW(SelfHighHeartRate), by=10), ]
    SelfLowHeartRate <- matrix(data$HeartRate[data$SelfLowWindow == TRUE], ncol = length(SelfLowIndex))
    SelfLowHeartRate <- SelfLowHeartRate[seq(1, NROW(SelfLowHeartRate), by=10), ]
    OtherHighHeartRate <- matrix(data$HeartRate[data$OtherHighWindow == TRUE], ncol = length(OtherHighIndex))
    OtherHighHeartRate <- OtherHighHeartRate[seq(1, NROW(OtherHighHeartRate), by=10), ]
    OtherLowHeartRate <- matrix(data$HeartRate[data$OtherLowWindow == TRUE], ncol = length(OtherLowIndex))
    OtherLowHeartRate <- OtherLowHeartRate[seq(1, NROW(OtherLowHeartRate), by=10), ]
    
    # Make a list for each participant and return it as output
    HeartRateData <- list(X, SelfHighHeartRate, SelfLowHeartRate, OtherHighHeartRate, OtherLowHeartRate)
    return(HeartRateData)
  }
}

# Read files
# First demographic data etc
demDataURL <- getURL("https://raw.githubusercontent.com/GNilsonne/Data-and-analysis-code-Oxazepam-and-emotion/master/demographics.csv", ssl.verifypeer = FALSE)
demData <- read.csv(text = demDataURL)
# Then the Acqknowledge logfiles containing EMG data
IncludedSubjects <- demData$Subject[demData$Included_EP == T & demData$Wave == 2] # Heart rate was only measured in wave 2
Excluded <- c(60, 76, 78) # These participants can be included in the experiment but not on the EKG measure, because they had electrode disattachment or frequent extrasystoles.
IncludedSubjects <- IncludedSubjects[!IncludedSubjects %in% Excluded]
HeartRateDataList <- lapply(IncludedSubjects, FUN = fun_readEPdata)

# Move all data to one big frame
HeartRateData <- data.frame()

for(i in 1:length(HeartRateDataList)){
  if(!is.null(HeartRateDataList[i][[1]])){
    SelfHighData <- melt(HeartRateDataList[i][[1]][[2]], na.rm = F)
    names(SelfHighData) <- c("time_0.1s", "event_no", "hr_bpm")
    SelfHighData$Condition <- "Self"
    SelfHighData$Stimulus <- "High"
    
    OtherHighData <- melt(HeartRateDataList[i][[1]][[3]], na.rm = F)
    names(OtherHighData) <- c("time_0.1s", "event_no", "hr_bpm")
    OtherHighData$Condition <- "Other"
    OtherHighData$Stimulus <- "High"
    
    SelfLowData <- melt(HeartRateDataList[i][[1]][[4]], na.rm = F)
    names(SelfLowData) <- c("time_0.1s", "event_no", "hr_bpm")
    SelfLowData$Condition <- "Self"
    SelfLowData$Stimulus <- "Low"
    
    OtherLowData <- melt(HeartRateDataList[i][[1]][[5]], na.rm = F)
    names(OtherLowData) <- c("time_0.1s", "event_no", "hr_bpm")
    OtherLowData$Condition <- "Other"
    OtherLowData$Stimulus <- "Low"
    
    HeartRateData_temp <- rbind(SelfHighData, OtherHighData, SelfLowData, OtherLowData)
    HeartRateData_temp$subject <- IncludedSubjects[i]
  }
  HeartRateData <- rbind(HeartRateData, HeartRateData_temp)
}

# Check how many data points have been removed
num_na <- sum(is.na(HeartRateData$hr_bpm))
frac_na <- num_na/length(HeartRateData$hr_bpm)

# Convert time scale and set 0 to event onset
HeartRateData$time_s <- HeartRateData$time_0.1s/10
HeartRateData$time_s <- HeartRateData$time_s -2
  
# Make spaghetti plots
#plot(hr_bpm ~ time_s, data = subset(HeartRateData, Condition == "Self" & Stimulus == "High"), frame.plot = F, type = 'n', main = "Self high", ylim = c(40, 200))
#for(i in unique(HeartRateData$subject)){
#  for( j in unique(HeartRateData$event_no[HeartRateData$subject == i])){
#    lines(hr_bpm ~ time_s, data = HeartRateData[HeartRateData$subject == i & HeartRateData$event_no == j & HeartRateData$Condition == "Self" & HeartRateData$Stimulus == "High", ], col = col4)
#  }
#}
  
#plot(hr_bpm ~ time_s, data = subset(HeartRateData, Condition == "Self" & Stimulus == "Low"), frame.plot = F, type = 'n', main = "Self low", ylim = c(40, 200))
#for(i in unique(HeartRateData$subject)){
#  for( j in unique(HeartRateData$event_no[HeartRateData$subject == i])){
#    lines(hr_bpm ~ time_s, data = HeartRateData[HeartRateData$subject == i & HeartRateData$event_no == j & HeartRateData$Condition == "Self" & HeartRateData$Stimulus == "Low", ], col = col7)
#  }
#}

#plot(hr_bpm ~ time_s, data = subset(HeartRateData, Condition == "Other" & Stimulus == "High"), frame.plot = F, type = 'n', main = "Other high", ylim = c(40, 200))
#for(i in unique(HeartRateData$subject)){
#  for( j in unique(HeartRateData$event_no[HeartRateData$subject == i])){
#    lines(hr_bpm ~ time_s, data = HeartRateData[HeartRateData$subject == i & HeartRateData$event_no == j & HeartRateData$Condition == "Other" & HeartRateData$Stimulus == "High", ], col = col4)
#  }
#}

#plot(hr_bpm ~ time_s, data = subset(HeartRateData, Condition == "Other" & Stimulus == "Low"), frame.plot = F, type = 'n', main = "Other low", ylim = c(40, 200))
#for(i in unique(HeartRateData$subject)){
#  for( j in unique(HeartRateData$event_no[HeartRateData$subject == i])){
#    lines(hr_bpm ~ time_s, data = HeartRateData[HeartRateData$subject == i & HeartRateData$event_no == j & HeartRateData$Condition == "Other" & HeartRateData$Stimulus == "Low", ], col = col7)
#  }
#}

MeanHRSelfHigh <- aggregate(hr_bpm ~ time_s, data = subset(HeartRateData, Condition == "Self" & Stimulus == "High"), mean)
MeanHRSelfLow <- aggregate(hr_bpm ~ time_s, data = subset(HeartRateData, Condition == "Self" & Stimulus == "Low"), mean)
MeanHROtherHigh <- aggregate(hr_bpm ~ time_s, data = subset(HeartRateData, Condition == "Other" & Stimulus == "High"), mean)
MeanHROtherLow <- aggregate(hr_bpm ~ time_s, data = subset(HeartRateData, Condition == "Other" & Stimulus == "Low"), mean)

plot(MeanHRSelfHigh, type = "l", col = col4, frame.plot = F, main = "Heart rate, self condition", ylim = c(67, 81))
lines(MeanHRSelfLow, col = col7)
legend("topleft", lty = 1, col = c(col4, col7), legend = c("High", "Low"), bty = "n")
abline(v = c(0, 0.5), lty = 2)

plot(MeanHROtherHigh, type = "l", col = col4, frame.plot = F, main = "Heart rate, other condition", ylim = c(67, 81))
lines(MeanHROtherLow, col = col7)
legend("topleft", lty = 1, col = c(col4, col7), legend = c("High", "Low"), bty = "n")
abline(v = c(0, 0.5), lty = 2)

# Since data have varying baselines between conditions, we index each response to its baseline from -2 to 0 seconds
HeartRateData$hr_index <- NA
for(i in unique(HeartRateData$subject)){
  for(j in unique(HeartRateData$event_no[HeartRateData$subject == i])){
    for(k in c("Self", "Other")){
      for(l in c("High", "Low")){
        HeartRateData$hr_index[HeartRateData$subject == i & HeartRateData$event_no == j & HeartRateData$Condition == k & HeartRateData$Stimulus == l] <- HeartRateData$hr_bpm[HeartRateData$subject == i & HeartRateData$event_no == j & HeartRateData$Condition == k & HeartRateData$Stimulus == l]/mean(HeartRateData$hr_bpm[HeartRateData$subject == i & HeartRateData$event_no == j & HeartRateData$Condition == k & HeartRateData$Stimulus == l][1:20])
      }
    }
  }
}

MeanHRIndexSelfHigh <- aggregate(hr_index ~ time_s, data = subset(HeartRateData, Condition == "Self" & Stimulus == "High"), mean)
MeanHRIndexSelfLow <- aggregate(hr_index ~ time_s, data = subset(HeartRateData, Condition == "Self" & Stimulus == "Low"), mean)
MeanHRIndexOtherHigh <- aggregate(hr_index ~ time_s, data = subset(HeartRateData, Condition == "Other" & Stimulus == "High"), mean)
MeanHRIndexOtherLow <- aggregate(hr_index ~ time_s, data = subset(HeartRateData, Condition == "Other" & Stimulus == "Low"), mean)

pdf("Fig_HR1.pdf", width = 4, height = 4)
plot(MeanHRIndexSelfHigh, type = "n", col = col4, frame.plot = F, main = "A. Heart rate, Self", ylim = c(0.95, 1.16), xlab = "Time, s", ylab = "Heart rate, normalised ratio", xaxt = "n")
abline(v = c(0, 0.5), lty = 3)
rect(2.5, 0, 4, 2, col = "gray88", border = NA)
axis(1)
lines(MeanHRIndexSelfHigh, col = col4, lwd = 2)
lines(MeanHRIndexSelfLow, col = col7, lty = 5, lwd = 2)
legend("topright", lty = c(1, 5), lwd = 2, col = c(col4, col7), legend = c("High shock", "Low shock"), bg = "white", cex = 0.8)
dev.off()

pdf("Fig_HR2.pdf", width = 4, height = 4)
plot(MeanHRIndexOtherHigh, type = "n", col = col4, frame.plot = F, main = "B. Heart rate, Other", ylim = c(0.95, 1.16), xlab = "Time, s", ylab = "Heart rate, normalised ratio", xaxt = "n")
abline(v = c(0, 0.5), lty = 3)
rect(2.5, 0, 4, 2, col = "gray88", border = NA)
axis(1)
lines(MeanHRIndexOtherHigh, col = col4, lwd = 2)
lines(MeanHRIndexOtherLow, col = col7, lty = 5, lwd = 2)
dev.off()

# Average data over 2.5-4 second interval for each event for statistical modelling
HeartRateEventData <- data.frame()
for(i in unique(HeartRateData$subject)){
  for(j in unique(HeartRateData$event_no[HeartRateData$subject == i])){
    for(k in c("Self", "Other")){
      for(l in c("High", "Low")){
        hr_mean <- mean(HeartRateData$hr_index[HeartRateData$subject == i & HeartRateData$event_no == j & HeartRateData$Condition == k & HeartRateData$Stimulus == l][45:60])
        HeartRateEventData <- rbind(HeartRateEventData, data.frame(i, j, k, l, hr_mean))
      }
    }
  }
}
names(HeartRateEventData) <- c("Subject", "event_no", "Condition", "Stimulus", "hr_mean")

# Analyse data
# Include IRI-EC terms in model since we wish to control for baseline empathic propensity 
HeartRateEventData <- merge(HeartRateEventData, demData, by = "Subject")
HeartRateEventData$Subject <- as.factor(HeartRateEventData$Subject)
HeartRateEventData$Treatment <- relevel(HeartRateEventData$Treatment, ref = "Placebo")
HeartRateEventData$Stimulus <- relevel(HeartRateEventData$Stimulus, ref = "Low")
HeartRateEventData$Condition <- relevel(HeartRateEventData$Condition, ref = "Self")

# Make a new column to specify an "Other High" contrast in modelling
HeartRateEventData$OtherHigh <- as.numeric(HeartRateEventData$Condition)*as.numeric(HeartRateEventData$Stimulus)
HeartRateEventData$OtherHigh[HeartRateEventData$OtherHigh != 4] <- 0
HeartRateEventData$OtherHigh[HeartRateEventData$OtherHigh == 4] <- 1

# z-transform IRI-EC
HeartRateEventData$IRI_EC_z <- scale(HeartRateEventData$IRI_EC)

# Make a regressor for predictor in other high condition only
HeartRateEventData$IRI_EC_z_OtherHigh <- HeartRateEventData$IRI_EC_z * HeartRateEventData$OtherHigh

# Mean center the new regressor
HeartRateEventData$IRI_EC_z_OtherHigh[HeartRateEventData$IRI_EC_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_EC_z_OtherHigh)] <- HeartRateEventData$IRI_EC_z_OtherHigh[HeartRateEventData$IRI_EC_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_EC_z_OtherHigh)] - mean(HeartRateEventData$IRI_EC_z_OtherHigh[HeartRateEventData$IRI_EC_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_EC_z_OtherHigh)], na.rm = TRUE)

# Build model
lme1 <- lme(hr_mean ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh, data = HeartRateEventData, random = ~1|Subject, na.action = na.omit)

plot(lme1)
summary(lme1)
intervals(lme1)

eff1 <- effect("Treatment*Stimulus*Condition", lme1)

pdf("Fig_HR3.pdf", width = 4, height = 4)
plot(c(eff1$fit[1], eff1$fit[3]),
     type = "n",
     frame.plot = F,
     ylab = "Heart rate, normalised ratio",
     xlab = "Shock intensity",
     xaxt = "n",
     yaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0.97, 1.12),
     col = col1,
     main = "C. Heart rate, Self"
)
lines(c(1, 2), c(eff1$fit[2], eff1$fit[4]), type = "b", col = col2, pch = 16)
lines(c(1.1, 2.1), c(eff1$fit[1], eff1$fit[3]), type = "b", col = col1)
lines(c(1.1, 1.1), c(eff1$upper[1], eff1$lower[1]), col = col1)
lines(c(2.1, 2.1), c(eff1$upper[3], eff1$lower[3]), col = col1)
lines(c(1, 1), c(eff1$upper[2], eff1$lower[2]), col = col2)
lines(c(2, 2), c(eff1$upper[4], eff1$lower[4]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("Low", "High"))
axis(2, at = c(1, 1.05, 1.1))
legend("topleft", col = c(col1, col2), pch = c(1, 16), legend = c("Placebo", "Oxazepam"), lty = 1, bty = "n")
dev.off()

pdf("Fig_HR4.pdf", width = 4, height = 4)
plot(c(eff1$fit[5], eff1$fit[7]),
     type = "n",
     frame.plot = F,
     ylab = "Heart rate, normalised ratio",
     xlab = "Shock intensity",
     xaxt = "n",
     yaxt = "n",
     xlim = c(1, 2.1),
     ylim = c(0.97, 1.12),
     col = col1,
     main = "D. Heart rate, Other"
)
lines(c(1, 2), c(eff1$fit[6], eff1$fit[8]), type = "b", col = col2, pch = 16)
lines(c(1.1, 2.1), c(eff1$fit[5], eff1$fit[7]), type = "b", col = col1)
lines(c(1.1, 1.1), c(eff1$upper[5], eff1$lower[5]), col = col1)
lines(c(2.1, 2.1), c(eff1$upper[7], eff1$lower[7]), col = col1)
lines(c(1, 1), c(eff1$upper[6], eff1$lower[6]), col = col2)
lines(c(2, 2), c(eff1$upper[8], eff1$lower[8]), col = col2)
axis(1, at = c(1.05, 2.05), labels = c("Low", "High"))
axis(2, at = c(1, 1.05, 1.1))
dev.off()

# Compare plots to less custom-generated output for verification
plot(effect("Treatment*Stimulus*Condition", lme1))
plot(effect("Stimulus*Condition", lme1))
plot(effect("IRI_EC_z_OtherHigh", lme1))

# Analyse other rating scales as predictors
# Since rating scales may be collinear, we include them one by one
# The procedure follows the same logic as above

# IRI-PT
HeartRateEventData$IRI_PT_z <- scale(HeartRateEventData$IRI_PT)
HeartRateEventData$IRI_PT_z_OtherHigh <- HeartRateEventData$IRI_PT_z * HeartRateEventData$OtherHigh
HeartRateEventData$IRI_PT_z_OtherHigh[HeartRateEventData$IRI_PT_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_PT_z_OtherHigh)] <- HeartRateEventData$IRI_PT_z_OtherHigh[HeartRateEventData$IRI_PT_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_PT_z_OtherHigh)] - mean(HeartRateEventData$IRI_PT_z_OtherHigh[HeartRateEventData$IRI_PT_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_PT_z_OtherHigh)], na.rm = TRUE)
lme2 <- lme(hr_mean ~ Treatment*Stimulus*Condition + IRI_PT_z + IRI_PT_z_OtherHigh, data = HeartRateEventData, random = ~1|Subject, na.action = na.omit)
plot(lme2)
summary(lme2)
intervals(lme2)

# IRI-PD
HeartRateEventData$IRI_PD_z <- scale(HeartRateEventData$IRI_PD)
HeartRateEventData$IRI_PD_z_OtherHigh <- HeartRateEventData$IRI_PD_z * HeartRateEventData$OtherHigh
HeartRateEventData$IRI_PD_z_OtherHigh[HeartRateEventData$IRI_PD_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_PD_z_OtherHigh)] <- HeartRateEventData$IRI_PD_z_OtherHigh[HeartRateEventData$IRI_PD_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_PD_z_OtherHigh)] - mean(HeartRateEventData$IRI_PD_z_OtherHigh[HeartRateEventData$IRI_PD_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_PD_z_OtherHigh)], na.rm = TRUE)
lme3 <- lme(hr_mean ~ Treatment*Stimulus*Condition + IRI_PD_z + IRI_PD_z_OtherHigh, data = HeartRateEventData, random = ~1|Subject, na.action = na.omit)
plot(lme3)
summary(lme3)
intervals(lme3)

# IRI-F
HeartRateEventData$IRI_F_z <- scale(HeartRateEventData$IRI_F)
HeartRateEventData$IRI_F_z_OtherHigh <- HeartRateEventData$IRI_F_z * HeartRateEventData$OtherHigh
HeartRateEventData$IRI_F_z_OtherHigh[HeartRateEventData$IRI_F_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_F_z_OtherHigh)] <- HeartRateEventData$IRI_F_z_OtherHigh[HeartRateEventData$IRI_F_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_F_z_OtherHigh)] - mean(HeartRateEventData$IRI_F_z_OtherHigh[HeartRateEventData$IRI_F_z_OtherHigh > 0 & !is.na(HeartRateEventData$IRI_F_z_OtherHigh)], na.rm = TRUE)
lme4 <- lme(hr_mean ~ Treatment*Stimulus*Condition + IRI_F_z + IRI_F_z_OtherHigh, data = HeartRateEventData, random = ~1|Subject, na.action = na.omit)
plot(lme4)
summary(lme4)
intervals(lme4)

# STAI-T
HeartRateEventData$STAI.T_z <- scale(HeartRateEventData$STAI.T)
HeartRateEventData$STAI.T_z_OtherHigh <- HeartRateEventData$STAI.T_z * HeartRateEventData$OtherHigh
HeartRateEventData$STAI.T_z_OtherHigh[HeartRateEventData$STAI.T_z_OtherHigh > 0 & !is.na(HeartRateEventData$STAI.T_z_OtherHigh)] <- HeartRateEventData$STAI.T_z_OtherHigh[HeartRateEventData$STAI.T_z_OtherHigh > 0 & !is.na(HeartRateEventData$STAI.T_z_OtherHigh)] - mean(HeartRateEventData$STAI.T_z_OtherHigh[HeartRateEventData$STAI.T_z_OtherHigh > 0 & !is.na(HeartRateEventData$STAI.T_z_OtherHigh)], na.rm = TRUE)
lme5 <- lme(hr_mean ~ Treatment*Stimulus*Condition + STAI.T_z + STAI.T_z_OtherHigh, data = HeartRateEventData, random = ~1|Subject, na.action = na.omit)
plot(lme5)
summary(lme5)
intervals(lme5)

# TAS-20
HeartRateEventData$TAS.20_z <- scale(HeartRateEventData$TAS.20)
HeartRateEventData$TAS.20_z_OtherHigh <- HeartRateEventData$TAS.20_z * HeartRateEventData$OtherHigh
HeartRateEventData$TAS.20_z_OtherHigh[HeartRateEventData$TAS.20_z_OtherHigh > 0 & !is.na(HeartRateEventData$TAS.20_z_OtherHigh)] <- HeartRateEventData$TAS.20_z_OtherHigh[HeartRateEventData$TAS.20_z_OtherHigh > 0 & !is.na(HeartRateEventData$TAS.20_z_OtherHigh)] - mean(HeartRateEventData$TAS.20_z_OtherHigh[HeartRateEventData$TAS.20_z_OtherHigh > 0 & !is.na(HeartRateEventData$TAS.20_z_OtherHigh)], na.rm = TRUE)
lme6 <- lme(hr_mean ~ Treatment*Stimulus*Condition + TAS.20_z + TAS.20_z_OtherHigh, data = HeartRateEventData, random = ~1|Subject, na.action = na.omit)
plot(lme6)
summary(lme6)
intervals(lme6)

# PPI-R-SCI
HeartRateEventData$PPI_SCI_z <- HeartRateEventData$PPI_1_SCI_R
HeartRateEventData$PPI_SCI_z[HeartRateEventData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
HeartRateEventData$PPI_SCI_z <- scale(HeartRateEventData$PPI_SCI_z)
HeartRateEventData$PPI_SCI_z_OtherHigh <- HeartRateEventData$PPI_SCI_z * HeartRateEventData$OtherHigh
HeartRateEventData$PPI_SCI_z_OtherHigh[HeartRateEventData$PPI_SCI_z_OtherHigh > 0 & !is.na(HeartRateEventData$PPI_SCI_z_OtherHigh)] <- HeartRateEventData$PPI_SCI_z_OtherHigh[HeartRateEventData$PPI_SCI_z_OtherHigh > 0 & !is.na(HeartRateEventData$PPI_SCI_z_OtherHigh)] - mean(HeartRateEventData$PPI_SCI_z_OtherHigh[HeartRateEventData$PPI_SCI_z_OtherHigh > 0 & !is.na(HeartRateEventData$PPI_SCI_z_OtherHigh)], na.rm = TRUE)
lme7 <- lme(hr_mean ~ Treatment*Stimulus*Condition + PPI_SCI_z + PPI_SCI_z_OtherHigh, data = HeartRateEventData, random = ~1|Subject, na.action = na.omit)
plot(lme7)
summary(lme7)
intervals(lme7)

# PPI-R-FD
HeartRateEventData$PPI_FD_z <- HeartRateEventData$PPI_1_FD_R
HeartRateEventData$PPI_FD_z[HeartRateEventData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
HeartRateEventData$PPI_FD_z <- scale(HeartRateEventData$PPI_FD_z)
HeartRateEventData$PPI_FD_z_OtherHigh <- HeartRateEventData$PPI_FD_z * HeartRateEventData$OtherHigh
HeartRateEventData$PPI_FD_z_OtherHigh[HeartRateEventData$PPI_FD_z_OtherHigh > 0 & !is.na(HeartRateEventData$PPI_FD_z_OtherHigh)] <- HeartRateEventData$PPI_FD_z_OtherHigh[HeartRateEventData$PPI_FD_z_OtherHigh > 0 & !is.na(HeartRateEventData$PPI_FD_z_OtherHigh)] - mean(HeartRateEventData$PPI_FD_z_OtherHigh[HeartRateEventData$PPI_FD_z_OtherHigh > 0 & !is.na(HeartRateEventData$PPI_FD_z_OtherHigh)], na.rm = TRUE)
lme8 <- lme(hr_mean ~ Treatment*Stimulus*Condition + PPI_FD_z + PPI_FD_z_OtherHigh, data = HeartRateEventData, random = ~1|Subject, na.action = na.omit)
plot(lme8)
summary(lme8)
intervals(lme8)

# PPI-R-C
HeartRateEventData$PPI_C_z <- HeartRateEventData$PPI_1_C_R
HeartRateEventData$PPI_C_z[HeartRateEventData$PPI_1_IR >= 45] <- NA # Exclude participants with too high scores on Inconsistent Responding measure
HeartRateEventData$PPI_C_z <- scale(HeartRateEventData$PPI_C_z)
HeartRateEventData$PPI_C_z_OtherHigh <- HeartRateEventData$PPI_C_z * HeartRateEventData$OtherHigh
HeartRateEventData$PPI_C_z_OtherHigh[HeartRateEventData$PPI_C_z_OtherHigh > 0 & !is.na(HeartRateEventData$PPI_C_z_OtherHigh)] <- HeartRateEventData$PPI_C_z_OtherHigh[HeartRateEventData$PPI_C_z_OtherHigh > 0 & !is.na(HeartRateEventData$PPI_C_z_OtherHigh)] - mean(HeartRateEventData$PPI_C_z_OtherHigh[HeartRateEventData$PPI_C_z_OtherHigh > 0 & !is.na(HeartRateEventData$PPI_C_z_OtherHigh)], na.rm = TRUE)
lme9 <- lme(hr_mean ~ Treatment*Stimulus*Condition + PPI_C_z + PPI_C_z_OtherHigh, data = HeartRateEventData, random = ~1|Subject, na.action = na.omit)
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
pdf("Fig_HR5.pdf", width = 4, height = 4)
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
        main = "C. Heart rate",
        panel = function (x,y,z,...){
          panel.segplot(x,y,z,...)
          panel.abline(v=0,lty=2)
        })
dev.off()

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
pdf("Fig_HR6.pdf", width = 4, height = 4)
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
        main = "C. Heart rate",
        panel = function (x,y,z,...){
          panel.segplot(x,y,z,...)
          panel.abline(v=0,lty=2)
        })
dev.off()



# Make new plots including tables of effects
data_main2 <- data_main[4:9, ]
data_main2 <- data_main2[rev(rownames(data_main2)),]

pdf("Fig_HR7.pdf", width = 6, height = 3.2)
par(mar=c(5.1, 5, 4, 14))
plot(x = data_main2$beta, y = c(8:5, 3, 1), xlab = expression(beta), ylab = "", frame.plot = F, xlim = c(min(data_main2$lower), max(data_main2$upper)), xaxt = "n", yaxt = "n")
title("C. Heart Rate", line = 2)
abline(v = 0, col = "gray")
axis(1, at = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02))
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

pdf("Fig_HR8.pdf", width = 6, height = 3.2)
par(mar=c(5.1, 5, 4, 14))
plot(x = data_emp2$beta, y = c(8:5, 3, 1), xlab = expression(beta), ylab = "", frame.plot = F, xlim = c(min(data_emp2$lower), max(data_emp2$upper)), xaxt = "n", yaxt = "n")
title("C. Heart Rate", line = 2)
abline(v = 0, col = "gray")
axis(1, at = c(-0.02, 0, 0.02), labels = c(-0.02, 0, 0.02))
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

# Write regression table
write.csv(summary(lme1)$tTable, file = "Result_tables/HR_Wave2.csv")

# Analyse effect of rated likability of confederate
HeartRateEventData$RatedSympathy<- scale(HeartRateEventData$RatedSympathy)
HeartRateEventData$RatedSympathy_OtherHigh <- HeartRateEventData$RatedSympathy * HeartRateEventData$OtherHigh
HeartRateEventData$RatedSympathy_OtherHigh[HeartRateEventData$RatedSympathy_OtherHigh > 0 & !is.na(HeartRateEventData$RatedSympathy_OtherHigh)] <- HeartRateEventData$RatedSympathy_OtherHigh[HeartRateEventData$RatedSympathy_OtherHigh > 0 & !is.na(HeartRateEventData$RatedSympathy_OtherHigh)] - mean(HeartRateEventData$RatedSympathy_OtherHigh[HeartRateEventData$RatedSympathy_OtherHigh > 0 & !is.na(HeartRateEventData$RatedSympathy_OtherHigh)], na.rm = TRUE)

lme2 <- lme(hr_mean ~ Treatment*Stimulus*Condition + IRI_EC_z + IRI_EC_z_OtherHigh + RatedSympathy + RatedSympathy_OtherHigh, data = HeartRateEventData, random = ~1|Subject, na.action = na.omit)
plot(lme2)
summary(lme2)
intervals(lme2)
