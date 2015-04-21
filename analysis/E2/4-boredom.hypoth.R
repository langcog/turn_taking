# library(grid)
# library(ggplot2)
# library(bootstrap)
# library(lme4)
# library(stringr)
# library(plotrix)
# library(reshape)
# library(plyr)
# library(car)
# library(gridExtra)
# library(scales)  
# library(Hmisc)
# library(moments)
rm(list = ls())
source("0-helper.R")
library(data.table)
library(bit64)

################################################################################
################################################################################
info.path <- "info/"
processed.data.path <- "processed_data_boredom/"
plot.path <- "plots/"

colorPalette <- c("black", "gray")

# Read in supplemental data and prep
vid.info <- fread(paste(info.path, "VideoSegmentInfo.csv", sep=""))
#vid.info$VideoDuration <- NULL
turn.info <- fread(paste(info.path, "TurnInfo.csv", sep=""))
subject.info <- fread(paste(info.path, "SubjectInfo.csv", sep=""))

# Round the time values for turns
turn.info = within(turn.info, {
  Onset = round(Onset, 5)
  Offset = round(Offset, 5)
})

# Read in main datafile
all.data <- fread("processed_data/imported.data.csv")

# Merge in the stimulus (video) information
setkey(all.data, "Segment")
setkey(vid.info, "Segment")
all.data <- vid.info[all.data]
# Subset the data to only include target stimuli
all.data <- all.data[StimulusGroup == "target"]

# Collapse male-looks and female-looks into a single look-direction column
# F and M are each TRUE or FALSE (coerced to 1 or 0)
all.data[, Look.Dir := ifelse(
	F.looks > M.looks, "F", ifelse(
	M.looks > F.looks, "M", "0"))]

# Write out normal data up until now
write.csv(all.data,
    paste(processed.data.path, "all.data.before.analysis.bh.csv", sep=""),
    row.names = FALSE)

# Re-order the data table
setkeyv(all.data, c("Subject", "Segment", "TimeSec"))

all.data <- smoothLD(all.data)

# Write out the prepped data for input into the transition analysis
write.csv(all.data,
    paste(processed.data.path, "prepped.data.bh.csv", sep=""),
    row.names=FALSE)

# Only include rows where participants are looking at one speaker or the other
all.data <- all.data[Look.Dir != "0"]

# Create a data frame of fixations
fixations <- findFixations(all.data)
fixations[, FixDur := TSSOffset - TSSOnset]

# Write out basic fixation data
write.csv(fixations,
    paste(processed.data.path, "fixations.csv", sep=""),
    row.names = FALSE)

# Add cols for later
fixations[, onset := TSSOnset]
fixations[, offset := TSSOffset]
fixations[, UttID := rep("",nrow(fixations))]
fixations[, UttOns := rep("",nrow(fixations))]
fixations[, UttOff := rep("",nrow(fixations))]
# Add subject info back in
setkeyv(fixations, "Subject")
setkeyv(subject.info, "Subject")
fixations <- subject.info[fixations]

# Find fixations that have boredom-like look-aways
segments <- unique(turn.info$Segment)
boredom.fixes <- fixations[0,]
for (seg in segments) {
	onsets <- subset(turn.info, Segment == seg & UttID != "na")$Onset
	offsets <- onsets + 1
	targets <- subset(turn.info, Segment == seg & UttID != "na")$True.Sp.F
	targets <- ifelse(targets == 1, "F", "M")
	
	for (j in 1:length(onsets)) {
		# Retrieve indices for fixations that partially overlap with the
		# first second of a turn (offset is onset + 1s). Fixations must
		# be at least 500ms long and must be directed toward the current
		# speaker ("target")
		fixstartturn <- which(fixations$Segment == seg &
							fixations$TSSOnset < offsets[j] &
							fixations$TSSOffset > onsets[j] &
							fixations$Target == targets[j] &
							fixations$FixDur >= 0.5)
		# Retrieve indices for the subset of those fixations that start
		# before the turn does
		early <- which(fixations$Segment == seg &
							fixations$TSSOnset < offsets[j] &
							fixations$TSSOffset > onsets[j] &
							fixations$Target == targets[j] &
							fixations$FixDur >= 0.5 &
							fixations$TSSOnset < onsets[j])
		# Retrieve indices for the subset of those fixations that end
		# after the turn does
		late <- which(fixations$Segment == seg &
							fixations$TSSOnset < offsets[j] &
							fixations$TSSOffset > onsets[j] &
							fixations$Target == targets[j] &
							fixations$FixDur >= 0.5 &
							fixations$TSSOffset > offsets[j])
							

		# Change fixation onsets and offsets to turn beginnings and ends
		# (to calculate overlap)
		# Set the onset to the turn beginning for all fixations that started
		# before the turn 
		fixations[early, onset := onsets[j]]
		# Set the offset to the turn end for all fixations that ended after
		# the turn 
		fixations[late, offset := offsets[j]]
		
		
		# Check for four properties of a boredom-driven switch away:

		# 1:
		# Fixations that overlap with the first second of the turn by
		# at least 500ms
		fixone <- fixations$offset[fixstartturn] -
			fixations$onset[fixstartturn] >= 0.5

		# 2:
		# Fixations that are followed by a substantial look away (i.e., a
		# boredom-driven fixation, here defined as the mean fixation duration
		# for all fixations, discounting fixations shorter than 100ms, which
		# we have not counted as meaningful)
		fixtwo <- fixations$FixDur[fixstartturn+1] >=
			mean(subset(fixations, FixDur > 0.1)$FixDur)

		# 3:
		# Fixations that are followed by a fixation to the opposite speaker
		targetswitch <- fixations$Target[fixstartturn] !=
			fixations$Target[fixstartturn+1]

		# 4:
		# Fixations that are followed by a fixation in the same video stimulus
		# (to exclude the possibility of counting switches across video boundaries)
		segmentsame <- fixations$Segment[fixstartturn] ==
			fixations$Segment[fixstartturn+1]

		# Find the switches that would count as boredom-driven switches away:
		# Focusing on the subset of fixations the overlapped with a turn beginning,
		fixstarts <- fixations[fixstartturn]
		# Sum the four properties of a boredom switch into a column called "Keep"
		fixstarts[, Keep := fixone + fixtwo + targetswitch + segmentsame]
		# Only keep those with all four properties
		fixstarts <- subset(fixstarts, Keep == 4)
		fixstarts$Keep <- NULL
		# Set the utterance information (ID, onset, and offset) for
		# remaining fixations
		if (nrow(fixstarts) > 0) {
			fixstarts[, UttID :=
				rep(subset(turn.info, Segment == seg & UttID != "na")$UttID[j],
				nrow(fixstarts))]
			fixstarts[, UttOns := rep(onsets[j], nrow(fixstarts))]
			fixstarts[, UttOff :=
				rep(subset(turn.info, Segment == seg & UttID != "na")$Offset[j],
				nrow(fixstarts))]
		}
		boredom.fixes <- rbind(boredom.fixes, fixstarts)
	}
}

# Add columns for time until look away and utterance duration
boredom.fixes[, LookAway := TSSOffset - UttOns]
boredom.fixes[, UttDur := UttOff - UttOns]

# Only use utterances that were 1-4 seconds long
boredom.fixes <- subset(boredom.fixes, UttDur >= 1 & UttDur < 4)

# Discard the data from adults and one 7-year-old
boredom.fixes <- subset(boredom.fixes, Age < 7)

# Factor the data into utterance lengths and age groups
seconds <- c(1:5)
secbins <- findInterval(boredom.fixes$UttDur, seconds)
boredom.fixes[, SecBins :=  as.factor(seconds[secbins])]
boredom.fixes$SecBins <- factor(boredom.fixes$SecBins,
    levels = c("1","2","3"), labels=c("1-2 sec","2-3 sec", "3-4 sec"))
boredom.fixes$Age <- as.factor(boredom.fixes$Age)
boredom.fixes$Age <- factor(boredom.fixes$Age,
    levels = c("1","2","3","4","5","6"),
    labels=c("1-year-olds","2-year-olds", "3-year-olds",
    "4-year-olds","5-year-olds", "6-year-olds"))
    
# Write it out (for optional use later)
write.csv(boredom.fixes,
    paste(processed.data.path, "boredom.fixations.csv", sep=""),
    row.names = FALSE)
    
# Graph the results of the real gaze data against an idealized bored observer:
# Time sequence for the x-axis
time <- seq(0,4.5,0.1)
# Fill a data frame with cumulative looks away for the real gaze data
realdata <- data.frame(Time=numeric(),Switches=numeric(),SecBin=character())
for (j in 1:length(unique(boredom.fixes$SecBins))) {
	switches <- rep(0, length(time))
	secbin <- as.character(unique(boredom.fixes$SecBins)[j])
	subdata <- subset(boredom.fixes, SecBins == secbin)
	nswitches <- nrow(subdata)
	secbins <- rep(secbin,length(time))
	for (k in 1:length(time)) {
		switches[k] <- 1 - (nrow(subset(subdata, LookAway < time[k]))/nswitches)
	}
	newswitches <- data.frame(Time=time,Switches=switches,SecBin=secbins)
	realdata <- rbind(realdata, newswitches)
}
realdata$Sample <- "actual"


# Fill a data frame with cumulative looks away for the boredom-driven ideal
#
# Set properties of bored observer:
# 1. Linear rate of boredom (step size of look-aways with time)
# Set to assume complete boredom (100% cumulative look aways) after 4.5 sec
increment <- (1/(length(time)-6))
# 2. Initial fixation time on the speaker
# Set to 5*x-axis increments (500ms)
# Note: the subtracted value in "increment" should be the repetition value
# below in "boredstart" + 1
boredstart <- rep(1,5)

# Create cumulative look away given the initial fixation and
# rate of boredom
boredlooks <- 1-seq(0,1, increment)
boredlooks <- c(boredstart,boredlooks)
# Fill it into the cumulative data frame for plotting
boredom <- data.frame(Time=numeric(),Switches=numeric(),SecBin=character())
	for (j in 1:length(unique(boredom.fixes$SecBins))) {
		secbin <- as.character(unique(boredom.fixes$SecBins)[j])
		secbins <- rep(secbin,length(time))
		newswitches <- data.frame(Time=time,Switches=boredlooks,SecBin=secbins)
		boredom <- rbind(boredom, newswitches)
	}
boredom$Sample <- "bored"

# Bind the data together
real.vs.bored <- rbind(realdata, boredom)

# Clip off data points that are beyond the maximum turn length + 500ms
# (they are not informative about what happens over the course of the turn)
# Note: this needs to be done separately for turns of different lengths
real.vs.bored.12 <- subset(real.vs.bored, SecBin == "1-2 sec" & Time <= 2.5)
real.vs.bored.12$xmax <- 2.5
real.vs.bored.23 <- subset(real.vs.bored, SecBin == "2-3 sec" & Time <= 3.5)
real.vs.bored.23$xmax <- 3.5
real.vs.bored.34 <- subset(real.vs.bored, SecBin == "3-4 sec" & Time <= 4.5)
real.vs.bored.34$xmax <- 4.5
rvb.clipped <- rbind(real.vs.bored.12, real.vs.bored.23, real.vs.bored.34)

# Plot and save the results
boredom.hyp <- ggplot(rvb.clipped, aes(Time, Switches)) +   
     geom_point(aes(colour = Sample, size=1)) +
     geom_blank(aes(x = 0)) + geom_blank(aes(x = xmax)) +
    scale_colour_manual(name = "Sample", values=colorPalette) +
     facet_wrap(~ SecBin, scales = "free_x") +
     scale_x_continuous(breaks=c(0,1,2,3,4)) +
     xlab("Time (sec)") + ylab("Proportion gazing at the speaker\n") +
     plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(size=30, color="gray40"),
    axis.text.y = element_text(size=30, color="gray40"),
    axis.title.x = element_text(size=30, color="gray20"),
    axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),     
    strip.text.x = element_text(size=30, color="gray20"),
    legend.position = "none", plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "boredom-hypothesis.png", sep=""),
    width=1500,height=600,units="px", bg = "transparent")
print(boredom.hyp)
dev.off()
