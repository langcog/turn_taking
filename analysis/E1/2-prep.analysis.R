rm(list = ls())
source("0-useful.R")
source("0-helper.R")
source("0-find.transitions.R")

raw.data.path <- "tracker_data/"
info.path <- "info/"
processed.data.path <- "processed_data/"

# Read in supplemental data and prep
vid.info <- read.csv(paste(info.path,"VideoSegmentInfo.csv",sep=""))
turn.info <- read.csv(paste(info.path,"TurnInfo.csv",sep=""))
subject.info <- read.csv(paste(info.path,"SubjectInfo.csv",sep=""))
gap.info <- read.csv(paste(info.path,"GapInfo.csv",sep=""))

# Round the time values for turns and gaps
turn.info = within(turn.info, {
  Onset = round(Onset, 3)
  Offset = round(Offset, 3)
})
gap.info = within(gap.info, {
  Onset = round(Onset, 3)
  Offset = round(Offset, 3)
})


################################################################################
# First we'll merge the gaze data with the information about the stimuli

# Read in main datafile
all.data <- read.csv(paste(processed.data.path,"imported.data.csv",sep=""))

# Merge in condition sites, using onsets
all.data$Onset <- rep(NA, nrow(all.data))
for (cond in unique(all.data$Condition)) {
    # For each TimeSecSeg measurement, find the onset of the segment
    # (from vid.info) within which it lies
    onsets <- vid.info$Onset[vid.info$Cond == cond]
    # REMINDER: example use of findInterval:
    # findInterval(c(2,5,7,8), c(4,5,6,8,9,10)) => [1] 0 2 3 4
    # In this case it returns a list as long as all.data$TimeSecSeg, with
    # the index of the video onset that the each time measurement falls within
    # (e.g., t = 5, vid[2] and t=7, vid[3] in the above example)
    onset.indices <- findInterval(all.data$TimeSec[all.data$Condition == cond],
        onsets)
    # Maps these values onto the subset of times it was derived from
    all.data$Onset[all.data$Condition == cond] <- onsets[onset.indices]
}
# Add in vid.info data and re-order the main data frame
all.data <- merge(all.data, vid.info, by = c("Condition", "Onset"))
all.data <- all.data[order(all.data$Subject, all.data$TimeSec),]

# Merge in turn sites, using onsets
all.data$Onset <- rep(NA, nrow(all.data))
for (cond in unique(turn.info$Condition)) {
    for(seg in unique(turn.info$Segment)) {
    # For each TimeSecSeg measurement, find the onset of the utterance
    # (from turn.info) within which it lies
    onsets <- turn.info$Onset[turn.info$Condition == cond &
        turn.info$Segment == seg]
    # Same as above
    onset.indices <- findInterval(all.data$TimeSec[all.data$Condition == cond &
        all.data$VideoSegment == seg], onsets)
    # Maps these values onto the subset of times it was derived from
    all.data$Onset[all.data$Condition == cond & all.data$VideoSegment == seg] <-
        onsets[onset.indices]
    }
}
# Add in a subset of the turn.info data and re-order the main data frame
turn.info.cols <- c("True.Sp.L", "True.Sp.R", "GenderL", "Condition", "Onset")
m.turn.info <- turn.info[turn.info.cols]
all.data <- merge(all.data, m.turn.info, by = c("Condition", "Onset"))
all.data <- all.data[order(all.data$Subject, all.data$TimeSec),]

# Add in the subject.info data and re-order the main data frame
all.data <- merge(all.data, subject.info, by = c("Subject", "AgeGroup"))
all.data <- all.data[order(all.data$Subject, all.data$TimeSec),]


# Collapse left-looks and right-looks into a single look-direction column
# l and r are each TRUE or FALSE (coerced to 1 or 0)
all.data$Look.Dir <- mapply(function(l, r) {
		if(l > r) {
			return("L")
		}
		else if(l < r) {
			return("R")
		}
		else{
			return(NA)
		}
}, all.data$L.looks, all.data$R.looks)

# Write out the data up until now for analysis
write.csv(all.data, paste(processed.data.path,
    "all.data.before.analysis.csv", sep=""))
# Save a copy for the random baseline runs of the analysis
write.csv(all.data, paste(processed.data.path, "r.all.data.csv", sep=""))

# Separate adult and child data for finding anticipatory switches
# because adults have an assumed faster gaze planning time of 200 msec
# compared to our assumed 333 msec for children
all.data.A <- subset(all.data, AgeGroup == "ADU")
all.data.C <- subset(all.data, AgeGroup == "CHI")


################################################################################
# Now we'll find the anticipatory gaze switches, using the merged information

# Anticipatory switch identification variables
fixation.window = 0.1
# could change utt settings to "reaction" rather than "anticipation" values
utt1.overlap.A = 0.2
utt2.overlap.A = 0.2
utt1.overlap.C = 0.333
utt2.overlap.C = 0.333

# Extract time windows for switch analysis and prepare new looking columns
# For adults
target.windows.proport.A <- get.windows.categorical(all.data.A, gap.info,
    utt1.overlap.A, utt2.overlap.A, fixation.window)
target.windows.proport.A$Looks.Origin <- as.integer(
    target.windows.proport.A$Origin == target.windows.proport.A$Look.Dir)
target.windows.proport.A$Looks.Destination <- ifelse(((
    target.windows.proport.A$Origin == "L" &
    target.windows.proport.A$Look.Dir == "R") |
    (target.windows.proport.A$Origin == "R" &
    target.windows.proport.A$Look.Dir == "L")), 1, 0)
# For children
target.windows.proport.C <- get.windows.categorical(all.data.C, gap.info,
    utt1.overlap.C, utt2.overlap.C, fixation.window)
target.windows.proport.C$Looks.Origin <- as.integer(
    target.windows.proport.C$Origin == target.windows.proport.C$Look.Dir)
target.windows.proport.C$Looks.Destination <- ifelse(((
    target.windows.proport.C$Origin == "L" &
	target.windows.proport.C$Look.Dir == "R") |
	(target.windows.proport.C$Origin == "R" &
	target.windows.proport.C$Look.Dir == "L")), 1, 0)
					  
# Identify anticipatory switches in the data and combine it again
trans.info.A <- grab.transition.info(target.windows.proport.A, fixation.window)
trans.info.C <- grab.transition.info(target.windows.proport.C, fixation.window)
trans.info <- rbind(trans.info.A, trans.info.C)

# Write the switch output out
write.csv(trans.info, paste(processed.data.path, "trans.info.csv", sep=""))

# Merge gap and subject info back in for analysis. We don't care about the
# condition-specific information in gap.info, so we just pick an arbitrary
# condition.
trans.info.gaps <- merge(trans.info, subset(gap.info, Condition == "A"),
    by = "Gap")
trans.info.subs <- merge(trans.info.gaps, subject.info, by = "Subject")
trans.info.subs <- trans.info.subs[order(trans.info.subs$Subject,
    trans.info.subs$Gap),]

# Add "Transition" annotation for later combination with the random data
switch.trans <- trans.info.subs
switch.trans$SampleType <- rep("TRANSITION", nrow(switch.trans))

# Add condition data back in for main analysis
switch.final <- switch.trans
switch.final$Ttype <- switch.final$Type

# Write out the data, which is now ready for plotting and statistical analysis
write.csv(switch.final, paste(processed.data.path, "switch.final.csv", sep=""))

# Extract cumulative switch values to create a plot
cumulative.conditions.age <- cumulative.switches.by.cond(switch.final)
cumulative.conditions.age.final <-  subset(cumulative.conditions.age,
    TimeIncrement != "NA")
# Save it for later plotting
write.csv(cumulative.conditions.age.final, paste(processed.data.path,
    "cumulative.conditions.age.csv", sep=""))