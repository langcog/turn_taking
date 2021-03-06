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

# Add in the video.info data, keep only target videos, and re-order the data
all.data <- merge(all.data, vid.info, by = "Segment")
all.data <- all.data[order(all.data$Subject, all.data$TimeSecSeg),]

# Merge in turn sites, using onsets
all.data$Onset <- rep(NA, nrow(all.data))
for (vid in unique(all.data$Segment)) {
    # For each TimeSecSeg measurement, find the onset of the segment
    # (from turn.info) within which it lies
    onsets <- turn.info$Onset[turn.info$Segment == vid]
    # REMINDER: example use of findInterval:
    # findInterval(c(2,5,7,8), c(4,5,6,8,9,10)) => [1] 0 2 3 4
    # In this case it returns a list as long as all.data$TimeSecSeg, with
    # the index of the video onset that the each time measurement falls within
    # (e.g., t = 5, vid[2] and t=7, vid[3] in the above example)
    vid.onset.indices <- findInterval(
        all.data$TimeSecSeg[all.data$Segment == vid], onsets)
    # Maps these values onto the subset of times it was derived from
    all.data$Onset[all.data$Segment == vid] <- onsets[vid.onset.indices]
}
# Add in turn.info data and re-order the main data frame
all.data <- merge(all.data, turn.info, by = c("Segment", "Onset"))
all.data <- all.data[order(all.data$Subject, all.data$TimeSecSeg),]

# Save a copy for the random baseline runs of the analysis
write.csv(all.data, paste(processed.data.path, "r.all.data.csv", sep=""), row.names=FALSE)

# Merge in actual gap sites to the data, using onset values
all.data$Onset <- rep(NA, nrow(all.data))
for (vid in unique(all.data$Segment)) {
    # For each TimeSecSeg measurement, find the onset of the utterance
    # (from gap.info) within which it lies
    onsets <- gap.info$Onset[gap.info$Segment == vid]
    # Same as above
    vid.gap.indices <- findInterval(all.data$TimeSecSeg[all.data$Segment == vid],
        onsets)
    # Maps these values onto the subset of times it was derived from
    all.data$Onset[all.data$Segment == vid] <- onsets[vid.gap.indices]
}
# Add in gap.info data and re-order the main data frame
all.data <- merge(all.data, gap.info, by = c("Segment", "Onset"))
all.data <- all.data[order(all.data$Subject, all.data$TimeSecSeg),]

# Merge in subject info
all.data <- merge(all.data, subject.info, by = "Subject")

# Collapse male-looks and female-looks into a single look-direction column
# F and M are each TRUE or FALSE (coerced to 1 or 0)
all.data$Look.Dir <- mapply(function(f, m) {
    if(f > m) {
        return("F")
    }
    else if(f < m) {
        return("M")
    }
    else{
        return(NA)
    }
}, all.data$F.looks, all.data$M.looks)

# Write out normal data up until now for analysis
write.csv(all.data, paste(processed.data.path,
    "all.data.before.analysis.csv", sep=""), row.names=FALSE)

################################################################################
# Now we'll find the anticipatory gaze switches, using the merged information

# Anticipatory switch identification variables
fixation.window = 0.1
# could change utt settings to "reaction" rather than "anticipation" values
utt1.overlap = 0.25
utt2.overlap = 0.25

# Extract time windows for switch analysis and prepare new looking columns
target.windows.proport <- get.windows.categorical(all.data, gap.info,
    utt1.overlap, utt2.overlap, fixation.window)
target.windows.proport$Looks.Origin <- as.integer(
    target.windows.proport$Origin == target.windows.proport$Look.Dir)
target.windows.proport$Looks.Destination <- ifelse(((
    target.windows.proport$Origin == "F" &
	target.windows.proport$Look.Dir == "M") |
	(target.windows.proport$Origin == "M" &
	target.windows.proport$Look.Dir == "F")), 1, 0)
 
# Identify anticipatory switches in the data and combine it again
trans.info <- grab.transition.info(target.windows.proport, fixation.window)
trans.info <- subset(trans.info, Switch != "NA")

# Write the switch output out
write.csv(trans.info, paste(processed.data.path, "trans.info.csv", sep=""), row.names=FALSE)

# Merge gap and subject info back in for analysis.
trans.info.gaps <- merge(trans.info, gap.info, by = "Gap")
trans.info.subs <- merge(trans.info.gaps, subject.info, by = "Subject")
trans.info.subs <- trans.info.subs[order(trans.info.subs$Subject, trans.info.subs$Onset),]

# Add "Transition" annotation for later combination with the random data
switch.trans <- trans.info.subs
switch.trans$SampleType <- rep("TRANSITION", nrow(switch.trans))

# Add condition data back in for main analysis
switch.final <- merge(switch.trans, vid.info, by="Segment")

# Take out short gaps (not equal in all conditions)
switch.final <- subset(switch.final, Duration > 0.09)

# Create coarser age levels
switch.final$Age.coarse <- factor(NA,levels=c("1-2","3-4","5-6","21"))
switch.final$Age.coarse[switch.final$Age < 3] <- "1-2"
switch.final$Age.coarse[switch.final$Age >= 3 & switch.final$Age < 5] <- "3-4"
switch.final$Age.coarse[switch.final$Age >=5 & switch.final$Age < 7] <- "5-6"
switch.final$Age.coarse[switch.final$Age > 7] <- "21"
switch.final.coarse <- subset(switch.final, select = -Age)
switch.final.coarse$Age <- switch.final.coarse$Age.coarse
# Make the by-year ages character levels instead of integers
switch.final$Age <- as.character(switch.final$Age)

# Rename and re-code turn type for coarse and by-year versions;
# fix wh- to non-Qs in muffled condition
switch.final.coarse$Ttype <- switch.final.coarse$Type.x

# Write out the data, which is now ready for plotting and statistical analysis
# Save two versions, one with 1-year age and one with 2-year age bins
write.csv(switch.final, paste(processed.data.path, "switch.final.csv", sep=""), row.names=FALSE)
write.csv(switch.final.coarse, paste(processed.data.path,
    "switch.final.coarse.csv", sep=""), row.names=FALSE)

# Extract cumulative switch values to create a plot
cumulative.conditions.age.c <- cumulative.switches.by.cond(switch.final.coarse)
cumulative.conditions.age.c.final <- subset(cumulative.conditions.age.c,
    TimeIncrement != "NA")
# Save it for later plotting
write.csv(cumulative.conditions.age.c.final, paste(processed.data.path,
    "cumulative.conditions.age.c.csv", sep=""), row.names=FALSE)