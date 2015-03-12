rm(list = ls())
source("0-helper.R")
source("0-make.random.gaps.R")
library(data.table)
library(bit64)

# Set paths
raw.data.path <- "tracker_data/"
info.path <- "info/"
processed.data.path <- "processed_data/"


################################################################################
# Read in all the tracker data
################################################################################
# Initialize a data table for all the tracker data:
# Make a list of the raw tracker data files for importing
files <- dir(raw.data.path, pattern="*.txt")
# Set a maximum length for the data table
N <- length(files) * 28941 # max file length from tracker data
# Create the table
all.data <- data.table(
	x.pos = rep(0, N),
	y.pos = rep(0, N),
	F.looks = rep(FALSE, N),
	M.looks = rep(FALSE, N),
	Events = rep(FALSE, N),
	Version = rep(0, N),
	Subject = rep("", N),
	TimeSec = rep(0, N),
	TimeSecSeg = rep(0, N),
	MS.Increment = rep(0, N),
	Segment = rep("", N),
	Saccades = rep(FALSE, N),
	Fixations = rep(FALSE, N))
# Loop through the tracker files, grabbing the
# data we want and adding it to the table
curr.row <- 1
for (file.name in files) {
	#print (file.name)
	# Read in the data
	data <- read.data(raw.data.path, file.name)
    # Take only the columns we want for further analysis
    curr.rows <- curr.row:(curr.row+nrow(data)-1)
	data <- subset(data, select = c("x.pos", "y.pos", "F.looks",
	    "M.looks", "Events", "Version", "Subject", "TimeSec", "TimeSecSeg",
	    "MS.Increment", "Stimulus", "Saccades", "Fixations"))
	setnames(data, "Stimulus", "Segment")
    # Add the new file to the large data table
    all.data[curr.rows, names(all.data) := data]
    curr.row <- curr.row+nrow(data)
}
# Remove the empty rows and re-sort
all.data <- all.data[1:(curr.row-1),]
setkeyv(all.data, c("Subject","Segment","TimeSecSeg"))

# Save the imported data in a separate .csv, if desired
write.csv(all.data, paste(processed.data.path,"imported.data.csv",sep=""), row.names=FALSE)


################################################################################
# Merge the tracker data with supplementary information needed for analysis
################################################################################
# Read in supplemental data and prep
vid.info <- fread(paste(info.path,"VideoSegmentInfo.csv",sep=""))
turn.info <- fread(paste(info.path,"TurnInfo.csv",sep=""))
subject.info <- fread(paste(info.path,"SubjectInfo.csv",sep=""))
gap.info <- fread(paste(info.path,"GapInfo.csv",sep=""))

# Round the time values for turns and gaps
turn.info = within(turn.info, {
  Onset = round(Onset, 5)
  Offset = round(Offset, 5)
})
gap.info = within(gap.info, {
  Onset = round(Onset, 5)
  Offset = round(Offset, 5)
})

# Merge in the stimulus (video) information
setkey(all.data, "Segment")
setkey(vid.info, "Segment")
all.data <- vid.info[all.data]
# Subset the data to only include target stimuli
all.data <- subset(all.data, StimulusGroup == "target")

# Exclude subjects:
# Get the end times (durations) for each stimulus
endtimes <- aggregate(Offset ~ Segment, gap.info, max)
# Get a list of the unique subject-stimulus combinations
setkeyv(all.data, c("Segment", "Subject"))
subs.and.vids <- data.frame(unique(all.data[, list(Segment, Subject)]))
# Add a row "Offset" with the total possible looking time for each
# subject-stimulus combination
sub.looking <- aggregate(Offset ~ Subject, merge(subs.and.vids, endtimes), sum)
# Convert measurements into time by multiplying them by 125 (8ms samples/sec)
# Then multiply by minimum criteria (50% looking)
sub.looking$Offset <- (sub.looking$Offset*125)*0.5 # 50% looking time
# Merge with the number of measurements participants actually have for
# each subject-stimulus combination
sub.looking <- merge(sub.looking, data.frame(table(all.data$Subject)),
	by.x="Subject", by.y="Var1")
# Exclusions include the subjects who have fewer measurements
# than is required by the criteria (50% in the current case)
# Plus exclude one seven-year-old (outside age range)
exclusions <- c(subset(sub.looking, Freq < Offset)$Subject, "C-2_P24")
# Exclude the rows with those subjects
setkey(all.data, "Subject")
all.data <- all.data[!(Subject %in% exclusions),]

# Merge in turn sites, using onsets
all.data[, Onset := 0]
for (vid in unique(all.data$Segment)) {
    # For each TimeSecSeg measurement, find the onset of the segment
    # (from turn.info) within which it lies
    onsets <- turn.info[Segment == vid, Onset]
    # REMINDER: example use of findInterval:
    # findInterval(c(2,5,7,8), c(4,5,6,8,9,10)) => [1] 0 2 3 4
    # In this case it returns a list as long as all.data$TimeSecSeg, with
    # the index of the video onset that the each time measurement falls within
    # (e.g., t = 5, vid[2] and t=7, vid[3] in the above example)
    vid.onset.indices <- findInterval(
        all.data[Segment == vid, TimeSecSeg], onsets)
    # Maps these values onto the subset of times it was derived from
    all.data[Segment == vid, Onset := onsets[vid.onset.indices]] 
}
# Merge in turn.info data
setkeyv(all.data, c("Segment", "Onset"))
setkeyv(turn.info, c("Segment", "Onset"))
all.data <- turn.info[all.data]

# Collapse male-looks and female-looks into a single look-direction column
# F and M are each TRUE or FALSE (coerced to 1 or 0)
all.data[, Look.Dir := ifelse(
	F.looks > M.looks, "F", ifelse(
	M.looks > F.looks, "M", "0"))]

# Re-order the data table
setkeyv(all.data, c("Subject", "Segment", "Onset"))

# Smooth gaps in the tracker measurements	
all.data <- smoothLD(all.data)

# Write out the prepped data for input into the transition analysis
write.csv(all.data, paste(processed.data.path, "prepped.data.csv", sep=""), row.names=FALSE)


################################################################################
# Prepare randomized gap info files
################################################################################
# Read in the info files we need to create the random gaps
# Information for video stimuli (targets only)
vid.info <- subset(vid.info, StimulusGroup == "target")
vids <- unique(gap.info$Segment)
# List of gap durations
dur.data <- fread(paste(info.path,"duration.data.csv",sep=""))

# Remove uneeded rows from gap info
rem.cols <- c("SwitchType", "order", "notes")
gap.info[,(rem.cols) := NULL]

# Pre-set variables for the loop below
ngaps <- nrow(gap.info)
rem.cols.loop <- c("Onset", "Offset", "Duration")
NumRand <- 1:10#000 # Number of random gap files desired

# Make NumRand random versions of the gaps and save them as a numbered .csv
for (n in NumRand) {
	# Initialize an empty data table
	rand.info = data.table(
		Onset = rep(0, ngaps),
		Offset = rep(0, ngaps),
		Duration = rep(0, ngaps))
	# Make a copy of the gap info to shuffle and
	# remove the columns that we will replace
	gap.supp <- copy(gap.info)
	gap.supp[,(rem.cols.loop) := NULL]
	# Loop through the video stimuli, shuffling the gaps and turns in each
	curr.row <- 1
	for (vid in vids) {
		# Use find.rand.windows (above) to shuffle the gaps for the
		# current video (including "na" insertion rows)
		end <- max(gap.info[Segment == vid, Offset])
		gaps <- dur.data[Segment == vid, Duration]
		rand.gaps <- find.rand.windows(0,end,gaps)
		# Enter these new onset/offset/durations into the output data table
		curr.rows <- curr.row:(curr.row + nrow(rand.gaps)-1)
		rand.info[curr.rows, names(rand.info) := rand.gaps]
		curr.row <- curr.row+nrow(rand.gaps)
		# Find the utterances within this stimulus and shuffle them,
		# replacing them between the "na" insertion rows into gap.supp
		utt_idxs <- which(gap.supp$Segment == vid & gap.supp$Type != "na")
		utts_shuff <- gap.supp[utt_idxs,][sample(nrow(gap.supp[utt_idxs,])),]
		gap.supp[utt_idxs, names(gap.supp) := utts_shuff]
	}
	# Combine the shuffled onsets/offsets/durations with the shuffled gap info  
	new.durs <- cbind(rand.info, gap.supp)
    # Write it out
	write.csv(new.durs, paste(info.path,paste(
	    "GapInfoRandom", n, ".csv", sep=""),sep=""), row.names = FALSE)
}