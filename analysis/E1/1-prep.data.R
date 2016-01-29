rm(list = ls())
source("0-helper.R")
source("0-make.random.gaps.R")
library(data.table)
library(bit64)
library(dplyr)

# Set paths
raw.data.path <- "tracker_data/" #"../../data/E1/tracker_data/"
info.path <- "info/" #"../../data/E1/info/"
processed.data.path <- "processed_data/" #"../../data/E1/processed_data/"

################################################################################
# Read in all the tracker data
################################################################################
# Initialize a data table for all the tracker data:
# Make a list of the raw tracker data files for importing
files <- dir(raw.data.path, pattern="*.txt")
# Set a maximum length for the data table
N <- length(files) * 43990 # max file length from tracker data
# Create the table
all.data <- data.table(
    L.looks = rep(FALSE, N),
    R.looks = rep(FALSE, N),
    Condition = rep("", N),
    Subject = rep("", N),
    TimeSec = rep(0, N),
    MS.Increment = rep(0, N))

# Loop through the tracker files, grabbing the
# data we want and adding it to the table
curr.row <- 1
for (file.name in files) {
	# Read in the data
	data <- read.data(raw.data.path, file.name)
	# Take only the columns we want for futher analysis
	curr.rows <- curr.row:(curr.row + nrow(data) - 1)
	data <- subset(data, select = c("L.looks","R.looks",
        "Condition", "Subject", "TimeSec", "MS.Increment"))
    # Add the new file to the large data table
    all.data[curr.rows, names(all.data) := data]
    curr.row <- curr.row + nrow(data)
}
# Remove the empty rows and re-sort
all.data <- all.data[1:(curr.row - 1),]
setkeyv(all.data, c("Subject", "TimeSec"))

# Save the imported data in a separate .csv
write.csv(all.data,
    paste(processed.data.path, "imported.data.csv", sep=""),
    row.names=FALSE)

################################################################################
# Merge the tracker data with supplementary information needed for analysis
################################################################################
# Read in supplemental data and prep
vid.info <- fread(paste(info.path, "VideoSegmentInfo.csv", sep=""))
setnames(vid.info, "VideoSegment", "Segment")

# Merge in the stimulus (video) information
all.data[, Onset := 0]
for (cond in unique(all.data$Condition)) {
    # For each TimeSec measurement, find the onset of the segment
    # (from vid.info) within which it lies
    onsets <- vid.info[Condition == cond, Onset]
    # REMINDER: example use of findInterval:
    # findInterval(c(2,5,7,8), c(4,5,6,8,9,10)) => [1] 0 2 3 4
    # In this case it returns a list as long as all.data$TimeSecSeg, with
    # the index of the video onset that the each time measurement falls within
    # (e.g., t = 5, vid[2] and t=7, vid[3] in the above example)
    onset.indices <- findInterval(
        all.data[Condition == cond, TimeSec], onsets)
    # Maps these values onto the subset of times it was derived from
    all.data[Condition == cond, Onset := onsets[onset.indices]]
}
setkeyv(all.data, c("Condition", "Onset"))
setkeyv(vid.info, c("Condition", "Onset"))
all.data <- data.frame(vid.info[all.data])
# Subset the data to only include target stimuli
all.data <- data.table(filter(all.data, StimulusGroup == "target") %>%
            select(-StimulusGroup, -Onset, -Offset))

# Exclude subjects:
# Get the end times (durations for each stimulus in each condition)
vid.info[, Duration := Offset - Onset]
endtimes <- aggregate(Duration ~ Segment,
    subset(vid.info, StimulusGroup == "target"), max)
# Get a list of the unique subject-stimulus combinations
setkeyv(all.data, c("Segment", "Subject"))
subs.and.vids <- data.frame(unique(all.data[, list(Segment, Subject)]))
# Add a row "Offset" with the total possible looking time for each
# subject-stimulus combination
sub.looking <- aggregate(Duration ~ Subject, merge(subs.and.vids, endtimes), sum)
# Convert measurements into time by multiplying them by 125 (8ms samples/sec)
# Then multiply by minumum criteria (50% looking)
sub.looking$Duration <- (sub.looking$Duration*125)*0.5
# Merge with the number of measurements participants actually have for
# each subject-stimulus combination
sub.looking <- merge(sub.looking, data.frame(table(all.data$Subject)),
    by.x="Subject", by.y="Var1")
# Exclusions include the subjects who have fewer measurements
# than is required by the criteria (50% in the current case)
exclusions <- c(subset(sub.looking, Freq < Duration)$Subject)
# Exclude the rows with those subjects
setkey(all.data, "Subject")
all.data <- all.data[!(Subject %in% exclusions),]

# Collapse left-looks and right-looks into a single look-direction column
# L and R are each TRUE or FALSE (coerced to 1 or 0)
all.data[, Look.Dir := ifelse(
    L.looks > R.looks, "L", ifelse(
    R.looks > L.looks, "R", "0"))]

# Re-order the data table
setkeyv(all.data, c("Subject", "Segment", "TimeSec"))

# Smooth gaps in the tracker measurements
all.data <- smoothLD(all.data)

# Write out the prepped data for input into the transition analysis
write.csv(all.data, paste(
    processed.data.path, "prepped.data.csv", sep=""),
    row.names=FALSE)
