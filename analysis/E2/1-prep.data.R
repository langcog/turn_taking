rm(list = ls())
source("0-helper.R")
source("0-make.random.gaps.R")
library(data.table)
library(bit64)
library(dplyr)

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
	F.looks = rep(FALSE, N),
	M.looks = rep(FALSE, N),
	Version = rep(0, N),
	Subject = rep("", N),
	TimeSec = rep(0, N),
	TimeSecSeg = rep(0, N),
	MS.Increment = rep(0, N),
	Segment = rep("", N))

# Loop through the tracker files, grabbing the
# data we want and adding it to the table
curr.row <- 1
for (file.name in files) {
	#print (file.name)
	# Read in the data
	data <- read.data(raw.data.path, file.name)
    # Take only the columns we want for further analysis
    curr.rows <- curr.row:(curr.row+nrow(data)-1)
	data <- subset(data, select = c("F.looks", "M.looks", "Version",
		"Subject", "TimeSec", "TimeSecSeg", "MS.Increment", "Stimulus"))
	setnames(data, "Stimulus", "Segment")
    # Add the new file to the large data table
    all.data[curr.rows, names(all.data) := data]
    curr.row <- curr.row+nrow(data)
}
# Remove the empty rows and re-sort
all.data <- all.data[1:(curr.row-1),]
setkeyv(all.data, c("Subject","Segment","TimeSecSeg"))

# Save the imported data in a separate .csv, if desired
write.csv(all.data, paste(
	processed.data.path,"imported.data.csv",sep=""),
	row.names=FALSE)


################################################################################
# Merge the tracker data with supplementary information needed for analysis
################################################################################
# Read in supplemental data and prep
vid.info <- fread(paste(info.path,"VideoSegmentInfo.csv",sep=""))

# Merge in the stimulus (video) information
setkey(all.data, "Segment")
setkey(vid.info, "Segment")
all.data <- data.frame(vid.info[all.data])
# Subset the data to only include target stimuli
all.data <- data.table(filter(all.data, StimulusGroup == "target") %>%
            select(-StimulusGroup, -Duration))

# Exclude subjects:
# Get the end times (durations) for each stimulus
endtimes <- vid.info[StimulusGroup=="target",] %>%
	select(Segment, Duration)
# Get a list of the unique subject-stimulus combinations
setkeyv(all.data, c("Segment", "Subject"))
subs.and.vids <- data.frame(unique(all.data[, list(Segment, Subject)]))
# Add a row "Offset" with the total possible looking time for each
# subject-stimulus combination
sub.looking <- aggregate(Duration ~ Subject, merge(subs.and.vids, endtimes), sum)
# Convert measurements into time by multiplying them by 125 (8ms samples/sec)
# Then multiply by minimum criteria (50% looking)
sub.looking$Duration <- (sub.looking$Duration*125)*0.5 # 50% looking time
# Merge with the number of measurements participants actually have for
# each subject-stimulus combination
sub.looking <- merge(sub.looking, data.frame(table(all.data$Subject)),
	by.x="Subject", by.y="Var1")
# Exclusions include the subjects who have fewer measurements
# than is required by the criteria (50% in the current case)
# Plus exclude one seven-year-old (outside age range)
exclusions <- c(subset(sub.looking, Freq < Duration)$Subject, "C-2_P24")
# Exclude the rows with those subjects
setkey(all.data, "Subject")
all.data <- all.data[!(Subject %in% exclusions),]

# Collapse male-looks and female-looks into a single look-direction column
# F and M are each TRUE or FALSE (coerced to 1 or 0)
all.data[, Look.Dir := ifelse(
	F.looks > M.looks, "F", ifelse(
	M.looks > F.looks, "M", "0"))]

# Re-order the data table
setkeyv(all.data, c("Subject", "Segment", "TimeSecSeg"))

# Smooth gaps in the tracker measurements	
all.data <- smoothLD(all.data)

# Write out the prepped data for input into the transition analysis
write.csv(all.data, paste(
	processed.data.path, "prepped.data.csv", sep=""),
	row.names=FALSE)
