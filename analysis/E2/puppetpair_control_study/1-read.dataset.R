rm(list = ls())
source("0-useful.R")
source("0-helper.R")

raw.data.path <- "tracker_data/"
info.path <- "info/"
processed.data.path <- "processed_data/"

files <- dir(raw.data.path, pattern="*.txt")
all.data <- data.frame()

# Loop through the files, grabbing the data we want
for (file.name in files) {
    # Print an update
	print(file.name)
	data <- read.data(raw.data.path, file.name)
    # Take only the columns we want for further analysis
	data <- subset(data, select = c("Time", "x.pos", "y.pos", "F.looks",
	    "M.looks", "Events", "Condition", "Subject", "TimeSec","TimeSecSeg",
	    "MS.Increment", "Stimulus", "Saccades", "Fixations"))
    # Bind it together
	all.data <- rbind(all.data, data)
}

# Rename one column
colnames(all.data)[12] = "Segment"

all.data <- subset(all.data, Segment != "")

# Save the imported data
write.csv(all.data, paste(processed.data.path,"imported.data.csv",sep=""), row.names=FALSE)