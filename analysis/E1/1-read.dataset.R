rm(list = ls())
source("0-useful.R")
source("0-helper.R")

raw.data.path <- "../../data/E1/tracker_data/"
info.path <- "../../data/E1/info/"
processed.data.path <- "../../data/E1/processed_data/"

files <- dir(raw.data.path, pattern="*.txt")
all.data <- data.frame()

# Loop through the files, grabbing the data we want
for (file.name in files) {
    # Print an update
    print(file.name)
    data <- read.data(raw.data.path, file.name)
    # Take only the columns we want for further analysis
    data <- subset(data, select = c("Time", "x.pos", "y.pos", "L.looks",
        "R.looks", "Events", "Condition", "Subject", "AgeGroup", "TimeSec",
        "MS.Increment", "Stimulus", "Saccades", "Fixations"))
    # Bind it together
    all.data <- rbind(all.data, data)
}

# Save the imported data
write.csv(all.data, paste(processed.data.path,"imported.data.csv",sep=""))