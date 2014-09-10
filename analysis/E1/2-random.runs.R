rm(list = ls())
source("0-useful.R")
source("0-helper.R")
source("0-find.transitions.R")

raw.data.path <- "tracker_data/"
info.path <- "info/"
processed.data.path <- "processed_data/"

# Read in supplemental data and prep
vid.info <- read.csv(paste(info.path,"VideoSegmentInfo.csv",sep=""))
subject.info <- read.csv(paste(info.path,"SubjectInfo.csv",sep=""))

all.data <- read.csv(paste(processed.data.path,"r.all.data.csv",sep=""))

# Separate adult and child data for finding anticipatory switches
# because adults have an assumed faster gaze planning time of 200 msec
# compared to our assumed 333 msec for children
all.data.A <- subset(all.data, AgeGroup == "ADU")
all.data.C <- subset(all.data, AgeGroup == "CHI")

# Anticipatory switch identification variables
fixation.window = 0.1
# could change utt settings to "reaction" rather than "anticipation" values
utt1.overlap.A = 0.2
utt2.overlap.A = 0.2
utt1.overlap.C = 0.333
utt2.overlap.C = 0.333

# Create 100 versions of the analyses with the random gap info files we made
# (using the random.runs.R script), and then run this loop...
for  (i in 1:100) {
    # Print an update
    print(paste("Working on random version ",i," now...", sep=""))
    # Start with a clean version of the data
    r.all.data.A <- all.data.A
    r.all.data.C <- all.data.C

    # Read in one version of the random gaps
    r.gap.info <- read.csv(paste(info.path,"GapInfoRandom",i,".csv", sep=""))
    r.gap.info = within(r.gap.info, {
        Onset = round(Onset, 3)
        Offset = round(Offset, 3)
    })
    # Extract time windows for switch analysis and prepare new looking columns
    print(paste("--Getting windows--"))
    # For adults
    r.target.windows.proport.A <- get.windows.categorical(r.all.data.A,
        r.gap.info, utt1.overlap.A, utt2.overlap.A, fixation.window)
    r.target.windows.proport.A$Looks.Origin <- as.integer(
        r.target.windows.proport.A$Origin ==
        r.target.windows.proport.A$Look.Dir)
    r.target.windows.proport.A$Looks.Destination <- ifelse(((
        r.target.windows.proport.A$Origin == "L" &
        r.target.windows.proport.A$Look.Dir == "R") |
        (r.target.windows.proport.A$Origin == "R" &
        r.target.windows.proport.A$Look.Dir == "L")), 1, 0)
    # For children
    r.target.windows.proport.C <- get.windows.categorical(r.all.data.C,
        r.gap.info, utt1.overlap.C, utt2.overlap.C, fixation.window)
    r.target.windows.proport.C$Looks.Origin <- as.integer(
        r.target.windows.proport.C$Origin ==
        r.target.windows.proport.C$Look.Dir)
    r.target.windows.proport.C$Looks.Destination <- ifelse(((
        r.target.windows.proport.C$Origin == "L" &
        r.target.windows.proport.C$Look.Dir == "R") |
        (r.target.windows.proport.C$Origin == "R" &
        r.target.windows.proport.C$Look.Dir == "L")), 1, 0)

    # Identify anticipatory switches in the data and combine it again
    print(paste("--Grabbing transition data--"))
    r.trans.info.A <- grab.transition.info(r.target.windows.proport.A,
        fixation.window)
    r.trans.info.C <- grab.transition.info(r.target.windows.proport.C,
        fixation.window)
    r.trans.info <- rbind(r.trans.info.A, r.trans.info.C)

    # Merge gap and subject info back in for analysis
    print(paste("--Prepping csv--"))
    r.trans.info.gaps <- merge(r.trans.info, subset(r.gap.info,
        Condition == "A"), by = "Gap")
    r.trans.info.subs <- merge(r.trans.info.gaps, subject.info, by = "Subject")
    r.trans.info.subs <- r.trans.info.subs[order(r.trans.info.subs$Subject,
        r.trans.info.subs$Gap),]

    # Combine random and transition based analysis window data
    switch.final <- r.trans.info.subs
    switch.final$SampleType <- rep("RANDOM", nrow(switch.final))

    write.csv(switch.final, paste(processed.data.path,
        "switch.final",i,".csv", sep=""))
}

# Read in all the switch results from the random runs and combine them
files <- dir(processed.data.path, pattern="switch.final.+.csv")
rr.data <- data.frame()
for (file.name in files) {
    print(file.name)
    data <- read.csv(paste(processed.data.path, file.name, sep=""))
    rr.data <- rbind(rr.data, data)
}

# If the random runs are for the anticipatory window:
write.csv(rr.data, paste(processed.data.path,"random.runs.anticipatory.csv",sep=""))
# If the random runs are for the reactive window:
#write.csv(rr.data, paste(processed.data.path,"random.runs.reactive.csv",sep=""))