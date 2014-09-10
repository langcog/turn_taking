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

# Create 100 versions of the analyses with the random gap info files we made
# (using the random.runs.R script), then run this loop...
for (i in 1:100) {
    # Print an update
    print(paste("Working on random version ",i," now...", sep=""))
    # Start with a clean version of the data
    r.all.data <- all.data

    # Read in one version of the random gaps
    r.gap.info <- read.csv(paste(info.path,"GapInfoRandom",i,".csv", sep=""))
    r.gap.info = within(r.gap.info, {
        Onset = round(Onset, 3)
        Offset = round(Offset, 3)
    })
    
    # Merge in gap sites, using new onset values
    print(paste("--Merging info files--"))
    r.all.data$Onset <- rep(NA, nrow(r.all.data))
    for (vid in unique(r.all.data$Segment)) {
        # For each TimeSecSeg measurement, find the onset of the utterance
        # (from gap.info) within which it lies
        onsets <- r.gap.info$Onset[r.gap.info$Segment == vid]
        # REMINDER: example use of findInterval:
        # findInterval(c(2,5,7,8), c(4,5,6,8,9,10)) => [1] 0 2 3 4
        # In this case it returns a list as long as all.data$TimeSecSeg, with
        # the index of the video onset that the each time measurement falls within
        # (e.g., t = 5, vid[2] and t=7, vid[3] in the above example)
        vid.gap.indices <- findInterval(
            r.all.data$TimeSecSeg[r.all.data$Segment == vid], onsets)
        # Maps these values onto the subset of times it was derived from
        r.all.data$Onset[r.all.data$Segment == vid] <- onsets[vid.gap.indices]
    }
    # Add in gap.info data and re-order the main data frame
    r.all.data <- merge(r.all.data, r.gap.info, by = c("Segment", "Onset"))
    r.all.data <- r.all.data[order(r.all.data$Subject, r.all.data$Segment,
        r.all.data$TimeSecSeg),]
    
    # Merge in subject.info data
    r.all.data <- merge(r.all.data, subject.info, by = "Subject")
    r.all.data <- subset(r.all.data, include == "1") # excluding a 7-year-old

    # Collapse male-looks and female-looks into a single look-direction column
    # F and M are each TRUE or FALSE (coerced to 1 or 0)
    r.all.data$Look.Dir <- mapply(function(f, m) {
        if(f > m) {
            return("F")
        }
        else if(f < m) {
            return("M")
        }
        else{
            return(NA)
        }
    }, r.all.data$F.looks, r.all.data$M.looks)

    # Separate adult and child data for finding anticipatory switches
    # because adults have an assumed faster gaze planning time of 200 msec
    # compared to our assumed 333 msec for children
    r.all.data.A <- subset(r.all.data, AgeGroup == "ADU")
    r.all.data.C <- subset(r.all.data, AgeGroup == "CHI")
    rm(r.all.data)

    # Anticipatory switch identification variables
    fixation.window = 0.1
    # could change utt settings to "reaction" rather than "anticipation" values
    utt1.overlap.A = 0.2
    utt2.overlap.A = 0.2
    utt1.overlap.C = 0.333
    utt2.overlap.C = 0.333

    # Extract time windows for switch analysis and prepare new looking columns
    print(paste("--Getting windows--"))
    # For adults
    r.target.windows.proport.A <- get.windows.categorical(r.all.data.A,
        r.gap.info, utt1.overlap.A, utt2.overlap.A, fixation.window)
    r.target.windows.proport.A$Looks.Origin <- as.integer(
        r.target.windows.proport.A$Origin ==
        r.target.windows.proport.A$Look.Dir)
    r.target.windows.proport.A$Looks.Destination <- ifelse(((
        r.target.windows.proport.A$Origin == "F" &
        r.target.windows.proport.A$Look.Dir == "M") |
        (r.target.windows.proport.A$Origin == "M" &
        r.target.windows.proport.A$Look.Dir == "F")), 1, 0)
    # For children
    r.target.windows.proport.C <- get.windows.categorical(r.all.data.C,
        r.gap.info, utt1.overlap.C, utt2.overlap.C, fixation.window)
    r.target.windows.proport.C$Looks.Origin <- as.integer(
        r.target.windows.proport.C$Origin ==
        r.target.windows.proport.C$Look.Dir)
    r.target.windows.proport.C$Looks.Destination <- ifelse(((
        r.target.windows.proport.C$Origin == "F" &
        r.target.windows.proport.C$Look.Dir == "M") |
        (r.target.windows.proport.C$Origin == "M" &
        r.target.windows.proport.C$Look.Dir == "F")), 1, 0)

    # Identify anticipatory switches in the data and combine it again
    print(paste("--Grabbing transition data--"))
    r.trans.info.A <- grab.transition.info(r.target.windows.proport.A,
        fixation.window)
    r.trans.info.C <- grab.transition.info(r.target.windows.proport.C,
        fixation.window)
    r.trans.info <- rbind(r.trans.info.A, r.trans.info.C)

    # Merge gap and subject info back in for analysis
    print(paste("--Prepping csv--"))
    r.trans.info.gaps <- merge(r.trans.info, r.gap.info, by = "Gap")
    r.trans.info.subs <- merge(r.trans.info.gaps, subject.info, by = "Subject")
    r.trans.info.subs <- r.trans.info.subs[order(r.trans.info.subs$Subject,
        r.trans.info.subs$Segment, r.trans.info.subs$Onset),]

    # Combine random and transition based analysis window data
    switch.final <- r.trans.info.subs
    switch.final$SampleType <- rep("RANDOM", nrow(switch.final))

    # Add condition data back in for main analysis
    switch.final <- merge(switch.final, vid.info, by="Segment")

    # Take out short gaps (not equal in all conditions) #NOTE!# Add to paper.
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
    switch.final.coarse[which(switch.final.coarse$QType == "wh" &
        switch.final.coarse$Condition == "muffled"),]$Ttype <- "S"
    switch.final$Ttype <- switch.final$Type.x
    switch.final[which(switch.final$QType == "wh" &
        switch.final$Condition == "muffled"),]$Ttype <- "S"

    # Write out the data, which is now ready for plotting and statistical analysis
    # Save two versions, one with 1-year age and one with 2-year age bins
    write.csv(switch.final.coarse, paste(processed.data.path,
        "switch.final.coarse",i,".csv", sep=""))
    write.csv(switch.final, paste(processed.data.path, "switch.final",
        i,".csv", sep=""))

}

# Read in all the switch results from the random runs and combine them
files <- dir(processed.data.path, pattern="switch.final[0-9]+.csv")
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