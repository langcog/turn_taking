source("0-helper.R")
source("0-find.transitions.R")
library(data.table) # can upgrade to later version with Mavericks
library(bit64)

################################################################################
# Read in processed data and set paths and assumed saccadic RTs
################################################################################
# Set paths
raw.data.path <- "tracker_data/"
info.path <- "info/"
processed.data.path <- "processed_data/"

# Read in supplemental data and prepped data
vid.info <- fread(paste(info.path,"VideoSegmentInfo.csv",sep=""))
subject.info <- fread(paste(info.path,"SubjectInfo.csv",sep=""))
all.data <- fread(paste(processed.data.path,"prepped.data.csv",sep=""))

# Anticipatory switch identification variables:
# Fixation window
fixation.window = 0.1
# Saccadic RTs set for...
# Adults
utt1.overlap.A = 0.2
utt2.overlap.A = 0.2
# 1-year-olds
utt1.overlap.C1 = 0.33
utt2.overlap.C1 = 0.33
# 2-year-olds
utt1.overlap.C2 = 0.304
utt2.overlap.C2 = 0.304
# 3-year-olds
utt1.overlap.C3 = 0.278
utt2.overlap.C3 = 0.278
# 4-year-olds
utt1.overlap.C4 = 0.252
utt2.overlap.C4 = 0.252
# 5-year-olds
utt1.overlap.C5 = 0.226
utt2.overlap.C5 = 0.226
# 6-year-olds
utt1.overlap.C6 = 0.2
utt2.overlap.C6 = 0.2

################################################################################
# Main analysis function: get.switches
################################################################################
# Analyzes the input observations for:
# 1 - fixation on the prior speaker in the Anchor window
# 2 - fixation on the upcoming speaker in the Transition window
# 3 - If "y" for 1 and 2, when the first look away from the prior speaker
#     occurred after the initial fixation
# Returns a data table with the results for each subject and gap:
# Switch/No switch + Switch onset (or NA if no switch)
#
# Takes in:
# - A vector of indices - a sequence of integers representing which
#   runs should be done in a loop, e.g., 1:10 to do 10 runs, with i as 1-10
# - A "sample" value - 1 for random runs, anything else for a real run
#
get.switches <- function (indices, sample) {
	# loop through the runs given in the indices
    for (i in indices) {
        # Start with a clean copy of the data
        data <- copy(all.data)
        # Read in a version of the gaps consistent with the index and sample values
        # Set the colnames and round the onset/offset values
        if (sample == 1) {
	        gap.info <- fread(paste(info.path,"GapInfoRandom",i,".csv", sep=""))
			setcolorder(gap.info, order(names(gap.info)))
	    } else {
	    	gap.info <- fread(paste(info.path,"GapInfo.csv", sep=""))
	        rem.cols <- c("SwitchType", "order", "notes")
    	    gap.info[,(rem.cols) := NULL]
			setcolorder(gap.info, order(names(gap.info)))
	    }
        gap.info[, Onset := round(Onset,5)]
        gap.info[, Offset := round(Offset,5)]

        # Merge in random gap sites, using the onsets
        #print(paste("Merging info files"))
        data[, Onset := 0]
        for (vid in unique(data$Segment)) {
            # For each TimeSecSeg measurement, find the onset of the utterance
            # (from gap.info) within which it lies
            onsets <- gap.info[Segment == vid, Onset]
            # REMINDER: example use of findInterval:
            # findInterval(c(2,5,7,8), c(4,5,6,8,9,10)) => [1] 0 2 3 4
            # In this case it returns a list as long as all.data$TimeSecSeg, with
            # the index of the video onset that the each time measurement falls within
            # (e.g., t = 5, vid[2] and t=7, vid[3] in the above example)
        	vid.idx <- which(data$Segment == vid)
            vid.gap.indices <- findInterval(
            	data[vid.idx, TimeSecSeg], onsets)
            # Maps these values onto the subset of times it was derived from
            data[vid.idx, Onset := onsets[vid.gap.indices]]
        }
        # Merge in gap.info data
		setkeyv(data, c("Segment", "Onset"))
		setkeyv(gap.info, c("Segment", "Onset"))
		data <- gap.info[data]

        # Merge in subject.info data
        setkey(subject.info, "Subject")
        setkey(data, "Subject")
        data <- subject.info[data]
        
        # Re-order the data set
        setkeyv(data, c("Subject", "Segment", "TimeSecSeg"))

        # Remove uneeded columns
        rem.cols <- c("Row", "Offset.1")
        data[,(rem.cols) := NULL]
        
        # Separate adult and child data for finding anticipatory switches:
        # We assume that adults have faster gaze planning time (200 ms)
        # compared to younger children (see saccadic RT settings above).
        data.A <- subset(data, AgeGroup == "ADU")
        data.C1 <- subset(data, Age == 1)
        data.C2 <- subset(data, Age == 2)
        data.C3 <- subset(data, Age == 3)
        data.C4 <- subset(data, Age == 4)
        data.C5 <- subset(data, Age == 5)
        data.C6 <- subset(data, Age == 6)

        # Extract time windows for switch analysis and prepare looking columns
        # to origin (prior speaker) and destination (upcoming speaker) for all
        # subject age groups
        #print("Getting windows")
        # For adults
        target.windows.A <- get.windows.categorical(data.A,
            gap.info, utt1.overlap.A, utt2.overlap.A, fixation.window)
        target.windows.A[, Looks.Origin := as.integer(Origin == Look.Dir)]
        target.windows.A[, Looks.Destination := ifelse(
        	(Origin == "F" & Look.Dir == "M") |
        	(Origin == "M" & Look.Dir == "F"), 1, 0)]

        # For children ages 1 and 2
        target.windows.C1 <- get.windows.categorical(data.C1,
            gap.info, utt1.overlap.C1, utt2.overlap.C1, fixation.window)
        target.windows.C1[, Looks.Origin := as.integer(Origin == Look.Dir)]
        target.windows.C1[, Looks.Destination := ifelse(
        	(Origin == "F" & Look.Dir == "M") |
        	(Origin == "M" & Look.Dir == "F"), 1, 0)]

        # For children ages 3 and 4
        target.windows.C2 <- get.windows.categorical(data.C2,
            gap.info, utt1.overlap.C2, utt2.overlap.C2, fixation.window)
        target.windows.C2[, Looks.Origin := as.integer(Origin == Look.Dir)]
        target.windows.C2[, Looks.Destination := ifelse(
        	(Origin == "F" & Look.Dir == "M") |
        	(Origin == "M" & Look.Dir == "F"), 1, 0)]

        # For children ages 5 and 6
        target.windows.C3 <- get.windows.categorical(data.C3,
            gap.info, utt1.overlap.C3, utt2.overlap.C3, fixation.window)
        target.windows.C3[, Looks.Origin := as.integer(Origin == Look.Dir)]
        target.windows.C3[, Looks.Destination := ifelse(
        	(Origin == "F" & Look.Dir == "M") |
        	(Origin == "M" & Look.Dir == "F"), 1, 0)]

        # For children ages 1 and 2
        target.windows.C1 <- get.windows.categorical(data.C1,
            gap.info, utt1.overlap.C1, utt2.overlap.C1, fixation.window)
        target.windows.C1[, Looks.Origin := as.integer(Origin == Look.Dir)]
        target.windows.C1[, Looks.Destination := ifelse(
        	(Origin == "F" & Look.Dir == "M") |
        	(Origin == "M" & Look.Dir == "F"), 1, 0)]

        # For children ages 3 and 4
        target.windows.C2 <- get.windows.categorical(data.C2,
            gap.info, utt1.overlap.C2, utt2.overlap.C2, fixation.window)
        target.windows.C2[, Looks.Origin := as.integer(Origin == Look.Dir)]
        target.windows.C2[, Looks.Destination := ifelse(
        	(Origin == "F" & Look.Dir == "M") |
        	(Origin == "M" & Look.Dir == "F"), 1, 0)]

        # For children ages 5 and 6
        target.windows.C3 <- get.windows.categorical(data.C3,
            gap.info, utt1.overlap.C3, utt2.overlap.C3, fixation.window)
        target.windows.C3[, Looks.Origin := as.integer(Origin == Look.Dir)]
        target.windows.C3[, Looks.Destination := ifelse(
        	(Origin == "F" & Look.Dir == "M") |
        	(Origin == "M" & Look.Dir == "F"), 1, 0)]

        # For children ages 5 and 6
        target.windows.C4 <- get.windows.categorical(data.C4,
            gap.info, utt1.overlap.C4, utt2.overlap.C4, fixation.window)
        target.windows.C4[, Looks.Origin := as.integer(Origin == Look.Dir)]
        target.windows.C4[, Looks.Destination := ifelse(
        	(Origin == "F" & Look.Dir == "M") |
        	(Origin == "M" & Look.Dir == "F"), 1, 0)]

        # For children ages 5 and 6
        target.windows.C5 <- get.windows.categorical(data.C5,
            gap.info, utt1.overlap.C5, utt2.overlap.C5, fixation.window)
        target.windows.C5[, Looks.Origin := as.integer(Origin == Look.Dir)]
        target.windows.C5[, Looks.Destination := ifelse(
        	(Origin == "F" & Look.Dir == "M") |
        	(Origin == "M" & Look.Dir == "F"), 1, 0)]

        # For children ages 5 and 6
        target.windows.C6 <- get.windows.categorical(data.C6,
            gap.info, utt1.overlap.C6, utt2.overlap.C6, fixation.window)
        target.windows.C6[, Looks.Origin := as.integer(Origin == Look.Dir)]
        target.windows.C6[, Looks.Destination := ifelse(
        	(Origin == "F" & Look.Dir == "M") |
        	(Origin == "M" & Look.Dir == "F"), 1, 0)]


        # Identify anticipatory switches in the data and combine the groups again
        #print("Grabbing transition data")
        trans.info.A <- grab.transition.info(target.windows.A, fixation.window)
        trans.info.C1 <- grab.transition.info(target.windows.C1, fixation.window)
        trans.info.C2 <- grab.transition.info(target.windows.C2, fixation.window)
        trans.info.C3 <- grab.transition.info(target.windows.C3, fixation.window)
        trans.info.C4 <- grab.transition.info(target.windows.C4, fixation.window)
        trans.info.C5 <- grab.transition.info(target.windows.C5, fixation.window)
        trans.info.C6 <- grab.transition.info(target.windows.C6, fixation.window)
        trans.info <- rbind(trans.info.A, trans.info.C1, trans.info.C2,
        	trans.info.C3, trans.info.C4, trans.info.C5, trans.info.C6)

        # Merge gap and subject info back in for analysis
        #print("Prepping csv")
        # Gap info merge
        gap.info <- subset(gap.info, Gap != "na")
        gap.info$Gap <- as.numeric(gap.info$Gap)        
        setkey(trans.info, "Gap")
        setkey(gap.info, "Gap")
        trans.info <- gap.info[trans.info]

		# Subject info merge
        setkey(trans.info, "Subject")
        setkey(subject.info, "Subject")
        trans.info <- subject.info[trans.info]
        
        # Re-order data
        setkeyv(trans.info, c("Subject", "Segment", "Onset"))
        
        # Set column with sample type (Random vs. Transition)
        if (sample == 1) {
	        trans.info[, Sample := "RANDOM"]
	    } else {
	    	trans.info[, Sample := "TRANSITION"]
	    }
        
        # Merge video stimulus info back in
        setkey(trans.info, "Segment")
        setkey(vid.info, "Segment")
        trans.info <- vid.info[trans.info]
        
        # Limit the data set to transitions with gaps longer than 0.09 sec
        trans.info <- subset(trans.info, Duration > 0.09)
		
		# Create a column for the transition type (Ttype: question/statement)
		trans.info[, Ttype := Type]
		# WH-questions in the muffled condition sound like declaratives,
		# so their Ttype becomes "S" (statement)
		trans.info[QType == "wh" & Condition == "muffled", Ttype := "S"]
		
        # Write out the data, which is now ready for plotting and
        # statistical analysis; name according to index and sample value
        if (sample == 1) {
	        write.csv(trans.info, paste(processed.data.path,
	        	"switch.final.rand.", i, ".csv", sep=""), row.names=FALSE)
	    } else {
	    	write.csv(trans.info, paste(processed.data.path,
	        	"switch.final.csv", sep=""), row.names=FALSE)
	    }
    }
}
