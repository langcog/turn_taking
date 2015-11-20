source("0-helper.R")
source("0-make.random.gaps.R")
library(data.table) # can upgrade to later version with Mavericks
library(bit64)
library(dplyr)

################################################################################
# Read in processed data and set paths and assumed saccadic RTs
################################################################################
# Set paths
raw.data.path <- "tracker_data/"
info.path <- "info/"
processed.data.path <- "processed_data/"

# Read in supplemental data and prepped data
real.gap.info <- fread(paste(info.path,"GapInfo.csv", sep=""))
spk.prevs <- distinct(select(real.gap.info, Gap, SpeakerPrev))
vid.info <- fread(paste(info.path,"VideoSegmentInfo.csv",sep=""))
subject.info <- fread(paste(info.path,"SubjectInfo.csv",sep=""))
age.info <- select(subject.info, Subject, Age)
all.data <- fread(paste(processed.data.path,"prepped.data.csv",sep=""))

# Re-order the data set
setkeyv(all.data, c("Subject", "Segment", "TimeSecSeg"))

# Merge in age data
setkey(age.info, "Subject")
setkey(all.data, "Subject")
all.data <- age.info[all.data]

# Separate adult and child data for finding anticipatory switches:
# We assume that adults have faster gaze planning time (200 ms)
# compared to younger children (see saccadic RT settings above).
all.data.A <- filter(all.data, Age == 21)
all.data.C1 <- filter(all.data, Age == 1)
all.data.C2 <- filter(all.data, Age == 2)
all.data.C3 <- filter(all.data, Age == 3)
all.data.C4 <- filter(all.data, Age == 4)
all.data.C5 <- filter(all.data, Age == 5)
all.data.C6 <- filter(all.data, Age == 6)

# Anticipatory switch identification variables:
fix.window <- 0.1
# Saccadic RTs set for...
saccadeRTs <- c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
# 1-year-olds
utt.overlap.C1 = saccadeRTs[1]
# 2-year-olds
utt.overlap.C2 = saccadeRTs[2]
# 3-year-olds
utt.overlap.C3 = saccadeRTs[3]
# 4-year-olds
utt.overlap.C4 = saccadeRTs[4]
# 5-year-olds
utt.overlap.C5 = saccadeRTs[5]
# 6-year-olds
utt.overlap.C6 = saccadeRTs[6]
# Adults
utt.overlap.A = saccadeRTs[7]

################################################################################
# Main analysis function: get.switches
################################################################################
get.switches <- function (ns, random = c(FALSE, TRUE)) {

    for (run in ns) {

        # Copy the by-age data tables
        data.C1 <- copy(all.data.C1)
        data.C2 <- copy(all.data.C2)
        data.C3 <- copy(all.data.C3)
        data.C4 <- copy(all.data.C4)
        data.C5 <- copy(all.data.C5)
        data.C6 <- copy(all.data.C6)
        data.A <- copy(all.data.A)
        
        # Read in a version of the gaps consistent with the index and sample values
        # Set the colnames and round the onset/offset values
        if (random == TRUE) {
	        gap.info <- fread(paste(info.path,"GapInfoRandom",run,".csv", sep=""))
	        gap.info <- left_join(gap.info, spk.prevs, by = "Gap") %>%
	            arrange(order)
			setcolorder(gap.info, order(names(gap.info)))
	    } else {
	    	gap.info <- copy(real.gap.info)
	        rem.cols <- c("SwitchType", "notes")
    	    gap.info[,(rem.cols) := NULL]
	        gap.info <- arrange(gap.info, order)
			setcolorder(gap.info, order(names(gap.info)))
	    }
        gap.info[, Onset := round(Onset,5)]
        gap.info[, Offset := round(Offset,5)]
        gap.info[, Gap := as.numeric(Gap)]

        # Extract time windows for switch analysis and prepare looking columns
        # to origin (prior speaker) and destination (upcoming speaker) for all
        # subject age groups
        # For 1-year-olds
        target.windows.C1 <- get.windows.categorical(data.C1,
            gap.info, utt.overlap.C1, fix.window)  %>%
        mutate(Looks.Origin = Origin == Look.Dir, 
            Looks.Destination = ifelse(((Origin == "F" & Look.Dir == "M") |
                                       (Origin == "M" & Look.Dir == "F")),
                                       TRUE, FALSE))
        # For 2-year-olds
        target.windows.C2 <- get.windows.categorical(data.C2,
            gap.info, utt.overlap.C2, fix.window)  %>%
        mutate(Looks.Origin = Origin == Look.Dir, 
            Looks.Destination = ifelse(((Origin == "F" & Look.Dir == "M") |
                                       (Origin == "M" & Look.Dir == "F")),
                                       TRUE, FALSE))
        # For 3-year-olds
        target.windows.C3 <- get.windows.categorical(data.C3,
            gap.info, utt.overlap.C3, fix.window)  %>%
        mutate(Looks.Origin = Origin == Look.Dir, 
            Looks.Destination = ifelse(((Origin == "F" & Look.Dir == "M") |
                                       (Origin == "M" & Look.Dir == "F")),
                                       TRUE, FALSE))
        # For 4-year-olds
        target.windows.C4 <- get.windows.categorical(data.C4,
            gap.info, utt.overlap.C4, fix.window)  %>%
        mutate(Looks.Origin = Origin == Look.Dir, 
            Looks.Destination = ifelse(((Origin == "F" & Look.Dir == "M") |
                                       (Origin == "M" & Look.Dir == "F")),
                                       TRUE, FALSE))
        # For 5-year-olds
        target.windows.C5 <- get.windows.categorical(data.C5,
            gap.info, utt.overlap.C5, fix.window)  %>%
        mutate(Looks.Origin = Origin == Look.Dir, 
            Looks.Destination = ifelse(((Origin == "F" & Look.Dir == "M") |
                                       (Origin == "M" & Look.Dir == "F")),
                                       TRUE, FALSE))
        # For 6-year-olds
        target.windows.C6 <- get.windows.categorical(data.C6,
            gap.info, utt.overlap.C6, fix.window)  %>%
        mutate(Looks.Origin = Origin == Look.Dir, 
            Looks.Destination = ifelse(((Origin == "F" & Look.Dir == "M") |
                                       (Origin == "M" & Look.Dir == "F")),
                                       TRUE, FALSE))
        # For adults
        target.windows.A <- get.windows.categorical(data.A,
            gap.info, utt.overlap.A, fix.window)  %>%
        mutate(Looks.Origin = Origin == Look.Dir, 
            Looks.Destination = ifelse(((Origin == "F" & Look.Dir == "M") |
                                       (Origin == "M" & Look.Dir == "F")),
                                       TRUE, FALSE))
                                       
        # Identify anticipatory switches in the data and combine the groups again
        trans.info <- bind_rows(grab.transition.info(target.windows.C1, fix.window), 
                          grab.transition.info(target.windows.C2, fix.window),
                          grab.transition.info(target.windows.C3, fix.window),
                          grab.transition.info(target.windows.C4, fix.window),
                          grab.transition.info(target.windows.C5, fix.window),
                          grab.transition.info(target.windows.C6, fix.window),
                          grab.transition.info(target.windows.A, fix.window)) %>%
                          arrange(Subject, Gap)
        
        # Write out the data; name according to index and sample value
        if (random == TRUE) {
	        write.csv(trans.info, paste(processed.data.path,
	        	"switch.final.rand.", run, ".csv", sep=""), row.names=FALSE)
	    } else {
	    	write.csv(trans.info, paste(processed.data.path,
	        	"switch.final.csv", sep=""), row.names=FALSE)
	    }
    }
}

################################################################################
# Sub-functions
################################################################################
# Retrieve samples from analysis-relevant time windows:
# This function adds annotations to those measurements that are
# within the analysis windows
# |--------A----|-|                     # Speaker A's turn
#                      |-|----B-----|   # Speaker B's turn
# Anchor:      |--|
# Transition:   |---------|
# Gap:            |----|
get.windows.categorical <- function(obs, gap.info, utt.window, fixation.window) {

  # Get rid of gaps under 90ms and over 550 ms (marked as 0 in the spreadsheet)
  vids.used <- distinct(select(obs, Segment))
  gaps <- gap.info %>% 
    filter(Gap != "NA", Exclude == 0, Segment != "NA") %>%
    right_join(vids.used, by = "Segment") %>%
	arrange(order)
    
  # shuffle and then rearrange
  df.out <- gaps %>% 
    group_by(order) %>% 
    do(get.gap(.$Segment, .$Onset, .$Offset, .$SpeakerPrev, .$Gap,
               obs, utt.window, fixation.window)) %>%
    arrange(Subject, order, Cumulative.Time)     
  
   return(df.out)
}



# Helper function for get.windows.categorical
get.gap <- function(segment, onset, offset, speakerprev, gap,
                    obs, utt.window, fixation.window) {
  obs %>% 
    filter(Segment == segment,
           (TimeSecSeg >= onset - utt.window - fixation.window), 
           (TimeSecSeg <= offset + utt.window + fixation.window)) %>%
    mutate(Anchor.Window = TimeSecSeg >= onset - utt.window - fixation.window &
             TimeSecSeg <= onset,
           Transition.Window = TimeSecSeg >= onset - utt.window &
             TimeSecSeg <= offset + utt.window + fixation.window,
           Gap.Window = TimeSecSeg >= onset & TimeSecSeg <= offset,
           Origin = speakerprev,
           Gap = gap,
           Cumulative.Time = round(TimeSecSeg - min(TimeSecSeg),3))
}   



# Checks for transitions in analysis windows
# Note: makes use of linear filter that depends on equal spacing of points
# .0083 msec * 12 = 100 msec, of which > 90 msec need to be looks. 
# Note: GapNum is a factor, so we cast it back to numeric. 
grab.transition.info <- function(obs, fixation.window) {
  obs %>%
    group_by(Subject, Gap) %>%
    mutate(
      iow = Looks.Origin * MS.Increment * Anchor.Window,
      idt = Looks.Destination * MS.Increment * Transition.Window,
      anchor.filter = apply.filter(iow),
      transition.filter = apply.filter(idt),
      transition.times = transition.filter > .09 & !is.na(transition.filter)) %>%
    group_by(Subject, Gap) %>%
    summarise(
      Anchor = any(anchor.filter > .09, na.rm = TRUE),
      Transition = any(transition.filter > .09, na.rm = TRUE),
      Transition.Time = Cumulative.Time[transition.times][1],
      Start = conditional.max(Cumulative.Time[
        Looks.Origin & Cumulative.Time < Cumulative.Time[transition.times][1]])) %>%
    ungroup() %>%
    mutate(Switch = ifelse(Start == 0, FALSE, Anchor & Transition)) %>%
    select(Subject, Gap, Switch, Start) %>%
    arrange(Gap)
}



# Helper functions for grab.transition.info:
# Ensures the filter isn't applied to too-small time series
apply.filter <- function(ns) {
  if (length(ns) >= 12) {
    as.numeric(stats::filter(ns, filter = rep(1, 12), side = 1))
  } else {
    rep(0, length(ns))
  }
}
# Returns a maximum value for non-empty non-NA vectors and 0 otherwise
conditional.max <- function(ns) {
	ns = ns[!is.na(ns)]
	if (length(ns) > 0) {
		max(ns)
	} else {
		0
	}
}
