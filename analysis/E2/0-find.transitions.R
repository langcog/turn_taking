################################################################################
# Returns a data frame of transition info (whether or not switches were made
# and, if so, when they started) for all subjects and transitions in obs
grab.transition.info <- function(obs, fixation.window) {
    # Preset the total number of rows needed
    subs <- unique(obs$Subject)
    gaps <- sort(as.integer(unique(obs$GapNum)))
    N <- length(subs) * length(gaps)
    # Make an empty data frame
    proportion.data <- data.table(
    	Subject=rep("", N),
    	Gap=rep(0, N),
        Switch=rep(0, N),
        Start.Time=rep(0, N))
    # Iterate through data frame rows, filling in
    # transition information for each subject and gap
    df.row <- 1
    # Grab a subject's data
    for (sub in subs) {
        sub.data <- subset(obs, Subject == sub)
        # Grab data for a particular gap
        sub.gaps <- unique(sub.data$GapNum)
        for (gap in sub.gaps) {
            # Find the transition information. Returns two parts:
            # 1 - Was there a switch in the analysis window
            # 2 - If so, when in the window did the switch occur?
            gap.transition <- find.transition(subset(sub.data,
                GapNum == gap), fixation.window)
            # Add the transition info to the data frame
            proportion.data[df.row,] <- c(as.character(sub),
                gap, gap.transition[1], gap.transition[2])
            # Move onto the next row in the ouput data frame
            df.row = df.row + 1
        }
    }
    # Return the output data frame,
    # minus any remaining empty rows
    return(proportion.data[1:df.row])
}

################################################################################
# Called by grab.transition.info
# Parent function to fixation.check
# Sets up data to check for fixations and switch start times
# Returns the result to grab.transition.info
find.transition <- function(obs, fixation.window) {
	# In case there are no eye tracking measurements in this
	# particular analysis window
    if (nrow(obs) == 0) {
        return(list(NA,NA))
    }
    
    # Dummy minstart; will set later
    minstart <- "NA"
    
    # Find the subset of the total analysis window that is just
    # linked to the Anchor window
    anchorobs <- subset(obs, Anchor.Window == "ANCHOR")
    # If there is none, exit without success
    if (nrow(anchorobs) == 0) {
    	anchor <- c("N", "NA")
    } else { # Otherwise search for fixation on the
    	# prior speaker in this window
    	anchor <- fixation.check(anchorobs, "ANCHOR",
    	fixation.window, minstart)
    }
    
    # Minstart now becomes the result from the anchor search
    # This way we don't waste time looking earlier in the
    # analysis window than needed when finding fixations
    # in the Transition window (below)
    minstart <- anchor[2]

    # Find the subset of the total analysis window that is just
    # linked to the Transition window
    transitionobs <- subset(obs, Transition.Window == "TRANSITION")
    if (nrow(transitionobs) == 0) {
    	transition <- c("N", "NA")
    # If there is none, exit without success
    } else { # Otherwise search for fixation on the upcoming speaker
    	# in this window
   		transition <- fixation.check(transitionobs, "TRANSITION",
   		fixation.window, minstart)	
    }
    
    # Return the results of the search in the Anchor and Transition windows
    if (anchor[1] == "Y" && transition[1] == "Y") {
    	# If there were fixations in both the Anchor and Transition windows,
    	# find the starting point of the look away from the prior speaker
        switch.time <- max(obs[Looks.Origin == "1" &
        	TimeSec < transition[2], Cumulative.Time])
        return(list(1, switch.time))
    } else { # If there weren't fixations in both the Anchor and Transition
    	# windows, there was no valid switch
        return(list(0,0))
    }
}


################################################################################
# Subfunction of find.transition: Searches for fixations on a speaker that
# last at least 100ms and returns (a) whether there is such a fixation and
# (b) if so, when it started
fixation.check <- function(obs, window.type, fixation.window, minstart) {

    # For fixation checks on Anchor windows
    if (window.type == "ANCHOR") {
		# Dummy start values
		# (assumes no switch)
    	max.time <- NA
		yn <- "N"
		# Compile the looks to the previous
		# speaker into runs of values (1/NA/0) 
    	numruns <- rle(obs$Looks.Origin)
    	# Convert the runs into a data table that gives
    	# run length and start time
    	run.vals <- data.table(Length = numruns[['lengths']],
    		Value = numruns[['values']])
		run.vals[, Start := cumsum(Length)-Length+1]
    	# Find the runs looking at the previous speaker (val:1)
    	# that are at least 13 measurements long because:
    	# 13 * 8 ms (avg measurement interval) = 104ms
    	# Returned value is the timestamp of the last
    	# measurement in the run
		max.orig <- run.vals[Length >= 13 & Value == 1, Start+Length-1]
		# If there's at least one run that meets this criteria,
		# grab the time value for the last measurement at the
		# previous speaker in the first fixation longer than 100ms
		if (length(max.orig) > 0) {
			max.time <- obs[max.orig[1], TimeSec]
			yn <- "Y"
		}
		# Return this time along with
		# an affirmative code for fixation
		return(list(yn, max.time))
    }
    # For fixation checks on Transition windows
    if(window.type == "TRANSITION") {
        # If there was no fixation during
        # the ANCHOR region, don't continue
        if (minstart == "NA") {
        	return(list("N","NA"))
        }
		# Dummy start values
		# (assumes no switch)
    	min.time <- NA
		yn <- "N"
		# Only look at data from the minstart to the end of the window
		# (minstart is the end of the known fixation on the prior speaker
		# in the Anchor window, so we only need to look *after* that point
		# for a fixation on the upcoming speaker)
		sub.obs <- obs[TimeSec > minstart,]
		# Compile the looks to the previous
		# speaker into runs of values (1/NA/0) 
    	numruns <- rle(sub.obs$Looks.Destination)
    	run.vals <- data.table(Length = numruns[['lengths']],
    		Value = numruns[['values']])
		run.vals[, Start := cumsum(Length)-Length+1]
    	# Find the runs looking at the previous speaker (val:1)
    	# that are at least 13 measurements long because:
    	# 13 * 8 ms (avg measurement interval) = 104ms
    	# Returned value is the timestamp of the first
    	# measurement in the run
		min.dest <- run.vals[Length >= 12 & Value == 1, Start]
		# If there's at least one run that meets this criteria,
		# grab the time value for the first look at the
		# upcoming speaker in the first fixation longer than 100ms
		if (length(min.dest) > 0) {
			min.time <- sub.obs[min.dest[1], TimeSec]
			yn <- "Y"
		}
		# Return this time along with
		# an affirmative code for fixation
		return(list(yn, min.time))
    }
}