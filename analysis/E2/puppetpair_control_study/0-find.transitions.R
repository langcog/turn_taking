# Returns a data frame of transition info (whether or not switches were made
# and, if so, when they started) for all subjects and transitions
grab.transition.info <- function(obs, fixation.window) {
    # Preset the total number of rows needed
    subs <- unique(obs$Subject)
    gaps <- sort(as.integer(unique(obs$GapNum)))
    N <- length(subs) * length(gaps)
    # Make the empty data frame
    proportion.data <- data.frame(Subject=rep(NA, N), Gap=rep(NA, N),
        Switch=rep(NA, N), Start.Time=rep(NA, N))
    # Iterate through data frame rows, filling in transition information for
    # each subject and gap
    df.row = 1
    # Grab a subject's data
    for (i in 1:length(subs)) {
        sub.data <- subset(obs, Subject == subs[i])
        # Grab data for a particular gap
        for (j in 1:length(gaps)) {
            # Find the transition information (is there a switch? where?)
            gap.transition <- find.transition(subset(sub.data,
                GapNum == gaps[j]), fixation.window)
            # Add the result to the data frame
            proportion.data[df.row,] <- c(as.character(subs[i]),
                gaps[j], gap.transition[1], gap.transition[2])
            # Move onto the next row in the ouput data frame
            df.row = df.row + 1
        }
    }
    return(proportion.data)
}


# Parent function, linking the results of fixation.check and
# find.transition.start together
find.transition <- function(obs, fixation.window) {
    if(nrow(obs) < 1) {
        return(list(NA,NA))
    }
    if(is.numeric(max(obs$TimeSec)) == FALSE) {
        return(list(NA,NA))
    }
    # Dummy minstart
    minstart <- "NA"
    # Search for a 100 msec fixation on the origin speaker during
    # the ANCHOR period
    anchor <- fixation.check(subset(obs, Anchor.Window == "ANCHOR"), "ANCHOR",
        fixation.window, minstart)
    minstart <- anchor[2]
    # Search for a 100 msec fixation on the response speaker during
    # the TRANSITION period
    transition <- fixation.check(subset(obs, Transition.Window == "TRANSITION"),
        "TRANSITION", fixation.window, minstart)
    # Return the results of the search
    if(anchor[1] == "Y" & transition[1] == "Y") {
    	# If you found fixations on the ANCHOR and TRANSITION, find the starting
        # point of the transition
        return(list(1, find.transition.start(obs, transition[2])))
    }
    else {
        # Otherwise there's no valid switch being made
        return(list(0,0))
    }
}


# Searches 100 msec windows of time for 90%+ fixation on a speaker (origin or
# responder) during a pre-set analysis window (ANCHOR or TRANSITION)
fixation.check <- function(obs, window.type, fixation.window, minstart) {
    # The maximum starting point for a 100 msec window
    maxstart <- max(obs$TimeSec) - fixation.window
    # Start value for the 90%+ looking criteria
    looking <- 0
    # Smallest possible time increment in the eye measurements
    ms.increment <- min(obs$MS.Increment)

    # For fixation checks on anchors...
    if(window.type == "ANCHOR") {
        # Set the start time for incrementing forward with 100 msec windows
        start <- min(obs$TimeSec)
        # Increment forward until there are no more 100 msec segments to check
        # or until 90%+ fixation is found
        while((start < maxstart) && (looking < 0.9)) {
            # Get the mean of looks to the origin speaker during the current 100 
            # msec window
            looks.origin <- obs$Looks.Origin[which(obs$TimeSec >= start &
                obs$TimeSec <= start + fixation.window)]
            if (length(looks.origin) > 0) {
                looks.origin[is.na(looks.origin)] <- 0
                # Reset "looking" to the current mean
                looking <- mean(looks.origin)
            } else {
                looking <- 0
            }
            # Increment forward to next minimum fixation window
            start = start + ms.increment
        }
        # Return the findings: was there a fixation or not?
        if (looking >= 0.9) {
        	# Return the current start time to help find fixation during the 
            # TRANSITION region (coming up next)
            return(list("Y", start))
        } else {
            return(list("N", "NA"))
        }
    }
    # For fixation checks on transitions...
    if(window.type == "TRANSITION") {
        # If there was no fixation during the ANCHOR region, don't continue
        if (minstart == "NA") {return(list("N","NA"))}

        # Start time for incrementing must be after the fixation on the origin
        # started (from the ANCHOR region)
        # Needs to be converted from the list format to numeric
        start <- as.numeric(minstart)
        # Increment forward until there are no more 100 msec segments to check
        # or until 90%+ fixation is found
        while((start < maxstart) && (looking < 0.9)) {
            # Get the mean of looks to the origin speaker during the current 100 
            # msec window
            looks.destination <- obs$Looks.Destination[which(
                obs$TimeSec >= start & obs$TimeSec <= start + fixation.window)]
            if (length(looks.destination) > 0) {
                looks.destination[is.na(looks.destination)] <- 0
                # Reset "looking" to the current mean
                looking <- mean(looks.destination)
            } else {
                looking <- 0
            }
            # Increment forward to next minimum fixation window
            start = start + ms.increment
        }
        # Return the findings: was there a fixation or not?
        if (looking >= 0.9) {
        	# Return the current start time to help find the beginning of the 
            # transition time (coming up next)
            return(list("Y", start))
        } else {
            return(list("N", "NA"))
        }
    }
}


# Working backwards from the start time of fixations to the responder, find the 
# last look to the origin speaker
find.transition.start <- function(obs, start) {
    # Create a subset of the rows: where the participant is looking at the
    # origin speaker before the fixation on the responder begins
    # (handed down from fixation.check)
    looks.to.origin <- subset(obs, Looks.Origin == "1" & TimeSec < start)
    # Find the latest time in that subset, and retrieve it's equivalent in 
    # Cumulative Time
    return(obs[which.max(looks.to.origin$TimeSec),]$Cumulative.Time)
}


# Creates a summary of the switch data using Cumulative Time to enable us to 
# easily plot and analyze cumulative switches over the transition window
cumulative.switches.by.cond <- function(obs) {
    # Create an empty output data frame
	obs <- subset(obs, Switch != "NA")
	conditions <- unique(obs$Condition)
	ages <- sort(unique(obs$Age))
	df.row <- 1
	increment <- 0.025
	maxtime.all <- max(obs$Start.Time)
	N <- ceiling(maxtime.all/increment)*length(ages)*length(conditions)
	cumulative.prob <- data.frame(TimeIncrement=rep(NA, N),
	    Proportion=rep(NA, N), Age=rep(NA, N), Condition=rep(NA, N))
    # Loops through the language groups
	for (i in 1:length(conditions)) {
	sub.data.c <- subset(obs, Condition == conditions[i])
    # Finds the max time needed
	maxtime <- max(sub.data.c$Start.Time)
		# Grab data for a particular age
		for (j in 1:length(ages)) {
			start <- 0
			total.looks <- nrow(sub.data.c[sub.data.c$Age == as.character(ages[j]),])
            # Excludes cases where no switch was made (Start.Time == 0.0)
			sub.data.a <- subset(sub.data.c, Age == ages[j] & Start.Time > 0)
            # Change value for adults
            agenum <- ifelse(ages[j] == "21", "Adult", as.character(ages[j]))
			cond <- as.character(sub.data.a[1,]$Condition)
            # For each increment (set above), find the proportion of the total 
            # who have made switches by that point in time
			while(start < maxtime) {
                prop.looks <- sum(sub.data.a$Start.Time < start) / total.looks
                cumulative.prob[df.row,] <- c(start, prop.looks, agenum, cond)
                # Record the proportion in the data frame and increment forward
				df.row = df.row + 1
				start = start + increment
			}
		}
	}
	return(cumulative.prob)
}