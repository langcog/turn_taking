################################################################################
# Takes in a vector of gap durations associated with each stimulus and returns 
# a data.table with the same set of gap durations, only now shuffled into 
# random time points within each stimulus. The output is a set of gaps with the
# same durations, but whose onsets have been shuffled throughout the stimulus,
# with no overlaps between gaps
find.rand.windows <- function(min, max, vecdur) {
	# Number of gaps being shuffled
    N <- length(vecdur)
    # Set the maximum stop time for a gap as
    # 0.433 msec before the end of the stimulus
    give.win <- 0.433
    # Initialize a data frame for the new gaps
    new.windows <- data.frame(
    	Onset=rep(NA, N),
    	Offset=rep(NA, N),
        Duration=rep(NA, N))
    # Iterate through the gaps, filling in the new
    # information for each gap duration one-by-one
    df.row = 1
    # For each gap...
    for (i in 1:length(vecdur)) {
        success=0
        # Until you've found a successful random placement,
        while(success < 1) {
        	# The new gap will have the same duration as the old one
            dur = vecdur[i]
            # Generate a random start value within the video stimulus
            try.start <- runif(1, min + give.win, max - give.win - dur)
            # If this is the first start value for the new.windows
            # data frame, just fill it in:
            if (is.numeric(new.windows[1,1]) == FALSE) {
            	# Add the randomized gap to the data frame
                new.windows[df.row,] <- c(try.start, try.start + dur, dur)
                # Move onto the next row
                df.row = df.row + 1
                # Successful placement, move on to next iteration
                success = 1
            }
            # Otherwise
            else {		
                # Check to see if the new proposed random gap
                # overlaps with one we have already:
                #    Create a mini data frame with the old and new (proposed) gaps 
                try.win <- c(try.start, try.start + dur)
                used.win <- subset(new.windows[,1:2], Onset != "NA")
                temp.win <- rbind(try.win, used.win)
                #    Order mini data frame by onset (crucial for check.overlap)
                temp.win <- temp.win[order(temp.win$Onset),]
                # If there's no overlap with existing gaps,
                # add this new random gap to the data frame
                if (check.overlap(temp.win) == 0) {
                    new.windows[df.row,] <- c(try.start, try.start + dur, dur)
                    new.windows <- new.windows[order(new.windows$Onset),]
                    df.row = df.row + 1
                    success = 1
                }
                # Otherwise try again
                else {
                    success = 0		
                    #print(paste("failed index: ", i, " of ", N))	
                }
            }
        }
    }
    # Return newly shuffled gaps, filling in the space between
    # with "na" gap rows (matches the original gap info)
    return(fill.in(min, max, new.windows))
}


################################################################################
# Takes in a set of onsets and offsets and checks to see if they overlap
check.overlap <- function(vec) {
    overlap = 0
    max=nrow(vec)-1
    # Check row-by-row
    for(i in 1:max) {
        # Tests to see if the offsets are ordered; if not, there is an overlap
        if(vec[i+1,1]-vec[i,2] < 0) {
            overlap = 1
        }
    }
    return(overlap)
}


################################################################################
# Inserts "na" rows between each gap row to match the original gap info;
# this format is assumed in the way other functions are designed
fill.in <- function(min, max, timesvec) {
	row = 1
    # The total number of rows that will be needed
	stop.row <- nrow(timesvec)*2+2
	while (row < stop.row) {
        # For the first row:
		# Insert a row between the start of
		# the stimulus and the first gap
		if (row == 1) {
			end.val <- timesvec$Onset[row]
			ins.row <- data.frame(Onset=min, Offset=end.val,
			    Duration=end.val-min)
			timesvec <- rbind(ins.row, timesvec)
			# Renumber row names
			row.names(timesvec)<-1:nrow(timesvec)
            # Move forward to the next row insertion
			row <- row + 2
		}
        # For the last row:
		# Insert a row between the end of the 
		# last gap and the end of the stimulus
		else if (row == stop.row-1) {
			start.val <- timesvec$Offset[row-1]
			ins.row <- data.frame(Onset=start.val, Offset=max,
			    Duration=max-start.val)
			timesvec <- rbind(timesvec,ins.row)
			row.names(timesvec)<-1:nrow(timesvec)
            # Move forward to end the loop
			row <- row + 1
		}
        # For everything else:
		# Insert a row between the end of the prior
		# gap and the start of the next one
		else {
			start.val <- timesvec$Offset[row-1]
			end.val <- timesvec$Onset[row]
			ins.row <- data.frame(Onset=start.val, Offset=end.val,
			    Duration=end.val-start.val)
			timesvec <- rbind(timesvec[1:row-1,], ins.row,
			    timesvec[row:nrow(timesvec),])
			# Renumber row names
			row.names(timesvec)<-1:nrow(timesvec)
            # Move forward to the next row insertion
			row <- row + 2
			
		}
		
	}
	return(timesvec)
}