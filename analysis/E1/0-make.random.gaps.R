################################################################################
# Prepare randomized gap info files
################################################################################
make.rand.gaps <- function(ns) {
	NumRand <- ns # Number of random gap files desired
	# Read in the info files we need to create the random gaps
	vid.info <- fread(paste(info.path, "VideoSegmentInfo.csv", sep=""))
	vid.info <- subset(vid.info, StimulusGroup == "target")
	setnames(vid.info, "VideoSegment", "Segment")
	vids <- sort(unique(vid.info$Segment))
	conds <- unique(vid.info$Condition)
	
	# Round the time values for gaps
	gap.info <- fread(paste(info.path,"GapInfo.csv",sep=""))
	gap.info = within(gap.info, {
	  Onset = round(Onset, 5)
	  Offset = round(Offset, 5)
	})
	exclude.info <- distinct(select(gap.info, Gap, Exclude))
	
	# List of gap durations
	dur.data <- fread(paste(info.path, "duration.data.csv", sep=""))	
	N <- nrow(dur.data) * 2 + length(vids)
	
	# Remove unneeded columns from gap info
	rem.cols <- c("SwitchType", "notes")
	gap.info[, (rem.cols) := NULL]
	setkeyv(gap.info, c("Condition", "SegInclBuffer", "Onset"))
	
	# Pre-set variables for the loop below
	ngaps <- nrow(gap.info)
	rem.cols.loop <- c("Onset", "Offset", "Duration", "Segment", "Gap")
	
	# Make NumRand random versions of the gaps and save them as a numbered .csv
	for (i in NumRand) {
	    # Randomize gap order and onset for each video
	    # while inserting buffer rows between gaps
		new.durs <- data.table(
	    Onset=rep(0, N),
	    Offset=rep(0, N),
	    Duration=rep(0, N),
	    Gap=rep(0, N),
	    Segment=rep("", N))
	
	    row = 1
	    for (vid in vids) {
	    	start <- min(gap.info[Condition == "A" & Segment == vid, Onset])
	    	end <- max(gap.info[Condition == "A" & Segment == vid, Offset])
	        vid.dur <- end - start
	        new.rows <- length(which(dur.data$Segment == vid))*2
	        new.durs[row:(row + new.rows),] <- find.rand.windows(0, vid.dur,
	            subset(dur.data, Segment == vid))
	        row <- row + new.rows + 1
	    }
	
		# Initialize an empty data table
		rand.info = data.table(
		    Onset = rep(0, ngaps),
		    Offset = rep(0, ngaps),
		    Duration = rep(0, ngaps),
		    Gap = rep(0, ngaps),
		    Segment = rep("", ngaps))
	
		# Retrieve pre-stimulus buffer times
		buffers <- gap.info[!is.na(SegInclBuffer) & is.na(Segment),]
			
		# Loop through the video stimuli, shuffling the gaps and turns in each
		curr.row <- 1
		for (cond in conds) {
		    cond.durs <- copy(new.durs)
		    for (vid in vids) {
		    	# Use find.rand.windows to shuffle the gaps for the
		    	# current video (including "na" insertion rows)
	            # Start with the buffer time (pre-stimulus filler)
		    	buffer.row <- buffers[Condition == cond & SegInclBuffer == vid,
		    	    c("Onset", "Offset", "Duration", "Gap", "Segment"), with=FALSE]
		    	buffer.row[, Gap := as.numeric(Gap)]
	            # And then grab the rows from the real stimulus
		    	start <- buffer.row$Offset
		    	cond.durs[Segment == vid, `:=`
		    	    (Onset = Onset + start, Offset = Offset + start)]
		    	ins.rows <- bind_rows(buffer.row, cond.durs[Segment == vid,])
		    	rand.info[curr.row:(curr.row + nrow(ins.rows) - 1), `:=` (
		    	    Onset = ins.rows$Onset, Offset = ins.rows$Offset,
		    	    Duration = ins.rows$Duration, Gap = ins.rows$Gap,
		    	    Segment = ins.rows$Segment)]
		    	curr.row <- curr.row + nrow(ins.rows)
		    }
		}
		# Add other information, re-order it, and write it out
		row.supp <- gap.info[,c("SegInclBuffer", "LgGroup", "Condition"), with=FALSE]
		final.durs <- data.table(bind_cols(rand.info, row.supp)) %>%
		    left_join(exclude.info, by = "Gap") %>%
		    arrange(Condition, Onset) %>%
		    mutate(order = 1:nrow(rand.info))
		 
		write.csv(final.durs, paste(info.path, paste(
		    "GapInfoRandom", i, ".csv", sep=""), sep=""),
		    row.names = FALSE)
	}
}

################################################################################
# Helper functions
################################################################################

# Takes in a vector of gap durations associated with each stimulus and returns a 
# data frame of with the same set of gap durations, only shuffled at random 
# within the time boundaries of the associated stimulus.
find.rand.windows <- function(min, max, durs) {
    # Number of gaps being shuffled
    N <- length(durs$Gap)
    seg <- durs$Segment[1]
    # Set the maximum stop time for a gap as
    # 0.433 sec before the end of the stimulus
    give.win <- 0.433
    # Initialize a data frame for the new gaps
    new.windows <- data.frame(
        Onset=rep(NA, N),
        Offset=rep(NA, N),
        Duration=rep(NA, N),
        Gap=rep(0,N))
    # Iterate through the gaps, filling in the new
    # information for each gap duration one-by-one
    df.row = 1
    # For each gap...
    for (i in 1:N) {
        success = 0
        # Until you've found a successful random placement,
        while(success < 1) {
        	# The new gap will have the same duration as the old one
            dur = durs$Duration[i]
            gap = durs$Gap[i]
            # Generate a random start value within the video stimulus
            try.start <- runif(1, min + give.win, max - give.win - dur)
            # If this is the first start value for the new.windows
            # data frame, just fill it in
            if (is.numeric(new.windows[1,1]) == FALSE) {
            	# Add the randomized gap to the data frame
                new.windows[df.row,] <- c(try.start, try.start + dur, dur, gap)
                # Move onto the next row
                df.row = df.row + 1
                # Successul placement, move on to next iteration
                success = 1
            }
            # Otherwise
            else {
                # Check to see if the new proposed random gap
                # overlaps with one we have already:
                #      Create a mini data frame with the old and
                #      new (proposed) gaps
                try.win <- c(try.start, try.start + dur)
                used.win <- subset(new.windows[,1:2], Onset != "NA")
                temp.win <- rbind(try.win, used.win)
                #     Order mini data frame by onset
                #     (crucial for the check.overlap function)
                temp.win <- temp.win[order(temp.win$Onset),]
                # If there's no overlap with existing gaps,
                # add this new random gap to the data frame
                if (check.overlap(temp.win) == 0) {
                    new.windows[df.row,] <- c(try.start, try.start + dur, dur, gap)
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
    return(cbind(fill.in(min, max, new.windows),
        data.frame(Segment = rep(seg, (nrow(new.windows)*2+1)))))
}


# Takes in a set of onsets and offsets and checks to see if they overlap
check.overlap <- function(vec) {
    overlap = 0
    max = nrow(vec) - 1
    # Check row-by-row
    for(i in 1:max) {
        # Tests to see if the offsets are ordered; if not, there is an overlap
        if(vec[i + 1, 1] - vec[i, 2] < 0) {
            overlap = 1
        }
    }
    return(overlap)
}


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
			    Duration=end.val-min, Gap=NA)
			timesvec <- rbind(ins.row, timesvec)
			# Renumber row names
			row.names(timesvec)<-1:nrow(timesvec)
            # Move forward to the next row insertion
			row <- row + 2
		}
        # For the last row:
        # Insert a row between the end of the
        # last gap and the end of the stimulus
        else if (row == stop.row - 1) {
        	start.val <- timesvec$Offset[row - 1]
        	ins.row <- data.frame(Onset=start.val, Offset=max,
        	    Duration =max-start.val, Gap=NA)
        	timesvec <- rbind(timesvec, ins.row)
        	row.names(timesvec) <- 1:nrow(timesvec)
        	# Move forward to end the loop
        	row <- row + 1
        }
        # For everything else:
        # Insert a row between the end of the prior
        # gap and the start of the next one
		else {
			start.val <- timesvec$Offset[row - 1]
			end.val <- timesvec$Onset[row]
			ins.row <- data.frame(Onset=start.val, Offset=end.val,
			    Duration=end.val-start.val, Gap=NA)
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
