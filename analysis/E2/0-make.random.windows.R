# Takes in a vector of gap durations associated with each stimulus and returns a 
# data frame of with the same set of gap durations, only shuffled at random 
# within the time boundaries of the associated stimulus.
find.rand.windows <- function(min, max, vecdur) {
    N <- length(vecdur)
    # Maximum start time for a gap is .433 msec before the end of the stimulus
    give.win <- 0.433
    # Create a new, empty data frame
    new.windows <- data.frame(Onset=rep(NA, N), Offset=rep(NA, N),
        Duration=rep(NA, N))
    # Iterate through new.windows, filling in information for each gap duration
    df.row = 1
    for (i in 1:length(vecdur)) {
        success=0
        while(success < 1) {
            dur = vecdur[i]
            # Generate a random start value within the video stimulus
            try.start <- runif(1, min + give.win, max - give.win - dur)
            # Fill in the first value
            if (is.numeric(new.windows[1,1]) == FALSE) {
            	# Add the randomized gap to the data frame
                new.windows[df.row,] <- c(try.start, try.start + dur, dur)
                # Move onto the next row
                df.row = df.row + 1
                success = 1
            }
            # For the second row and beyond...
            else {		
                try.win <- c(try.start, try.start + dur)
                # Check to see if the new proposed random gap overlaps with
                # one we have already
                used.win <- subset(new.windows[,1:2], Onset != "NA")
                temp.win <- rbind(try.win, used.win)
                # Orders the accepted and proposed random gaps by onset
                # (crucial for the check.overlap function)
                temp.win <- temp.win[order(temp.win$Onset),]
                # If it doesn't, add it to the data frame
                if (check.overlap(temp.win) == 0) {
                    new.windows[df.row,] <- c(try.start, try.start + dur, dur)
                    new.windows <- new.windows[order(new.windows$Onset),]
                    df.row = df.row + 1
                    success = 1
                }
                # If it does, announce it, and try again
                else {
                    success = 0		
                    print(paste("failed index: ", i, " of ", N))	
                }
            }
        }
    }
    return(fill.in(min, max, new.windows))
}


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


# Inserts rows between each "gap" onset and offset so that each row's offset
# matches the next row's onset (important for the way the other functions work)
fill.in <- function(min, max, timesvec) {
	row = 1
    # The total number of rows that will be needed
	stop.row <- nrow(timesvec)*2+2
	while (row < stop.row) {
        # For the first row
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
		# For the final row
		else if (row == stop.row-1) {
			start.val <- timesvec$Offset[row-1]
			ins.row <- data.frame(Onset=start.val, Offset=max,
			    Duration=max-start.val)
			timesvec <- rbind(timesvec,ins.row)
			row.names(timesvec)<-1:nrow(timesvec)
            # Move forward to end the loop
			row <- row + 1
		}
		# For everything else
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


# Set the info files path
info.path <- "info/"

# Read in the info files we'll need to create the random gaps
dur.data <- read.csv(paste(info.path,"duration.data.csv",sep=""))
supp.info <- read.csv(paste(info.path,"GapInfoSupp.csv",sep=""))

# Create subsets for each version of the stimulus
blbi <- subset(dur.data, Segment == "cdm-blueguys-bikes-mf.avi")$Duration
blli <- subset(dur.data, Segment == "cdm-blueguys-library-fm.avi")$Duration
blra <- subset(dur.data, Segment == "cdm-blueguys-rainyday-fm.avi")$Duration
rebi <- subset(dur.data, Segment == "cdm-redguys-bikes-mf.avi")$Duration
reli <- subset(dur.data, Segment == "cdm-redguys-library-mf.avi")$Duration
rera <- subset(dur.data, Segment == "cdm-redguys-rainyday-fm.avi")$Duration
yebi <- subset(dur.data, Segment == "cdm-yellowguys-bikes-mf.avi")$Duration
yeli <- subset(dur.data, Segment == "cdm-yellowguys-library-fm.avi")$Duration
yera <- subset(dur.data, Segment == "cdm-yellowguys-rainyday-mf.avi")$Duration
robi <- subset(dur.data, Segment == "cdm-robots-birthday-fm.avi")$Duration
roki <- subset(dur.data, Segment == "cdm-robots-kitty-mf.avi")$Duration
ropa <- subset(dur.data, Segment == "cdm-robots-pancakes-fm.avi")$Duration
mebi <- subset(dur.data, Segment == "cdm-merpeople-birthday-fm-25.avi")$Duration
meki <- subset(dur.data, Segment == "cdm-merpeople-kitty-mf-45.avi")$Duration
mepa <- subset(dur.data, Segment == "cdm-merpeople-pancakes-mf-45.avi")$Duration
pabi <- subset(dur.data, Segment == "cdm-party-birthday-fm-55.avi")$Duration
paki <- subset(dur.data, Segment == "cdm-party-kitty-fm-55.avi")$Duration
papa <- subset(dur.data, Segment == "cdm-party-pancakes-mf-55.avi")$Duration

# Make 100 random versions of the gaps and save them as a numbered .csv
for (i in 1:100) {
    # Print an update
	print(paste("Working on ",i, "..."))
	d1 <- find.rand.windows(0, 24.823333, blbi)
	d2 <- find.rand.windows(0, 20.386666, blli)
	d3 <- find.rand.windows(0, 24.19, blra)
	d4 <- find.rand.windows(0, 24.923333, rebi)
	d5 <- find.rand.windows(0, 20.353333, reli)
	d6 <- find.rand.windows(0, 24.19, rera)
	d7 <- find.rand.windows(0, 24.79, yebi)
	d8 <- find.rand.windows(0, 20.32, yeli)
	d9 <- find.rand.windows(0, 24.39, yera)
	d10 <- find.rand.windows(0, 23.323333, robi)
	d11 <- find.rand.windows(0, 24.19, roki)
	d12 <- find.rand.windows(0, 23.456666, ropa)
	d13 <- find.rand.windows(0, 23.523333, mebi)
	d14 <- find.rand.windows(0, 23.79, meki)
	d15 <- find.rand.windows(0, 23.255011, mepa)
	d16 <- find.rand.windows(0, 23.323333, pabi)
	d17 <- find.rand.windows(0, 23.756666, paki)
	d18 <- find.rand.windows(0, 23.221678, papa)
    # Bind it all together
	all.durs <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12,
	    d13, d14, d15, d16, d17, d18)
	new.durs <- cbind(all.durs, supp.info)
    # Print it out
	write.csv(new.durs, paste(info.path,paste(
	    "GapInfoRandom",i,".csv", sep=""),sep=""))
}