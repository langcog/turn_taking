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
	stop.row <- nrow(timesvec)*2+1
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
        # For the second row and beyond...
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
aae1 <- subset(dur.data, Segment == "AE1" & Condition == "A")$Duration
aae2 <- subset(dur.data, Segment == "AE2" & Condition == "A")$Duration
ajap <- subset(dur.data, Segment == "Japanese" & Condition == "A")$Duration
aheb <- subset(dur.data, Segment == "Hebrew" & Condition == "A")$Duration
ager <- subset(dur.data, Segment == "German" & Condition == "A")$Duration
akor <- subset(dur.data, Segment == "Korean" & Condition == "A")$Duration

bae1 <- subset(dur.data, Segment == "AE1" & Condition == "B")$Duration
bae2 <- subset(dur.data, Segment == "AE2" & Condition == "B")$Duration
bjap <- subset(dur.data, Segment == "Japanese" & Condition == "B")$Duration
bheb <- subset(dur.data, Segment == "Hebrew" & Condition == "B")$Duration
bger <- subset(dur.data, Segment == "German" & Condition == "B")$Duration
bkor <- subset(dur.data, Segment == "Korean" & Condition == "B")$Duration

cae1 <- subset(dur.data, Segment == "AE1" & Condition == "C")$Duration
cae2 <- subset(dur.data, Segment == "AE2" & Condition == "C")$Duration
cjap <- subset(dur.data, Segment == "Japanese" & Condition == "C")$Duration
cheb <- subset(dur.data, Segment == "Hebrew" & Condition == "C")$Duration
cger <- subset(dur.data, Segment == "German" & Condition == "C")$Duration
ckor <- subset(dur.data, Segment == "Korean" & Condition == "C")$Duration

dae1 <- subset(dur.data, Segment == "AE1" & Condition == "D")$Duration
dae2 <- subset(dur.data, Segment == "AE2" & Condition == "D")$Duration
djap <- subset(dur.data, Segment == "Japanese" & Condition == "D")$Duration
dheb <- subset(dur.data, Segment == "Hebrew" & Condition == "D")$Duration
dger <- subset(dur.data, Segment == "German" & Condition == "D")$Duration
dkor <- subset(dur.data, Segment == "Korean" & Condition == "D")$Duration

# Make 100 random versions of the gaps and save them as a numbered .csv
for (i in 1:100) {
    # Print an update
    print(paste("Working on ",i, "..."))
    d1 <- find.rand.windows(30.538702, 66.784323, aae1)
    d2 <- find.rand.windows(81.301707, 106.031872, aheb)
    d3 <- find.rand.windows(121.24391, 148.169398, ajap)
    d4 <- find.rand.windows(161.367756, 192.356962, ager)
    d5 <- find.rand.windows(206.161226, 239.505831, akor)
    d6 <- find.rand.windows(271.916913, 307.952822, aae2)
    d7 <- find.rand.windows(30.538702, 66.784323, bae1)
    d8 <- find.rand.windows(81.301707, 108.227195, bjap)
    d9 <- find.rand.windows(123.439233, 154.428439, bger)
    d10 <- find.rand.windows(167.626797, 200.971402, bkor)
    d11 <- find.rand.windows(214.775666, 239.505831, bheb)
    d12 <- find.rand.windows(271.916913, 307.952822, bae2)
    d13 <- find.rand.windows(30.538702, 66.784323, cae1)
    d14 <- find.rand.windows(81.301707, 112.290913, cger)
    d15 <- find.rand.windows(127.502951, 160.847556, ckor)
    d16 <- find.rand.windows(174.045914, 198.776079, cheb)
    d17 <- find.rand.windows(212.580343, 239.505831, cjap)
    d18 <- find.rand.windows(271.916913, 307.952822, cae2)
    d19 <- find.rand.windows(30.538702, 66.784323, dae1)
    d20 <- find.rand.windows(81.301707, 114.646312, dkor)
    d21 <- find.rand.windows(129.85835, 154.588515, dheb)
    d22 <- find.rand.windows(167.786873, 194.712361, djap)
    d23 <- find.rand.windows(208.516625, 239.505831, dger)
    d24 <- find.rand.windows(271.916913, 307.952822, dae2)
    # Bind it all together
    all.durs <- rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12,
        d13, d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24)
    new.durs <- cbind(all.durs, supp.info)
    # Print it out
    write.csv(new.durs, paste(info.path,paste(
        "GapInfoRandom",i,".csv", sep=""),sep=""))
}