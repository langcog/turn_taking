library(grid)
library(ggplot2)
library(bootstrap)
library(lme4)
library(stringr)
library(plotrix)
library(reshape)
library(plyr)
library(car)


################################################################################
# Misc helper functions (from Mike)
################################################################################
# Add some style elements for ggplot2
plot.style <- theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour="black",size=.5),
    axis.ticks = element_line(size=.5),
    axis.title.x = element_text(vjust=-.5),
    axis.title.y = element_text(angle=90,vjust=0.25),
    panel.margin = unit(1.5,"lines"))

theme_set(theme_bw())

# Standard error of the mean
sem <- function (x) {
    sd(x) / sqrt(length(x))
}

na.mean <- function(x) {mean(x,na.rm=T)}

to.n <- function(x) {
    as.numeric(as.character(x))
}

# Number of unique subs
n.unique <- function (x) {
    length(unique(x))
}

# For bootstrapping 95% confidence intervals
theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
    mean(x) - quantile(bootstrap(
    	1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
    quantile(bootstrap(
    	1:length(x),1000,theta,x)$thetastar,.975) - mean(x)}
ci95 <- function(x) {sem(x)*1.96}

# For basic plots, add linear models with correlations
lm.txt <- function (p1,p2,x=7.5,yoff=.05,lt=2,c="black",data=data) {
    l <- lm(p2 ~ p1)
    regLine(l,lty=lt,col=c)
    cl <- coef(l)
    text(x,cl[1] + cl[2] * x + yoff,
        paste("r = ",sprintf("%2.2f",sqrt(summary(l)$r.squared)),
        getstars(anova(l)$"Pr(>F)"[1]),sep=""), xpd="n")
}

getstars <- function(x) {
    if (x > .1) {return("")}
    if (x < .001) {return("***")}
    if (x < .01) {return("**")}
    if (x < .05) {return("*")}
}


################################################################################
# Data preparation functions
################################################################################
# Reads in the raw tracker data files and converts them to the basic
# information we need to proceed with analyses
read.data <- function(path,file.name) {
    # E.g., A-ADU-P01.txt for condition A, ADULT, and subject 1
    name.parts <- unlist(strsplit(file.name, "[-_.]"))
    subname <- unlist(strsplit(file.name, "[.]"))[1]
    # Read in a tracker data file
    data <- fread(paste(path,file.name,sep=""))
    # Make the colnames readable in R
    newnames <- chartr(" []°", "....", names(data))
    setnames(data, names(data), newnames)
	# First time in file (to initialize
	# a new time column later)
    t1 <- data$Time[1]
    # Reset the values from the following columns to
    # something more useful for analysis:
    # - Make x and y eye position summaries
    data[, L.POR.X..px. := to.n(L.POR.X..px.)]
    data[, R.POR.X..px. := to.n(R.POR.X..px.)]
    data[, L.POR.Y..px. := to.n(L.POR.Y..px.)]
    data[, R.POR.Y..px. := to.n(R.POR.Y..px.)]
    data[, x.pos := mean(c(L.POR.X..px., R.POR.X..px.))]
    data[, y.pos := mean(c(L.POR.Y..px., R.POR.Y..px.))]
    # - Make target hit (Female vs. Male) summaries
    data[, F.looks := L.Object.Hit == "FEMALE" |
    	R.Object.Hit == "FEMALE"]
    data[, M.looks := L.Object.Hit == "MALE" |
    	R.Object.Hit == "MALE"]
    # - Make eye event summaries
	data[, Events := L.Event.Info != "Blink" & L.Event.Info != "-" &
            R.Event.Info != "Blink" & R.Event.Info != "-"]
    data[, Saccades := L.Event.Info == "Saccade" |
    	R.Event.Info == "Saccade"]
    data[, Fixations := L.Event.Info == "Fixation" |
    	R.Event.Info == "Fixation"]
    # - Add the experiment version, age group, and subject number
	data[, Version := name.parts[2]]
	data[, AgeGroup := name.parts[1]]
	data[, Subject := subname]
    # - Add new time columns, including
    #    + TimeSec: time from the start of the experiment and
	data[, TimeSec := round((Time - t1)/1000000, 3)]
	data[, MS.Increment := c(0, diff(TimeSec))]
	#    + TimeSecSeg: time from the start of each segment
	data[, TimeSecSeg := 0]
    videos <- unique(data$Stimulus)
    for (video in videos) {
    	curr.vid <- which(data$Stimulus == video)
    	data[curr.vid[1], MS.Increment := 0]
    	data[curr.vid, TimeSecSeg :=
    		cumsum(data[curr.vid, MS.Increment])]
    }
    # return the newly converted tracker data
    return (data)
}


# Finds gaps in tracker measurements of less than 100ms and, if the participant
# was looking at the same target object before and after the <100ms gap, fills
# in the missing values as looking at the same target object
smoothLD <- function(dt) {
	subjects <- unique(dt$Subject)
	for (sub in subjects) {
		#print(sub)
		segments <- unique(dt[Subject == sub, Segment])
		for (seg in segments) {
			#print(seg)
			# Create a vector of Look.Dir value runs
	    	numruns <- rle(dt[Subject == sub & Segment == seg, Look.Dir])
	    	# Convert the runs into a data table that gives
	    	# run length and start time
	    	run.vals <- data.table(Length = numruns[['lengths']],
	    		Value = numruns[['values']])
	    	if (nrow(run.vals) > 1) {
				run.vals[, Start := cumsum(Length)-Length+1]
				run.vals[, Prior := c("0",run.vals[1:(nrow(run.vals)-1), Value])]
				run.vals[, Next := c(run.vals[2:nrow(run.vals), Value], "0")]
				# find runs of NAs 96ms or less between looks to the same object 
				to.smooth <- which(run.vals$Length < 12 & run.vals$Value == "0"
					 & run.vals$Prior == run.vals$Next)
				# Smooth the gaps; this is about where I would use inverse.rle()
				# if I could figure out how to make an rle object from this altered
				# version of the data
				run.vals[to.smooth, Value := Next]
				smoothed.LD <- c()
				curr.dir <- "F"
				count <- 0
				for (i in 1:nrow(run.vals)) {
					if (i == nrow(run.vals)) {
						last.count <- run.vals[i, Length]
						new.input <- c(rep(curr.dir, count),
							rep(run.vals[i, Value], last.count))
						smoothed.LD <- c(smoothed.LD, new.input)
					} else if (run.vals[i, Value] != curr.dir) {
						new.input <- rep(curr.dir, count)
						smoothed.LD <- c(smoothed.LD, new.input)
						curr.dir <- run.vals[i, Value]
						count <- run.vals[i, Length]
					} else {
						count <- count + run.vals[i, Length]
					}
				}
				# add the smoothed values back into the data table
				dt[Subject == sub & Segment == seg, Look.Dir := smoothed.LD]
			}
		}			
	}
	# return the smoothed data 
	return(dt)
}


# Returns a subset of the observations it receives as input:
# It extracts the observations that are parts of analysis windows
# and puts those together into a new data.table that is then returned
# In addition, it also adds annotations to eye measurements as being
# within particular sub-windows (Anchor/Transition/Gap) for easy
# identification later
#
# |--------A----|-|                     # Speaker A's turn
#                      |-|----B-----|   # Speaker B's turn
# Anchor:      |--|                     # last X ms of A's turn, plus
#                                       #     100ms for fixation
# Transition:   |---------|             # last X ms of A's turn till
#                                       #     the first X ms of B's turn,
#                                       #     plus 100ms for fixation
# Gap:            |----|                # the gap between turns
#
# X is the assumed planning time for making a saccade; it is variable
# by age (shorter for older participants)
#
get.windows.categorical <- function(obs, gap.info, utt1.window,
    utt2.window, fixation.window) {
    # Get rid of gaps with durations < 0 and > 500ms
    # (marked as 0 in the spreadsheet)
    gaps <- subset(gap.info, Gap != "na" & Gap != "0")

    # Initialize the empty data table:
	# Analysis windows on either side of a gap are maximally
	# 0.433 (* 2 for both sides); multiply by # gaps for max
	# time spent in analysis windows beside gaps
    sides <- 0.866*nrow(gaps)
    numsubjects <- length(unique(obs$Subject))
    # Get the maximum number of measurement rows possibly needed
    # for all the analysis windows together:
    # Add the max sidewindow time to the total gap duration,
    # multiply it by 125 (approximate # of 8 ms samples per second)
    # and then by the number of subjects. 
    maxnewobs <- round((sum(gaps$Duration) + sides)*125, 0) * numsubjects
    # Set the values of the new data table with this number of rows
    df.out <- data.table(
    	Subject = rep("", maxnewobs),
    	SubjectNum = rep(0, maxnewobs),
    	TestDate = rep("", maxnewobs),
    	TestTime = rep("", maxnewobs),
    	Gender = rep("", maxnewobs),
    	Birthdate = rep("", maxnewobs),
    	AgeMonths = rep(0, maxnewobs),
    	Age = rep(0, maxnewobs),
    	AgeGroup = rep("", maxnewobs),
    	include = rep(0, maxnewobs),
    	includemonoling = rep(0, maxnewobs),
    	OldSubject = rep("", maxnewobs),
    	Segment = rep("", maxnewobs),
    	Onset = rep(0, maxnewobs),
    	Offset = rep(0, maxnewobs),
    	Duration = rep(0, maxnewobs),
    	Gap = rep("", maxnewobs),
    	SpeakerPrev = rep("", maxnewobs),
    	SpeakerNext = rep("", maxnewobs),
    	Type = rep("", maxnewobs),
    	QType = rep("", maxnewobs),
    	TranscriptPrev = rep("", maxnewobs),
    	TranscriptNext = rep("", maxnewobs),
    	SwitchType = rep("", maxnewobs),
    	order = rep(0, maxnewobs),
    	Time = rep(0, maxnewobs),
    	x.pos = rep(0, maxnewobs),
    	y.pos = rep(0, maxnewobs),
    	F.looks = rep(FALSE, maxnewobs),
    	M.looks = rep(FALSE, maxnewobs),
    	Events = rep(FALSE, maxnewobs),
    	Version = rep(0, maxnewobs),
    	TimeSec = rep(0, maxnewobs),
    	TimeSecSeg = rep(0, maxnewobs),
    	MS.Increment = rep(0, maxnewobs),
    	Saccades = rep(FALSE, maxnewobs),
    	Fixations = rep(FALSE, maxnewobs),
    	StimulusGroup = rep("", maxnewobs),
    	Condition = rep("", maxnewobs),
    	Conversation = rep("", maxnewobs),
    	Turn = rep("", maxnewobs),
    	UttID = rep("", maxnewobs),
    	Scnd.Turn = rep("", maxnewobs),
    	True.Sp.F = rep(0, maxnewobs),
    	True.Sp.M = rep(0, maxnewobs),
    	GenderL = rep("", maxnewobs),
    	SpeechAct.F = rep("", maxnewobs),
    	SpeechAct.M = rep("", maxnewobs),
    	Transcript.F = rep("", maxnewobs),
    	Transcript.M = rep("", maxnewobs),
    	SegGap = rep("", maxnewobs),
    	Look.Dir = rep("", maxnewobs),
    	Anchor.Window = rep("", maxnewobs),
    	Transition.Window = rep("", maxnewobs),
    	Gap.Window = rep("", maxnewobs),
    	Origin = rep("", maxnewobs),
    	QS = rep("", maxnewobs),
    	GapNum = rep(0, maxnewobs),
    	Cumulative.Time = rep(0, maxnewobs))
    # Loop through the gaps being analyzed, pulling out the relevant
    # measurements for each window (Anchor/Gap/Transition), and labeling
    # them for easy identification later
    curr.row <- 1
    for (i in 1:nrow(gaps)) {
    	# Set a current gap
        gap <- gaps[i,]
        # Find the measurements relevant to the whole analysis window
        # for that gap
        rows <- which(obs[,Segment == toString(gap$Segment) &
        	TimeSecSeg >= gap$Onset - utt1.window - fixation.window &
        	TimeSecSeg <= gap$Offset + utt2.window + fixation.window])
        # It's possible that there are no measurements there
        # If so, move on
        if (length(rows) == 0) {
    	    next
        }
        # Otherwise, insert the found measurements into the
        # output data.table
        curr.rows <- curr.row:(curr.row+length(rows)-1)
        df.out[curr.rows, (names(obs)) := obs[rows]]
        
        # Set the anchor window measurements to "ANCHOR"
        anchor.window.idx <- which(obs[rows,
        	TimeSecSeg >= gap$Onset - utt1.window - fixation.window &
        	TimeSecSeg <= gap$Onset])
        df.out[curr.row + anchor.window.idx-1,
        	Anchor.Window := "ANCHOR"]
        
        # Set the transition window measurements to "TRANSITION"
        transition.window.idx <- which(obs[rows,
            TimeSecSeg >= gap$Onset - utt1.window &
            TimeSecSeg <= gap$Offset + utt2.window + fixation.window])
        df.out[curr.row + transition.window.idx-1,
        	Transition.Window := "TRANSITION"]

        # Set the gap window measurements to "GAP"
        gap.window.idx <- which(obs[rows,
        	TimeSecSeg >= gap$Onset &
            TimeSecSeg <= gap$Offset])
        df.out[curr.row + gap.window.idx-1, Gap.Window := "GAP"]

        # Set the "origin" to the previous speaker at that gap
        df.out[curr.rows, Origin := as.character(gap$SpeakerPrev)]

        # Set the QS status to the transition type (question/non-question)
        df.out[curr.rows, QS := as.character(gap$Type)]

        # Set the gap number
        df.out[curr.rows, GapNum := as.character(gap$Gap)]

        # Create a "cumulative" time to plot the analysis window from t=0    
        minTime <- min(obs$TimeSecSeg[rows])
		cumul.time <- round((obs$TimeSecSeg[rows] - minTime), 3)
        df.out[curr.rows, Cumulative.Time := cumul.time]

        curr.row <- curr.row+length(rows)
    }
    # Return the analysis window measurements,
    # excluding empty rows and re-ordered
    df.out <- df.out[1:(curr.row-1),]
    df.out <- df.out[order(Subject, GapNum, Cumulative.Time),]
    return(df.out)
}