library(data.table)
library(grid)
library(ggplot2)
library(bootstrap)
library(lme4)
library(stringr)
library(plotrix)
library(reshape2)
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
    mean(x) - quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
    quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.975) - mean(x)}
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
	# First timepoint in file (for initializing a new time col)
    t1 <- data$Time[1]
    # Reset the values from the following columns to
    # something more useful for analysis:
    # - Make target hit (Female vs. Male) summaries
    data[, F.looks := L.Object.Hit == "FEMALE" |
    	R.Object.Hit == "FEMALE"]
    data[, M.looks := L.Object.Hit == "MALE" |
    	R.Object.Hit == "MALE"]
    # - Make eye event summaries
	# data[, Events := L.Event.Info != "Blink" & L.Event.Info != "-" &
            # R.Event.Info != "Blink" & R.Event.Info != "-"]
    # data[, Saccades := L.Event.Info == "Saccade" |
    	# R.Event.Info == "Saccade"]
    # data[, Fixations := L.Event.Info == "Fixation" |
    	# R.Event.Info == "Fixation"]
    # - Add the experiment version, age group, and subject number
	data[, Version := name.parts[2]]
	# data[, AgeGroup := name.parts[1]]
	data[, Subject := subname]
    # Add a new column for time from the start of the experiment
	data[, TimeSec := round((Time - t1)/1000000, 3)]
	data[, MS.Increment := c(0, diff(TimeSec))]
    # Add a new column for time from the start of each stimulus
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
		segments <- unique(dt[Subject == sub, Segment])
		for (seg in segments) {
			# Create a vector of Look.Dir value runs
	    	numruns <- rle(dt[Subject == sub & Segment == seg, Look.Dir])
	    	# Convert the runs into a data table that gives
	    	# run length and start time
	    	run.vals <- data.table(Length = numruns[['lengths']],
	    		Value = numruns[['values']])
	    	if (nrow(run.vals) > 1) {
				run.vals[, Start := cumsum(Length) - Length + 1]
				run.vals[, Prior := c("0",run.vals[1:(nrow(run.vals) - 1), Value])]
				run.vals[, Next := c(run.vals[2:nrow(run.vals), Value], "0")]
				# find runs of NAs 96ms or less between looks to the same object 
				to.smooth <- which(run.vals$Length < 12 & run.vals$Value == "0"
					 & run.vals$Prior == run.vals$Next)
				# Smooth the gaps; this is about where I would use inverse.rle()
				# if I could figure out how to make an rle object from this altered
				# version of the data.
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
				# Add the smoothed values back into the data table
				dt[Subject == sub & Segment == seg, Look.Dir := smoothed.LD]
			}
		}			
	}
	# Return the smoothed data
	return(dt)
}


################################################################################
# Random run plot functions
################################################################################

combine.runs <- function(DT, filelist, name) {
	curr.row <- 1
	for (file in filelist) {
		# Read in a file
		data <- fread(paste(processed.data.path, file, sep=""))
		# Add the data to the big data table
		DT[curr.row, names(DT) := data]
		curr.row <- curr.row + 1
	}
	setkeyv(DT, "Run")
	# Write it out
	write.csv(DT, paste(processed.data.path,
		name, ".random.runs.csv",sep=""), row.names=FALSE)
	# Return it
	return(DT)
}

get95ths <- function(meltdf, tails, origdf) {
	variables <- unique(meltdf$variable)
	if (tails == 1) {
		idx95 <- round((nrow(origdf) - 1)*.95)
		ninetyfifths <- data.frame(
			variable=variables,
			tvalue=rep(0,length(variables)))
		for (v in variables) {
			ninetyfifths$tvalue[which(ninetyfifths$variable == v)] <-
				sort(subset(meltdf, variable == v)$tvalue)[idx95]
		}
	}
	else if (tails == 2) {
		idx95.l <- round((nrow(origdf) - 1)*.025)
		idx95.u <- round((nrow(origdf) - 1)*.975)
		ninetyfifths <- data.frame(
			variable=variables,
			tvalue.l=rep(0,length(variables)),
			tvalue.u=rep(0,length(variables)))
		for (v in variables) {
			ninetyfifths$tvalue.l[which(ninetyfifths$variable == v)] <-
				sort(subset(meltdf, variable == v)$tvalue)[idx95.l]
			ninetyfifths$tvalue.u[which(ninetyfifths$variable == v)] <-
				sort(subset(meltdf, variable == v)$tvalue)[idx95.u]
		}

	}
	return(ninetyfifths)
}

getPercentile <- function(realvals, randvals, tails) {
	variables <- unique(randvals$variable)
	if (tails == 1) {
		percentiles <- data.frame(
			variable=variables,
			percent=rep(0,length(variables)))
		for (v in variables) {
			percentiles$percent[which(percentiles$variable == v)] <-
			round((nrow(subset(subset(randvals, variable == v), tvalue <
				abs(subset(realvals, variable == v)$tvalue))) /
				nrow(subset(randvals, variable == v)))*100,1)
		}
	}
	else if (tails == 2) {
		percentiles <- data.frame(
			variable=variables,
			percent=rep(0,length(variables)))
		for (v in variables) {
			tval <- subset(realvals, variable == v)$tvalue
			pos <- ifelse(tval > 0, 1, 0)
			if (pos == 1) {
				percentiles$percent[which(percentiles$variable == v)] <-
				round((nrow(subset(subset(randvals, variable == v &
					tvalue > 0), tvalue < tval))) /
					nrow(subset(randvals, variable == v &
					tvalue > 0))*100,1)
			}
			else {
				percentiles$percent[which(percentiles$variable == v)] <-
				round((nrow(subset(subset(randvals, variable == v &
					tvalue < 0), tvalue > tval))) /
					nrow(subset(randvals, variable == v &
					tvalue < 0))*100,1)
			}
		}
	}
	return(percentiles)	
}
