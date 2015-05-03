# Reads in the raw tracker data files and converts some basic raw values for us.
read.data <- function(path,file.name) {
    # E.g., A-ADU-P01.txt (for condition A, ADULT, and participant 01)
    name.parts <- unlist(strsplit(file.name, "[-_.]"))
    data <- read.csv(paste(path,file.name,sep=""))
    # Within the tracker data for this participant...
    data <- within(data, {
        L.POR.X..px. <- to.n(L.POR.X..px.)
        R.POR.X..px. <- to.n(R.POR.X..px.)
        L.POR.Y..px. <- to.n(L.POR.Y..px.)
        R.POR.Y..px. <- to.n(R.POR.Y..px.)
        # Average x and y positions
        x.pos <- rowMeans(data[, c("L.POR.X..px.", "R.POR.X..px.")])
        y.pos <- rowMeans(data[, c("L.POR.Y..px.", "R.POR.Y..px.")])
        # Object.Hits are when the eyes are looking in an area of interest
        L.looks <- L.Object.Hit == "LEFT" | R.Object.Hit == "LEFT"
        R.looks <- L.Object.Hit == "RIGHT" | R.Object.Hit == "RIGHT"
        # Events are what the eyes are doing (e.g., fixating, saccading, etc.)
        Events <- L.Event.Info != "Blink" & L.Event.Info != "-" &
            R.Event.Info != "Blink" & R.Event.Info != "-"
        Saccades <- L.Event.Info == "Saccade" | R.Event.Info == "Saccade"
        Fixations <- L.Event.Info == "Fixation" | R.Event.Info == "Fixation"
        # Add a version/agegroup/subject column based on the file name
        Condition <- rep(name.parts[1], nrow(data))
        AgeGroup <- rep(name.parts[2], nrow(data))
        Subject <- rep(paste(name.parts[1], name.parts[3], name.parts[2],
            sep = "-"), nrow(data))
        # Make the time into seconds
        TimeSec <- round((Time - Time[1])/(1000000), 3)
        MS.Increment <- c(0, diff(TimeSec))
    })
    return (data)
}


# Adds annotations to those measurements that are within the analysis windows
# |--------A----|-|                     # Speaker A's turn
#                      |-|----B-----|   # Speaker B's turn
# Anchor:      |--|
# Transition:   |---------|
# Gap:            |----|
#
get.windows.categorical <- function(obs, gap.info, utt1.window,
                                    utt2.window, fixation.window) {
  # Get rid of gaps over 550 ms (marked as 0 in the spreadsheet)
  gaps <- subset(gap.info, Gap != "na" & Gap != "0")
  df.out = data.frame()
  for (i in 1:nrow(gaps)) {
    gap <- gaps[i,]
    rows <- subset(obs,
                   Condition == toString(gap$Condition) & 
                     (TimeSec >= gap$Onset - utt1.window - fixation.window) & 
                     (TimeSec <= gap$Offset + utt2.window + fixation.window))           
    if (nrow(rows) == 0) {
      next
    }
    # Set the anchor window measurements to "ANCHOR"
    anchor.window.idx <- which(
      (rows$TimeSec >= gap$Onset - utt1.window - fixation.window) &
        rows$TimeSec <= gap$Onset)
    rows$Anchor.Window <- rep(NA, nrow(rows))
    rows$Anchor.Window[anchor.window.idx] <- "ANCHOR"
    # Set the transition window measurements to "TRANSITION"
    transition.window.idx <- which(
      (rows$TimeSec >= gap$Onset - utt1.window) &
        (rows$TimeSec <= gap$Offset + utt2.window + fixation.window))
    rows$Transition.Window <- rep(NA, nrow(rows))
    rows$Transition.Window[transition.window.idx] <- "TRANSITION"
    # Set the gap window measurements to "GAP"
    gap.window.idx <- which(rows$TimeSec >= gap$Onset &
                              rows$TimeSec <= gap$Offset)
    rows$Gap.Window <- rep(NA, nrow(rows))
    rows$Gap.Window[gap.window.idx] <- "GAP"
    # Set the origin to the previous speaker at that gap
    rows$Origin <- rep(as.character(gap$SpeakerPrev), nrow(rows))
    # Set the QS status to the transition type (question or non-question)
    rows$QS <- rep(as.character(gap$Type), nrow(rows))
    # Set the gap number
    rows$GapNum <- rep(toString(gap$Gap), nrow(rows))
    # Create a "cumulative" time to plot the analysis window from t=0    
    minTime <- min(rows$TimeSec)
    rows$Cumulative.Time <- round((rows$TimeSec - minTime), 3) 
    df.out <- rbind(df.out, rows)
  }
  # Re-order data and return it
  df.out <- df.out[order(df.out$Subject, as.integer(df.out$GapNum),
                         df.out$Cumulative.Time), ]
  return(df.out)
}



# optimized version of the above function, should perform identically
# mcf 4/30/15
get.windows.categorical2 <- function(obs, gap.info, utt1.window,
                                    utt2.window, fixation.window) {
  
  # Get rid of gaps over 550 ms (marked as 0 in the spreadsheet)
  gaps <- gap.info %>% 
    filter(Gap != "na", Gap != "0")
 
  # shuffle and then rearrange
  df.out <- gaps %>% 
    group_by(order) %>% 
    do(get.gap2(.$Condition, .$Onset, .$Offset, .$SpeakerPrev, .$Type, .$Gap,
               obs, utt1.window, utt2.window, fixation.window)) %>%
    arrange(Subject, order, Cumulative.Time)     
  
   return(df.out)
}

## helper function for optimized get.windows.categorical
get.gap2 <- function(condition, onset, offset, speakerprev, type, gap,
                    obs, utt1.window, utt2.window, fixation.window) {
  
  obs %>% 
    filter(Condition == condition,
           (TimeSec >= onset - utt1.window - fixation.window), 
           (TimeSec <= offset + utt2.window + fixation.window)) %>%
    mutate(Anchor.Window = TimeSec >= onset - utt1.window - fixation.window &
             TimeSec <= onset,
           Transition.Window = TimeSec >= onset - utt1.window &
             TimeSec <= offset + utt2.window + fixation.window,
           Gap.Window = TimeSec >= onset & TimeSec <= offset,
           Origin = speakerprev,
           QS = type,
           Gap = gap,
           Cumulative.Time = round(TimeSec - min(TimeSec),3))
}   

## get gap original function
get.gap <- function(condition, onset, offset, speakerprev, type, gap,
                    obs, utt1.window, utt2.window, fixation.window) {
  
  rows <- subset(obs,
                 Condition == toString(condition) & 
                   (TimeSec >= onset - utt1.window - fixation.window) & 
                   (TimeSec <= offset + utt2.window + fixation.window))           
  if (nrow(rows) == 0) {
    next
  }
  # Set the anchor window measurements to "ANCHOR"
  anchor.window.idx <- which(
    (rows$TimeSec >= onset - utt1.window - fixation.window) &
      rows$TimeSec <= onset)
  rows$Anchor.Window <- rep(NA, nrow(rows))
  rows$Anchor.Window[anchor.window.idx] <- "ANCHOR"
  # Set the transition window measurements to "TRANSITION"
  transition.window.idx <- which(
    (rows$TimeSec >= onset - utt1.window) &
      (rows$TimeSec <= offset + utt2.window + fixation.window))
  rows$Transition.Window <- rep(NA, nrow(rows))
  rows$Transition.Window[transition.window.idx] <- "TRANSITION"
  # Set the gap window measurements to "GAP"
  gap.window.idx <- which(rows$TimeSec >= onset &
                            rows$TimeSec <= offset)
  rows$Gap.Window <- rep(NA, nrow(rows))
  rows$Gap.Window[gap.window.idx] <- "GAP"
  # Set the origin to the previous speaker at that gap
  rows$Origin <- rep(as.character(speakerprev), nrow(rows))
  # Set the QS status to the transition type (question or non-question)
  rows$QS <- rep(as.character(type), nrow(rows))
  # Set the gap number
  rows$GapNum <- rep(toString(gap), nrow(rows))
  # Create a "cumulative" time to plot the analysis window from t=0    
  minTime <- min(rows$TimeSec)
  rows$Cumulative.Time <- round((rows$TimeSec - minTime), 3) 
  
  return(rows)
}

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

# optimized version of the above
# note, makes use of linear filter that depends on equal spacing of points
# .0083 msec * 12 = 100 msec, of which > 90 msec need to be looks. 
# note: GapNum is factor, oddly, so we cast back to numeric. 
grab.transition.info2 <- function(obs, fixation.window) {
  
  obs %>%  
    group_by(Subject, Gap) %>% 
    mutate(iow = Looks.Origin * MS.Increment * Anchor.Window, 
           idt = Looks.Destination * MS.Increment * Transition.Window,
           anchor.filter = as.numeric(stats::filter(iow, filter = rep(1, 12), 
                                                    side = 1)),
           transition.filter = as.numeric(stats::filter(idt, filter = rep(1, 12), 
                                                        side = 1)))  %>%
    summarise(Anchor = any(anchor.filter > .09, na.rm = TRUE),
              Transition = any(transition.filter > .09, na.rm = TRUE),              
              Switch = Anchor & Transition,               
              # first look to destination is corrected by 100ms for filter
              Transition.Time = Cumulative.Time[transition.filter > .09 & 
                                                  !is.na(transition.filter)][1] - .1,              
              # last look to origin before first look to transition
              Start = ifelse(Switch, 
                             max(Cumulative.Time[Looks.Origin & 
                                                   Cumulative.Time < Transition.Time], 
                                 na.rm=TRUE),
                             0)) %>%
    arrange(Gap) # arrange nicely, not necessary
}

# looks.to.origin <- subset(obs, Looks.Origin == "1" & TimeSec < start)
# # Find the latest time in that subset, and retrieve it's equivalent in 
# # Cumulative Time
# return(obs[which.max(looks.to.origin$TimeSec),]$Cumulative.Time))


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
  lgrps <- unique(obs$LgGroup)
  ages <- sort(unique(obs$Age))
  df.row <- 1
  increment <- 0.025
  maxtime.all <- max(obs$Start.Time)
  N <- ceiling(maxtime.all/increment)*length(ages)*length(lgrps)
  cumulative.prob <- data.frame(TimeIncrement=rep(NA, N),
                                Proportion=rep(NA, N), Age=rep(NA, N), Condition=rep(NA, N))
  # Loops through the language groups
  for (i in 1:length(lgrps)) {
    sub.data.g <- subset(obs, LgGroup == lgrps[i])
    # Finds the max time needed
    maxtime <- max(sub.data.g$Start.Time)
    # Grab data for a particular age
    for (j in 1:length(ages)) {
      start <- 0
      total.looks <- sum(sub.data.g$Age == ages[j])
      # Excludes cases where no switch was made (Start.Time == 0.0)
      sub.data.a <- subset(sub.data.g, Age == ages[j] & Start.Time > 0)
      # Change value for adults
      agenum <- ifelse(ages[j] == "A", "Adult", as.character(ages[j]))
      cond <- as.character(lgrps[i])
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