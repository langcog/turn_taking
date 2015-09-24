source("0-helper.R")
library(dplyr)
library(ggplot2)
library(lme4)
library(gridExtra)
library(scales)  
library(Hmisc)
library(data.table)

################################################################################
# Read in data in processed_data/ path and set plotting variables
################################################################################
# Set paths
raw.data.path <- "tracker_data/" #"../../data/E1/tracker_data/"
info.path <- "info/" # "../../data/E1/info/"
processed.data.path <- "processed_data/" #"../../data/E1/processed_data/"
plot.path <- "plots/" #"plots/"

# Read in supplemental data and prepped data
gap.info <- fread(paste(info.path,"GapInfo.csv", sep=""))
subject.info <- fread(paste(info.path,"SubjectInfo.csv",sep=""))

# Color and line palettes
stoplightPalette <- c("dodgerblue3", "forestgreen",
	"green3", "firebrick2")
stoplightPalette2 <- c("gray26", "firebrick2",
     "forestgreen", "dodgerblue3", "green3")
stoplightPalette3 <- c("dodgerblue3", "forestgreen",
	"green3", "firebrick2")
lines <- c("solid", "dashed")
lines2 <- c("longdash", "dotted")

# Read in all the random runs and combine them
files <- dir(processed.data.path, pattern="switch.final.rand.[0-9]+.csv")
# Initialize an empty data table
N <- length(files) * 4000 # > max length of a switch.final.rand csv file
switch.final.r <- data.table(
	Subject = rep("", N),
	Gap = rep(0, N),
	Switch = rep(0, N),
	Start = rep(0, N),
	Run = rep(0,N))
# Fill the data table with the random runs
curr.row <- 1
for (file in files) {
	# Read in a file
	data <- fread(paste(processed.data.path, file, sep=""))
	# Record the random run number
	data[, Run := unlist(strsplit(file, "[.]"))[4]]
	# Add the data to the big data table
	curr.rows <- curr.row:(curr.row + nrow(data) - 1)
	switch.final.r[curr.rows, names(switch.final.r) := data]
	curr.row <- curr.row + nrow(data)
}
# Get rid of empty rows and re-order data
switch.final.r <- switch.final.r[1:(curr.row-1),]
switch.final.r[, Sample := "Random"]
setkeyv(switch.final.r, c("Run", "Subject", "Gap"))
# Write it out
write.csv(switch.final.r, paste(processed.data.path,
	"random.runs.csv",sep=""), row.names=FALSE)

# Read in real transition data and set Run number (0)
switch.final <- fread(paste(processed.data.path, "switch.final.csv", sep=""))
switch.final[, Run := 0]
switch.final[, Sample := "Transition"]

# Combine the two data sets (real and random)
switch.all <- rbind(switch.final, switch.final.r)

# Add in supplementary Gap and Subject information
gap.info <- gap.info %>%
            filter(Exclude == 0) %>%
            select(Gap, Duration, Type, QType, Segment) %>%
            left_join(select(vid.info, Segment, Condition), by = "Segment") %>%
            mutate(Type = ifelse(QType == "wh" &
            	Condition == "muffled", "S", Type))

sub.info <- select(subject.info, Subject, Age) %>%
            filter(Age != "NA")

switch.all <- switch.all %>%
        inner_join(gap.info, by = "Gap") %>%
        inner_join(sub.info, by = "Subject")


################################################################################
# Create plots
################################################################################
###### DIFF BETWEEN RANDOM AND TRANSITION SAMPLE SETS ##########################
mean.agg <- aggregate(Switch ~ Condition + Sample + Age, switch.all, mean)
conds <- mean.agg$Condition
ages <- mean.agg$Age
ages[ages == "21"] <- "A"
samples <- mean.agg$Sample
means <- mean.agg$Switch
errs <- aggregate(Switch ~ Condition + Sample + Age, switch.all, sem)$Switch
linecolors <- c(
	"black", "black", "black", "black",
	"blue", "green1", "green2", "red",
	"black", "black", "black", "black",
	"blue", "green1", "green2", "red",
	"black", "black", "black", "black",
	"blue", "green1", "green2", "red",
	"black", "black", "black", "black",
	"blue", "green1", "green2", "red",
	"black", "black", "black", "black",
	"blue", "green1", "green2", "red",
	"black", "black", "black", "black",
	"blue", "green1", "green2", "red",
	"black", "black", "black", "black",
	"blue", "green1", "green2", "red")

df <- data.frame(
    Age = factor(ages),
    Condition = factor(conds),
    Sample = factor(samples),
    LnColors = factor(linecolors),
    m = means,
    se = errs
)
df$Condition <- factor(df$Condition, labels=c("No speech", "Prosody only",
    "Normal speech", "Words only"))
df$Condition <- factor(df$Condition, levels=c("Normal speech", "Prosody only",
    "Words only", "No speech"))

limits <- aes(ymax = m + se, ymin= m - se)

p1 <- qplot(Age,m,facets = . ~ Condition, group=Sample, ylim=c(-0.1,0.6),
    ymin= m - se, ymax= m + se, color=LnColors, linetype=Sample,
    xlab="Age (years)",ylab="Proportion gaze switches",
    position=position_dodge(width=.1), data=df) +
	geom_line(size=2, position=position_dodge(width=.1)) +
	geom_pointrange(size=1, position=position_dodge(width=.1)) +
    scale_colour_manual(name="LnColors", values=stoplightPalette2, guide=FALSE) +
    scale_linetype_manual(name="Sample", values=lines) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),
    strip.text.x = element_text(size=30, color="gray20"),
    legend.justification=c(1,0), legend.position=c(1,0),
    legend.title = element_text(colour="gray20", size=24),
    legend.text = element_text(colour="gray20", size=26),
    legend.key.width = unit(4, "lines"), legend.key.height = unit(2, "lines"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "samples-by-conditions.png", sep=""),
    width=2000,height=600,units="px", bg = "transparent")
print(p1)
dev.off()    
################################################################################


###### BY CONDITION & CONDENSED AGE & Q-EFFECTS SWITCHES #######################
switch.real <- filter(switch.all, Sample == "Transition") %>%
               select(-Sample)
errs.agg <- aggregate(Switch ~ Type + Condition + Age,
    switch.real, sem)
errs <- errs.agg$Switch
means.agg <- aggregate(Switch ~ Type + Condition + Age,
    switch.real, mean)
means <- means.agg$Switch
ages <- means.agg$Age
ages[ages == "21"] <- "A"
conds <- means.agg$Condition
types <- means.agg$Type

df <- data.frame(
    Age = factor(ages),
    Condition = factor(conds),
    Transition = factor(types),
    m = means,
    se = errs
)
df$Condition <- factor(df$Condition, labels=c("No speech", "Prosody only",
    "Normal speech", "Words only"))
df$Transition <- factor(df$Transition, labels=c("Question", "Non-Question"))
df$Condition <- factor(df$Condition, levels=c("Normal speech", "Prosody only",
    "Words only", "No speech"))

limits <- aes(ymax = m + se, ymin= m - se)

p3 <- qplot(Age,m,facets = . ~ Condition, group=Transition, ylim=c(-0.1,0.6),
    ymin= m - se, ymax= m + se, color=Condition, linetype=Transition,
    xlab="Age (years)",ylab="Proportion gaze switches",
    position=position_dodge(width=.1), data=df) +
	geom_line(size=2, position=position_dodge(width=.1)) +
	geom_pointrange(size=1, position=position_dodge(width=.1)) +
    scale_colour_manual(name="Condition", values=stoplightPalette3, guide=FALSE) +
    scale_linetype_manual(name="Transition", values=lines2) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),     
    strip.text.x = element_text(size=30, color="gray20"),
    legend.justification=c(1,0), legend.position=c(1,0),
    legend.title = element_text(colour="gray20", size=24),
    legend.text = element_text(colour="gray20", size=26),
    legend.key.width = unit(4, "lines"), legend.key.height = unit(2, "lines"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "transitions-by-conditions-uncorrected.png", sep=""),
    width=2000,height=600,units="px", bg = "transparent")
print(p3)
dev.off()


################################################################################
# Run statistics
################################################################################
# Split the data into adults and children for separate models
switch.real <- switch.real %>%
	mutate(Condition = as.factor(Condition),
		   Type = as.factor(Type))
switch.real.C <- filter(switch.real, Age != 21) %>%
                 mutate(Age = as.numeric(Age))
switch.real.A <- filter(switch.real, Age == 21)

# Models
# Children
chi.max <- glmer(Switch ~ Age * Condition * Type + Duration + 
    (Type|Subject) + (1|Gap), data= switch.real.C, family=binomial,
    control = glmerControl(optimizer="bobyqa"))
chi.coefs <- data.frame(t(sapply(fixef(chi.max),c)))
chi.coefs$Sample <- "real"
chi.coefs$Error <- ""
chi.coefs$Run <- 0

# Adults
adu.max <- glmer(Switch ~ Condition * Type + Duration + 
    (Type|Subject) + (1|Gap), data=switch.real.A, family=binomial,
    control = glmerControl(optimizer="bobyqa"))
adu.coefs <- data.frame(t(sapply(fixef(adu.max),c)))
adu.coefs$Sample <- "real"
adu.coefs$Error <- ""
adu.coefs$Run <- 0

# Write out the real data model results
write.csv(summary(chi.max)$coefficients, paste(
	processed.data.path, "chi.model.csv", sep=""),
	row.names=FALSE)
write.csv(summary(adu.max)$coefficients, paste(
	processed.data.path, "adu.model.csv", sep=""),
	row.names=FALSE)
	
# Write out the real model results for combination with
# the random runs
write.csv(chi.coefs, paste(
	processed.data.path, "chi.coefs-0.csv", sep=""),
	row.names=FALSE)
write.csv(adu.coefs, paste(
	processed.data.path, "adu.coefs-0.csv", sep=""),
	row.names=FALSE)


# Post hoc test of difference from random in 1- and 2-year-olds'
# performance in the lex-only and pros-only conditions
# u3.r <- subset(switch.final.r, Age < 3)
# u3.a <- subset(switch.final, Age < 3)
# u3.lex.r <- subset(u3.r, Condition == "robot")
# u3.lex.a <- subset(u3.a, Condition == "robot")
# u3.pro.r <- subset(u3.r, Condition == "muffled")
# u3.pro.a <- subset(u3.a, Condition == "muffled")

# u3.lex.r.agg <- aggregate(Switch ~ Subject, u3.lex.r, mean)
# u3.lex.a.agg <- aggregate(Switch ~ Subject, u3.lex.a, mean)
# u3.pro.r.agg <- aggregate(Switch ~ Subject, u3.pro.r, mean)
# u3.pro.a.agg <- aggregate(Switch ~ Subject, u3.pro.a, mean)

# ttest1 <- t.test(u3.lex.r.agg$Switch, u3.lex.a.agg$Switch)
# ttest2 <- t.test(u3.pro.r.agg$Switch, u3.pro.a.agg$Switch)

run.models <- function(ns) {
    # Split the data into adults and children for separate models
    switch.rand <- filter(switch.all, Sample == "Random") %>%
                   select(-Sample) %>%
				   mutate(Condition = as.factor(Condition),
		   		          Type = as.factor(Type))

	switch.rand.C <- filter(switch.rand, Age != 21) %>%
         			        mutate(Age = as.numeric(Age))
	switch.rand.A <- filter(switch.rand, Age == 21)

    withCallingHandlers({
        for(i in ns){
            w <- length(warnings())
            errors <- ""
	    	# Update
    	    print(i)

    		# Retrieve the random gap info data for this particular run
	    	rand.data.C <- switch.rand.C[Run == i,]
	    	chi.coefs.r <- chi.coefs[0,]

    		# Models
	    	# Children
	    	chi.max.r <- glmer(Switch ~ Age * Condition * Type + Duration + 
                (Type|Subject) + (1|Gap), data=rand.data.C, family=binomial,
                control = glmerControl(optimizer="bobyqa"))
    		chi.coefs.r <- rbind(chi.coefs.r,
    			cbind(data.frame(t(sapply(fixef(chi.max.r),c))),
    			data.frame(Sample = "random", Error = errors, Run = i)))
    		write.csv(chi.coefs.r, paste(
				processed.data.path, "chi.coefs.r-", i, ".csv", sep=""),
				row.names=FALSE)
        }
    }, warning = function(w){
	    if (errors == "") {
	        errors <<- w$message
        }
        invokeRestart("muffleWarning")
    })

    withCallingHandlers({
        for(i in ns){
            w <- length(warnings())
            errors <- ""
    		# Update
        	print(i)

    		# Retrieve the random gap info data for this particular run
	    	rand.data.A <- switch.rand.A[Run == i,]	
	    	adu.coefs.r <- adu.coefs[0,]

		    # Models
    		# Adults
	    	adu.max.r <- glmer(Switch ~ Condition * Type + Duration + 
                (Type|Subject) + (1|Gap), data=rand.data.A, family=binomial,
                control = glmerControl(optimizer="bobyqa"))
    		adu.coefs.r <- rbind(adu.coefs.r,
    			cbind(data.frame(t(sapply(fixef(adu.max.r),c))),
    			data.frame(Sample = "random", Error = errors, Run = i)))
    		write.csv(adu.coefs.r, paste(
				processed.data.path, "adu.coefs.r-", i, ".csv", sep=""),
				row.names=FALSE)

        }
    }, warning = function(w){
	    if (errors == "") {
	        errors <<- w$message
        }
        invokeRestart("muffleWarning")
    })
}

compare.models <- function() {
	chi.models <- do.call("rbind", lapply(list.files(
		path="processed_data/", pattern="chi.coefs*", full.names=TRUE),
		read.csv, header=TRUE))
	write.csv(chi.models, paste(processed.data.path,
        "chi.models.csv", sep=""), row.names=FALSE)

		
	adu.models <- do.call("rbind", lapply(list.files(
		path="processed_data/", pattern="adu.coefs*", full.names=TRUE),
		read.csv, header=TRUE))
	write.csv(adu.models, paste(processed.data.path,
        "adu.models.csv", sep=""), row.names=FALSE)

	# Derive the absolute values of the coefficients for
	# The real data and
	abs.chi.coefs <- cbind(abs(chi.models[,1:(length(chi.models)-3)]),
		chi.models[,(length(chi.models)-2):length(chi.models)])
	abs.adu.coefs <- cbind(abs(adu.models[,1:(length(adu.models)-3)]),
		adu.models[,(length(adu.models)-2):length(adu.models)])

	# Print, for each model, how many random coefficients
	# are less than the real data coefficients (with absolute values)
	print("### Child model: ")
	for (i in 1:(length(abs.chi.coefs)-3)) {
		pred <- names(abs.chi.coefs)[i]
		trueval <- as.numeric(abs.chi.coefs[abs.chi.coefs$Sample == "real",i])
		gtrthn <- sum(trueval > abs.chi.coefs[
			abs.chi.coefs$Sample == "random",i])/(nrow(abs.chi.coefs)-1)*100
		print(paste(pred, " B=", round(trueval,3),
		            " > ", round(gtrthn,3), "%", sep=""))
	}
	print(" ")
	print("### Adult model: ")
	for (i in 1:(length(abs.adu.coefs)-3)) {
		pred <- names(abs.adu.coefs)[i]
		trueval <- as.numeric(abs.adu.coefs[abs.adu.coefs $Sample == "real",i])
		gtrthn <- sum(trueval > abs.adu.coefs[
			abs.adu.coefs$Sample == "random",i])/(nrow(abs.adu.coefs)-1)*100
		print(paste(pred, " B=", round(trueval,3),
		            " > ", round(gtrthn,3), "%", sep=""))
	}

}
