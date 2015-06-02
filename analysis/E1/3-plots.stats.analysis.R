library(dplyr)
library(ggplot2)
library(lme4)
library(gridExtra)
library(scales)  
library(Hmisc)
library(data.table)
source("0-helper.R")

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
stoplightPalette <- c("dodgerblue3", "firebrick2")
stoplightPalette2 <- c("gray26", "dodgerblue3", "firebrick2")
stoplightPalette3 <- c("dodgerblue3", "firebrick2")
lines <- c("solid", "dashed")
lines2 <- c("longdash", "dotted")

# Read in all the random runs and combine them
files <- dir(processed.data.path, pattern="switch.final.rand.[0-9]+.csv")
# Initialize an empty data table
N <- length(files) * 3737 # maximum length of a switch.final.rand csv file
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
            filter(Exclude == 0, Condition == "A") %>%
            select(Gap, Duration, Type, QType, Segment, LgGroup)

sub.info <- select(subject.info, Subject, Age) %>%
            filter(Age != "NA")

switch.all <- switch.all %>%
        inner_join(gap.info, by = "Gap") %>%
        inner_join(sub.info, by = "Subject")


################################################################################
# Create plots
################################################################################
###### DIFF BETWEEN RANDOM AND TRANSITION SAMPLE SETS ##########################
mean.agg <- aggregate(Switch ~ LgGroup + Sample + Age, switch.all, mean)
langs <- mean.agg$LgGroup
ages <- mean.agg$Age
samples <- mean.agg$Sample
means <- mean.agg$Switch
errs <- aggregate(Switch ~ LgGroup + Sample + Age, switch.all, sem)$Switch
linecolors <- c(
	"black", "black",
	"green2", "red",
	"black", "black",
	"green2", "red",
	"black", "black",
	"green2", "red",
	"black", "black",
	"green2", "red")

df <- data.frame(
    Age = factor(ages),
    Sample = factor(samples),
    Language = factor(langs),
    m = means,
    se = errs,
    LnColors = factor(linecolors)
)
df$Sample <- factor(df$Sample, levels=rev(levels(df$Sample)))
df$Language <- factor(df$Language, labels=c("English", "Non-English"))

limits <- aes(ymax = m + se, ymin= m - se)
dodge <- position_dodge(width=0.9)

p1 <- qplot(Age,m,facets = . ~ Language, group=Sample, ylim=c(-0.1,0.6),
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
png(paste(plot.path, "samples-by-lang-groups.png", sep=""),
    width=1100,height=600,units="px", bg = "transparent")
print(p1)
dev.off()    
################################################################################


###### BY CONDITION & CONDENSED AGE & Q-EFFECTS SWITCHES #######################
switch.real <- filter(switch.all, Sample == "Transition") %>%
               select(-Sample)
errs.agg <- aggregate(Switch ~ Type + LgGroup + Age,
    switch.real, sem)
errs <- errs.agg$Switch
means.agg <- aggregate(Switch ~ Type + LgGroup + Age,
    switch.real, mean)
means <- means.agg$Switch
ages <- means.agg$Age
langs <- means.agg$LgGroup
types <- means.agg$Type

df <- data.frame(
    Age = factor(ages),
    Language = factor(langs),
    Transition = factor(types),
    m = means,
    se = errs
)
df$Language <- factor(df$Language, labels=c("English", "Non-English"))
df$Transition <- factor(df$Transition, labels=c("Question", "Non-Question"))

limits <- aes(ymax = m + se, ymin= m - se)

p3 <- qplot(Age,m,facets = . ~ Language, group=Transition, ylim=c(-0.1,0.6),
    ymin= m - se, ymax= m + se, color= Language, linetype=Transition,
    xlab="Age (years)",ylab="Proportion gaze switches",
    position=position_dodge(width=.1), data=df) +
	geom_line(size=2, position=position_dodge(width=.1)) +
	geom_pointrange(size=1, position=position_dodge(width=.1)) +
    scale_colour_manual(name="Language", values=stoplightPalette3, guide=FALSE) +
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
    width=1100,height=600,units="px", bg = "transparent")
print(p3)
dev.off()


################################################################################
# Run statistics
################################################################################
# Split the data into adults and children for separate models
switch.real.C <- filter(switch.real, Age != "A") %>%
                 mutate(Age = as.numeric(Age))
switch.real.A <- filter(switch.real, Age == "A")

# Models
# Children
chi.max <- glmer(Switch ~ Age * LgGroup * Type + Duration + 
    (Type * LgGroup|Subject) + (1|Gap), data= switch.real.C, family=binomial,
    control = glmerControl(optimizer="bobyqa"))
chi.coefs <- data.frame(t(sapply(fixef(chi.max),c)))
chi.coefs$Sample <- "real"

# Adults
adu.max <- glmer(Switch ~ LgGroup * Type + Duration + 
    (Type * LgGroup|Subject) + (1|Gap), data= switch.real.A, family=binomial,
    control = glmerControl(optimizer="bobyqa"))
adu.coefs <- data.frame(t(sapply(fixef(adu.max),c)))
adu.coefs$Sample <- "real"

# Write out the real data model results
write.csv(summary(chi.max)$coefficients, paste(
	processed.data.path, "chi.model.csv", sep=""),
	row.names=FALSE)
write.csv(summary(adu.max)$coefficients, paste(
	processed.data.path, "adu.model.csv", sep=""),
	row.names=FALSE)

run.models <- function(ns) {
    # Split the data into adults and children for separate models
    switch.rand <- filter(switch.all, Sample == "Random") %>%
                   select(-Sample)
    switch.rand.C <- filter(switch.rand, Age != "A") %>%
                     mutate(Age = as.numeric(Age))
    switch.rand.A <- filter(switch.rand, Age == "A")

    # Initialize coefficient data frames
    # for collecting random run results
    chi.coefs.r <- chi.coefs[0,]
    adu.coefs.r <- adu.coefs[0,]

	for (i in ns) {
		# Update
		print(i)
		
		# Retrieve the random gap info data for this particular run
		rand.data.C <- switch.rand.C[Run == i,]
		rand.data.A <- switch.rand.A[Run == i,]	
		# Models
		# Children
		chi.max.r <- glmer(Switch ~ Age * LgGroup * Type + Duration + 
            (Type * LgGroup|Subject) + (1|Gap), data= rand.data.C, family=binomial,
            control = glmerControl(optimizer="bobyqa"))
		chi.coefs.r <- rbind(chi.coefs.r, data.frame(t(sapply(fixef(chi.max.r),c))))
		
		# Adults
		adu.max.r <- glmer(Switch ~ LgGroup * Type + Duration + 
            (Type * LgGroup|Subject) + (1|Gap), data= rand.data.A, family=binomial,
            control = glmerControl(optimizer="bobyqa"))
		adu.coefs.r <- rbind(adu.coefs.r, data.frame(t(sapply(fixef(adu.max.r),c))))
		
	}
	
	chi.coefs.r$Sample <- "random"
	adu.coefs.r$Sample <- "random"

    # Add a label to the coefficient tables
    # And then bind them together
    all.chi.coefs <- rbind(chi.coefs.r, chi.coefs)
    all.adu.coefs <- rbind(adu.coefs.r, adu.coefs)

    # And write them out
    write.csv(all.chi.coefs, paste(processed.data.path,
        "chi.coefs.csv", sep=""), row.names=FALSE)
    write.csv(all.adu.coefs, paste(processed.data.path,
        "adu.coefs.csv", sep=""), row.names=FALSE)

	# Derive the absolute values of the coefficients for
	# The real data and
	all.chi.coefs <- abs(all.chi.coefs[,1:(length(all.chi.coefs)-1)])
	all.adu.coefs <- abs(all.adu.coefs[,1:(length(all.adu.coefs)-1)])
	
	# Print, for each model, how many random coefficients
	# are less than the real data coefficients (with absolute values)
	print("Child model: ")
	for (i in 1:length(all.chi.coefs)) {
		pred <- names(all.chi.coefs)[i]
		trueval <- as.numeric(all.chi.coefs[nrow(all.chi.coefs),i])
		percunder <- (1 - sum(all.chi.coefs[(1:nrow(all.chi.coefs)-1),i] > trueval)/
		             (nrow(all.chi.coefs)-1))*100
		print(paste(pred, " absB=", round(trueval,3),
		            ";more than ", round(percunder,3),
		            "% of random absBs.", sep=""))
	}
	
	print("Adult model: ")
	for (i in 1:length(all.adu.coefs)) {
		pred <- names(all.adu.coefs)[i]
		trueval <- as.numeric(all.adu.coefs[nrow(all.adu.coefs),i])
		percunder <- (1 - sum(all.adu.coefs[(1:nrow(all.adu.coefs)-1),i] > trueval)/
		             (nrow(all.adu.coefs)-1))*100
		print(paste(pred, " absB=", round(trueval,3),
		            "; more than ", round(percunder,3),
		            "% of random absBs.", sep=""))
	}
}
