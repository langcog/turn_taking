rm(list = ls())
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
raw.data.path <- "tracker_data/"
info.path <- "info/"
processed.data.path <- "processed_data/"
plot.path <- "plots/"

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
N <- length(files) * 3737 # maximum length of a switch.final.rand csv file
switch.final.r <- data.table(
	Segment = rep("", N),
	StimulusGroup = rep("", N),
	Condition = rep("", N),
	Conversation = rep("", N),
	Subject = rep("", N),
	SubjectNum = rep(0, N),
	TestDate = rep("", N),
	TestTime = rep("", N),
	Gender = rep("", N),
	Birthdate = rep("", N),
	AgeMonths = rep(0, N),
	Age = rep(0, N),
	AgeGroup = rep("", N),
	include = rep(0, N),
	includemonoling = rep(0, N),
	OldSubject = rep("", N),
	Gap = rep(0, N),
	Duration = rep(0, N),
	Offset = rep(0, N),
	Onset = rep(0, N),
	QType = rep("", N),
	SpeakerNext = rep("", N),
	SpeakerPrev = rep("", N),
	TranscriptNext = rep("", N),
	TranscriptPrev = rep("", N),
	Type = rep("", N),
	Switch = rep(0, N),
	Start.Time = rep(0, N),
	Sample = rep("", N),
	Ttype = rep("", N),
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
setkeyv(switch.final.r, c("Run", "Subject", "Segment", "Gap"))
# Write it out
write.csv(switch.final.r, paste(processed.data.path,
	"random.runs.csv",sep=""), row.names=FALSE)

# Read in real transition data and set Run number (0)
switch.final <- fread(paste(processed.data.path, "switch.final.csv", sep=""))
switch.final[, Run := 0]

# Combine the two data sets (real and random)
switch.all <- rbind(switch.final, switch.final.r)


################################################################################
# Create plots
################################################################################
###### DIFF BETWEEN RANDOM AND TRANSITION SAMPLE SETS ##########################
errs.agg <- aggregate(Switch ~ Condition + Sample + Age,
    switch.all, sem)
errs <- errs.agg$Switch
means.agg <- aggregate(Switch ~ Condition + Sample + Age,
    switch.all, mean)
means <- means.agg$Switch
ages <- as.character(means.agg$Age)
ages[ages == "21"] <- "A"
conds <- means.agg$Condition
samples <- means.agg$Sample
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
df$Sample <- factor(df$Sample, labels=c("Transition", "Random"))
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


###### CORRECTED CONDITION MEANS ###############################################
# get random values to subtract from transition values
means.agg.r <- subset(means.agg, Sample == "RANDOM")
means.agg.t <- subset(means.agg, Sample == "TRANSITION")
errs <- subset(errs.agg, Sample == "TRANSITION")$Switch
means <- means.agg.t$Switch - means.agg.r$Switch
ages <- as.character(means.agg.t$Age)
ages[ages == "21"] <- "A"
conds <- means.agg.t$Condition

df <- data.frame(
    Age = factor(ages),
    Condition = factor(conds),
    m = means,
    se = errs
)
df$Condition <- factor(df$Condition, labels=c("No speech", "Prosody only",
    "Normal speech", "Words only"))
df$Condition <- factor(df$Condition, levels=c("Normal speech", "Prosody only",
    "Words only", "No speech"))

limits <- aes(ymax = m + se, ymin= m - se)

p2 <- qplot(Age,m, group=Condition, ylim=c(-0.1,0.6),
    ymin= m - se, ymax= m + se, color=Condition,
    xlab="Age (years)", ylab="Baseline-corrected proportion switches",
    position=position_dodge(width=.1), data=df) +
	geom_line(size=2, position=position_dodge(width=.1)) +
	geom_pointrange(size=1, position=position_dodge(width=.1)) +
    scale_colour_manual(name = "Condition", values=stoplightPalette) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position = "none", plot.margin=unit(c(10,10,10,10),"mm"))      
png(paste(plot.path, "all-conditions.png", sep=""),
    width=900,height=700,units="px", bg = "transparent")
print(p2)
dev.off()


p2bars <- qplot(Age,m, group=Condition, ylim=c(-0.1,0.6),
    ymin= m - se, ymax= m + se, fill=Condition,
    xlab="Age (years)", ylab="Baseline-corrected proportion switches",
    geom="bar", stat="identity", position=position_dodge(), data=df) +
    geom_errorbar(limits, position=position_dodge(width=0.9), width=0.25) +
    scale_fill_manual(name="", values=stoplightPalette) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.justification=c(0.05,0.95), legend.position=c(0.05,0.95),
    legend.text = element_text(colour="gray20", size=26),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    legend.key.height = unit(12, "mm"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "all-conditions-bars.png", sep=""),
    width=900,height=700,units="px", bg = "transparent")
print(p2bars)
dev.off()


###### BY CONDITION & CONDENSED AGE & Q-EFFECTS SWITCHES #######################
errs.agg <- aggregate(Switch ~ Ttype + Condition + Age,
    switch.final, sem)
errs <- errs.agg$Switch
means.agg <- aggregate(Switch ~ Ttype + Condition + Age,
    switch.final, mean)
means <- means.agg$Switch
ages <- as.character(means.agg$Age)
ages[ages == "21"] <- "A"
conds <- means.agg$Condition
ttypes <- means.agg$Ttype

df <- data.frame(
    Age = factor(ages),
    Condition = factor(conds),
    Transition = factor(ttypes),
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
# Create a releveled version of the conditions (to compare effects with both
# the babble and normal conditions as the reference level)
switch.final$ConditionN <- factor(switch.final$Condition, levels=c("normal",
    "muffled", "robot", "babble"))
# Split the data into adults and children for separate models
switch.final.C <- subset(switch.final, Age < 7)
switch.final.A <- subset(switch.final, Age == 21)

# Models
# Children
chi.max.b <- glmer(Switch ~ Age * Condition * Ttype + Duration + 
    (Ttype|Subject) + (1|Gap), data=switch.final.C, family=binomial,
    control = glmerControl(optimizer="bobyqa"))
chi.b.coefs <- data.frame(t(sapply(fixef(chi.max.b),c)))

chi.max.n <- glmer(Switch ~ Age * ConditionN * Ttype + Duration + 
    (Ttype|Subject) + (1|Gap), data=switch.final.C, family=binomial,
    control = glmerControl(optimizer="bobyqa"))
chi.n.coefs <- data.frame(t(sapply(fixef(chi.max.n),c)))

# Adults
adu.max.b <- glmer(Switch ~ Condition * Ttype + Duration + 
    (Ttype|Subject) + (1|Gap), data=switch.final.A, family=binomial,
    control = glmerControl(optimizer="bobyqa"))
adu.b.coefs <- data.frame(t(sapply(fixef(adu.max.b),c)))

adu.max.n <- glmer(Switch ~ ConditionN * Ttype + Duration + 
    (Ttype|Subject) + (1|Gap), data=switch.final.A, family=binomial,
    control = glmerControl(optimizer="bobyqa"))
adu.n.coefs <- data.frame(t(sapply(fixef(adu.max.n),c)))

# Initialize coefficient data frames
# for collecting random run results
chi.b.coefs.r <- chi.b.coefs[0,]
adu.b.coefs.r <- adu.b.coefs[0,]
chi.n.coefs.r <- chi.n.coefs[0,]
adu.n.coefs.r <- adu.n.coefs[0,]

switch.final.r$ConditionN <- factor(switch.final.r$Condition, levels=c("normal",
	"muffled", "robot", "babble"))
switch.final.r.C <- switch.final.r[Age < 7,]
switch.final.r.A <- switch.final.r[Age == 21,]
# Number of random runs to model
NumRand <- 10#00
for (i in 1:NumRand) {
	print(i) # Update
	# Retrieve the random gap info data consistent with the random run being modeled (i)
	rand.data.C <- switch.final.r.C[Run == i,]
	rand.data.A <- switch.final.r.A[Run == i,]	
	# Models
	# Children
	chi.max.b.r <- glmer(Switch ~ Age * Condition * Ttype + Duration + 
	    (Ttype|Subject) + (1|Gap), data=rand.data.C, family=binomial,
	    control = glmerControl(optimizer="bobyqa"))
	chi.b.coefs.r <- rbind(chi.b.coefs.r, data.frame(t(sapply(fixef(chi.max.b.r),c))))
	
	chi.max.n.r <- glmer(Switch ~ Age * ConditionN * Ttype + Duration + 
	    (Ttype|Subject) + (1|Gap), data=rand.data.C, family=binomial,
	    control = glmerControl(optimizer="bobyqa"))
	chi.n.coefs.r <- rbind(chi.n.coefs.r, data.frame(t(sapply(fixef(chi.max.n.r),c))))

	# Adults
	adu.max.b.r <- glmer(Switch ~ Condition * Ttype + Duration + 
	    (Ttype|Subject) + (1|Gap), data=rand.data.A, family=binomial,
	    control = glmerControl(optimizer="bobyqa"))
	adu.b.coefs.r <- rbind(adu.b.coefs.r, data.frame(t(sapply(fixef(adu.max.b.r),c))))
	
	adu.max.n.r <- glmer(Switch ~ ConditionN * Ttype + Duration + 
	    (Ttype |Subject) + (1|Gap), data=rand.data.A, family=binomial,
	    control = glmerControl(optimizer="bobyqa"))
	adu.n.coefs.r <- rbind(adu.n.coefs.r, data.frame(t(sapply(fixef(adu.max.n.r),c))))
}

# Add a label to the coefficient tables
chi.b.coefs.r$Sample <- "random"
adu.b.coefs.r$Sample <- "random"
chi.n.coefs.r$Sample <- "random"
adu.n.coefs.r$Sample <- "random"
chi.b.coefs$Sample <- "real"
adu.b.coefs$Sample <- "real"
chi.n.coefs$Sample <- "real"
adu.n.coefs$Sample <- "real"
# And then bind them together
chi.b <- rbind(chi.b.coefs.r, chi.b.coefs)
adu.b <- rbind(adu.b.coefs.r, adu.b.coefs)
chi.n <- rbind(chi.n.coefs.r, chi.n.coefs)
adu.n <- rbind(adu.n.coefs.r, adu.n.coefs)
# And write them out
write.csv(chi.b, paste(processed.data.path, "chi.ref-babble.csv", sep=""), row.names=FALSE)
write.csv(adu.b, paste(processed.data.path, "adu.ref-babble.csv", sep=""), row.names=FALSE)
write.csv(chi.n, paste(processed.data.path, "chi.ref-normal.csv", sep=""), row.names=FALSE)
write.csv(adu.n, paste(processed.data.path, "adu.ref-normal.csv", sep=""), row.names=FALSE)

# Write out the real data model results
write.csv(summary(chi.max.b)$coefficients, paste(
	processed.data.path, "chi.ref-babble.model.csv", sep=""),
	row.names=FALSE)
write.csv(summary(chi.max.n)$coefficients, paste(
	processed.data.path, "chi.ref-normal.model.csv", sep=""),
	row.names=FALSE)
write.csv(summary(adu.max.b)$coefficients, paste(
	processed.data.path, "adu.ref-babble.model.csv", sep=""),
	row.names=FALSE)
write.csv(summary(adu.max.n)$coefficients, paste(
	processed.data.path, "adu.ref-normal.model.csv", sep=""),
	row.names=FALSE)

# Derive the absolute values of the coefficients for
# The real data and
chi.b <- abs(chi.b[,1:(length(chi.b)-1)])
adu.b <- abs(adu.b[,1:(length(adu.b)-1)])
chi.n <- abs(chi.n[,1:(length(chi.n)-1)])
adu.n <- abs(adu.n[,1:(length(adu.n)-1)])

# Check, for each model (2 ea for children and adults), how many random coefficients
# are less than the real coefficients (with absolute values), and print the results

print("Child model: 'No spech' lx condition reference level ")
for (i in 1:length(chi.b)) {
	pred <- names(chi.b)[i]
	trueval <- as.numeric(chi.b[nrow(chi.b),i])
	percunder <- (1 - sum(chi.b[(1:nrow(chi.b)-1),i] > trueval)/(nrow(chi.b)-1))*100
	print(paste(pred, " absB=", round(trueval,3), "; more than ", round(percunder,3), "% of random absBs.", sep=""))
}
print("Adult model: 'No spech' lx condition reference level ")
for (i in 1:length(adu.b)) {
	pred <- names(adu.b)[i]
	trueval <- as.numeric(adu.b[nrow(adu.b),i])
	percunder <- (1 - sum(adu.b[(1:nrow(adu.b)-1),i] > trueval)/(nrow(adu.b)-1))*100
	print(paste(pred, " absB=", round(trueval,3), "; more than ", round(percunder,3), "% of random absBs.", sep=""))
}
print("Child model: 'Normal spech' lx condition reference level ")
for (i in 1:length(chi.n)) {
	pred <- names(chi.n)[i]
	trueval <- as.numeric(chi.n[nrow(chi.n),i])
	percunder <- (1 - sum(chi.n[(1:nrow(chi.n)-1),i] > trueval)/(nrow(chi.n)-1))*100
	print(paste(pred, " absB=", round(trueval,3), "; more than ", round(percunder,3), "% of random absBs.", sep=""))
}
print("Adult model: 'Normal spech' lx condition reference level ")
for (i in 1:length(adu.n)) {
	pred <- names(adu.n)[i]
	trueval <- as.numeric(adu.n[nrow(adu.n),i])
	percunder <- (1 - sum(adu.n[(1:nrow(adu.n)-1),i] > trueval)/(nrow(adu.n)-1))*100
	print(paste(pred, " absB=", round(trueval,3), "; more than ", round(percunder,3), "% of random absBs.", sep=""))
}

# Post hoc test of difference from random in 1- and 2-year-olds'
# performance in the lex-only and pros-only conditions
# Could be improved :)
u3.r <- subset(switch.final.r, Age < 3)
u3.a <- subset(switch.final, Age < 3)
u3.lex.r <- subset(u3.r, Condition == "robot")
u3.lex.a <- subset(u3.a, Condition == "robot")
u3.pro.r <- subset(u3.r, Condition == "muffled")
u3.pro.a <- subset(u3.a, Condition == "muffled")

u3.lex.r.agg <- aggregate(Switch ~ Subject, u3.lex.r, mean)
u3.lex.a.agg <- aggregate(Switch ~ Subject, u3.lex.a, mean)
u3.pro.r.agg <- aggregate(Switch ~ Subject, u3.pro.r, mean)
u3.pro.a.agg <- aggregate(Switch ~ Subject, u3.pro.a, mean)

ttest1 <- t.test(u3.lex.r.agg$Switch, u3.lex.a.agg$Switch)
ttest2 <- t.test(u3.pro.r.agg$Switch, u3.pro.a.agg$Switch)