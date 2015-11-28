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
model.path <- "processed_data/models/"
plot.path <- "plots/" #"plots/"

# Read in supplemental data and prepped data
gap.info <- fread(paste(info.path,"GapInfo.csv", sep=""))
subject.info <- fread(paste(info.path,"SubjectInfo.csv",sep=""))
vid.info <- fread(paste(info.path,"VideoSegmentInfo.csv",sep=""))

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
	Gap = rep(as.integer(0), N),
	Switch = rep(FALSE, N),
	Start = rep(0, N),
	Run = rep(as.integer(0),N))
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

# Write combined data
write.csv(switch.all, paste(processed.data.path,
	"SwitchAll.csv", sep=""), row.names=FALSE)


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


###### DIFF BETWEEN RANDOM AND TRANSITION w/ TYPE  #############################
mean.agg <- aggregate(Switch ~ Condition + Sample + Type + Age, switch.all, mean)
langs <- mean.agg$Condition
ages <- mean.agg$Age
ages[ages == "21"] <- "A"
samples <- mean.agg$Sample
types <- mean.agg$Type
means <- mean.agg$Switch
errs <- aggregate(Switch ~ Condition + Sample + Type + Age, switch.all, sem)$Switch
linecolors <- rep(c(
	"Random (all)", "Random (all)",
	"Random (all)", "Random (all)",
	"No speech", "Prosody only",
	"Normal speech", "Words only"),14)

df <- data.frame(
    Age = factor(ages),
    Sample = factor(samples),
    Type = factor(types),
    Condition = factor(langs),
    m = means,
    se = errs,
    ConditionL = factor(linecolors)
)
df$Sample <- factor(df$Sample, levels=rev(levels(df$Sample)))
df$Condition <- factor(df$Condition, labels=c("No speech", "Prosody only",
    "Normal speech", "Words only"))
df$Type <- factor(df$Type, labels=c("Question", "Non-Question"))
df$Condition <- factor(df$Condition, levels=c("Normal speech", "Prosody only",
    "Words only", "No speech"))
df$ConditionL <- factor(df$ConditionL, levels=c("Normal speech", "Prosody only",
    "Words only", "No speech", "Random (all)"))

limits <- aes(ymax = m + se, ymin= m - se)
dodge <- position_dodge(width=0.9)

p2 <- qplot(Age,m,facets = . ~ Condition, group=interaction(Sample,Type),
	ylim=c(-0.1,0.6), ymin= m - se, ymax= m + se,
	color= ConditionL, linetype=Type,
    xlab="Age (years)",ylab="Proportion gaze switches",
    position=position_dodge(width=.1), data=df) +
	geom_line(size=2, position=position_dodge(width=.1)) +
	geom_pointrange(size=1, position=position_dodge(width=.1)) +
    scale_colour_manual(name="Condition",
    	values=c("firebrick2", "forestgreen", "green3", "dodgerblue3", "gray26")) +
    scale_linetype_manual(name="Sample", values=lines) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20", vjust=2),
    plot.background = element_rect(fill = "transparent",colour = NA),
    strip.text.x = element_text(size=30, color="gray20"),
#    legend.justification=c(1,0), legend.position=c(1,0),
    legend.title = element_text(colour="gray20", size=24),
    legend.text = element_text(colour="gray20", size=26),
    legend.key.width = unit(4, "lines"), legend.key.height = unit(2, "lines"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "samples-by-lang-groups-trans-types.png", sep=""),
    width=2000,height=600,units="px", bg = "transparent")
print(p2)
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
switch.real$Cond2 <- ifelse((switch.real$Condition == "normal" |
	switch.real$Condition == "robot"),"lexical", "non-lexical")
switch.real.C <- filter(switch.real, Age != 21) %>%
                 mutate(Age = as.numeric(Age))
switch.real.A <- filter(switch.real, Age == 21)

# Models
# Children
# With all four conditions
chi.max <- glmer(Switch ~ Age * Condition * Type + Duration + 
    (Type|Subject) + (1|Gap), data=switch.real.C,
    family=binomial, control = glmerControl(optimizer="bobyqa"))
#	Notes:
#		- Model does not converge with Condition interaction random slope

# Adults
adu.max <- glmer(Switch ~ Condition * Type + Duration + 
    (Type+Condition|Subject) + (1|Gap), data=switch.real.A,
    family=binomial, control = glmerControl(optimizer="bobyqa"))
#	Notes:
#		- Interaction with Duration does not improve model but does increase AIC

# Export the model values for use in the random runs
ll.cm <- logLik(chi.max)
chi.max.m <- data.frame(
	B = fixef(chi.max),
	SE = SEstat(chi.max),
	t = tstat(chi.max))
chi.max.m <- cbind(Predictor = rownames(chi.max.m), chi.max.m)
rownames(chi.max.m) <- NULL
chi.max.i <- data.frame(
	LogLik = head(ll.cm), AIC = AIC(ll.cm),
	NumObs = as.numeric(unlist(attributes(ll.cm))[1]),
	Sample = "real", Error = "", Run = 0)
chi.max.r <- resid(chi.max)
write.csv(chi.max.m, paste(
	model.path, "chi.max.m-0.csv", sep=""),
	row.names=FALSE)
write.csv(chi.max.i, paste(
	model.path, "chi.max.i-0.csv", sep=""),
	row.names=FALSE)
write.csv(chi.max.r, paste(
	model.path, "chi.max.r-0.csv", sep=""),
	row.names=FALSE)

ll.am <- logLik(adu.max)
adu.max.m <- data.frame(
	B = fixef(adu.max),
	SE = SEstat(adu.max),
	t = tstat(adu.max))
adu.max.m <- cbind(Predictor = rownames(adu.max.m), adu.max.m)
rownames(adu.max.m) <- NULL
adu.max.i <- data.frame(
	LogLik = head(ll.am), AIC = AIC(ll.am),
	NumObs = as.numeric(unlist(attributes(ll.am))[1]),
	Sample = "real", Error = "", Run = 0)
adu.max.r <- resid(adu.max)
write.csv(adu.max.m, paste(
	model.path, "adu.max.m-0.csv", sep=""),
	row.names=FALSE)
write.csv(adu.max.i, paste(
	model.path, "adu.max.i-0.csv", sep=""),
	row.names=FALSE)
write.csv(adu.max.r, paste(
	model.path, "adu.max.r-0.csv", sep=""),
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

# Overall random rate comparison by age, transition type, and condition
switch.all.A <- filter(switch.all, Age == 21)
switch.all.C <- filter(switch.all, Age != 21)

runs <- unique(switch.all$Run)
nruns <- length(runs)

adusubs <- unique(switch.all.A$Subject)
aduvars <- c("Condition=normal", "Condition=robot", "Duration")
aduAvgs <- data.table(
	Variable = c(rep(aduvars[1], nruns),
		rep(aduvars[2], nruns),
		rep(aduvars[3], nruns)),
	Run = rep(as.character(runs),3))
for (sub in adusubs) {
	aduAvgs[,c(as.character(sub)) := rep(0, nrow(aduAvgs))]	
}

chisubs <- unique(switch.all.C$Subject)
chivars <- c("Duration", "Condition=muffled", "Condition=normal + Type")
chiAvgs <- data.table(
	Variable = c(rep(chivars[1], nruns),
		rep(chivars[2], nruns),
		rep(chivars[3], nruns*2)),
	Comparison = c(rep(1, nruns),
		rep(1, nruns),
		rep(1, nruns), rep(2, nruns)),
	Run = rep(as.character(runs),4))
for (sub in chisubs) {
	chiAvgs[,c(as.character(sub)) := rep(0, nrow(chiAvgs))]	
}

for (run in runs) {
	print(run)
	
	#### ADULTS ####
	currrun.A <- filter(switch.all.A, Run == run)

	# Adult: Condition=normal
	varagg <- aggregate(Switch ~ Condition + Subject, currrun.A, mean)
	subnorm <- subset(varagg, Condition == "normal")
	subbabb <- subset(varagg, Condition == "babble")
	deltas <- subnorm$Switch - subbabb$Switch
	insert <- data.frame(t(deltas))
	colnames(insert) <- adusubs
	aduAvgs[Run == run & Variable == "Condition=normal", (adusubs) := insert]
	aduAvgs[Run == run & Variable == "Condition=normal", Variable := "ConditionNB"]
	
	# Adult: Condition=robot
	subrobo <- subset(varagg, Condition == "robot")
	if (!("A-1_P09" %in% subrobo$Subject)) {
		subrobo <- rbind(subrobo, data.frame(
			Condition = "robot",
			Subject = "A-1_P09",
			Switch = NA))
			subrobo <- subrobo[order(subrobo$Subject),]
	}
	deltas <- subrobo$Switch - subbabb$Switch
	insert <- data.frame(t(deltas))
	colnames(insert) <- adusubs
	aduAvgs[Run == run & Variable == "Condition=robot", (adusubs) := insert]
	aduAvgs[Run == run & Variable == "Condition=robot", Variable := "ConditionRB"]

	# Adult: "Duration"
	varagg <- aggregate(Switch ~ Duration, currrun.A, mean)
	durcor <- cor(varagg$Switch, varagg$Duration)
	insert <- data.frame(t(rep(durcor, length(adusubs))))
	colnames(insert) <- adusubs
	aduAvgs[Run == run & Variable == "Duration", (adusubs) := insert]
	aduAvgs[Run == run & Variable == "Duration", Variable := "Duration-cor"]

	#### CHILDREN ####
	currrun.C <- filter(switch.all.C, Run == run)

	# Child: "Duration"
	varagg <- aggregate(Switch ~ Duration, currrun.C, mean)
	durcor <- cor(varagg$Switch, varagg$Duration)
	insert <- data.frame(t(rep(durcor, length(chisubs))))
	colnames(insert) <- chisubs
	chiAvgs[Run == run & Variable == "Duration", (chisubs) := insert]
	chiAvgs[Run == run & Variable == "Duration", Variable := "Duration-cor"]

	# Child: "Condition=muffled"
	varagg <- aggregate(Switch ~ Condition + Subject, currrun.C, mean)
	submuff <- subset(varagg, Condition == "muffled")
	subbabb <- subset(varagg, Condition == "babble")
	# fill in missing data with NA for the babble baseline
	babfiller <- data.frame(Condition = rep("babble", length(chisubs)),
		Subject = chisubs,
		Switch = rep(NA, length(chisubs)))
	neededbabb <- !(chisubs %in% subbabb$Subject)
	babinsert <- babfiller[neededbabb,]
	subbabb <- rbind(subbabb, babinsert)
	subbabb <- subbabb[order(subbabb$Subject),]
	# fill in missing data with NA for the muffled comparison
	muffiller <- data.frame(Condition = rep("muffled", length(chisubs)),
		Subject = chisubs,
		Switch = rep(NA, length(chisubs)))
	neededmuff <- !(chisubs %in% submuff$Subject)
	mufinsert <- muffiller[neededmuff,]
	submuff <- rbind(submuff, mufinsert)
	submuff <- submuff[order(submuff$Subject),]
	# Then continue finding the difference
	deltas <- submuff$Switch - subbabb$Switch
	insert <- data.frame(t(deltas))
	colnames(insert) <- chisubs
	chiAvgs[Run == run & Variable == "Condition=muffled", (chisubs) := insert]
	chiAvgs[Run == run & Variable == "Condition=muffled", Variable := "ConditionMB"]

	# Child: "Condition=normal + Type"
	varagg <- aggregate(Switch ~ Condition + Type + Subject, currrun.C, mean)
	subnormQ <- subset(varagg, Condition == "normal" & Type == "Q")
	subnormS <- subset(varagg, Condition == "normal" & Type == "S")
	subbabbQ <- subset(varagg, Condition == "babble" & Type == "Q")
	subbabbS <- subset(varagg, Condition == "babble" & Type == "S")
	# fill in missing data with NA for the babble baseline
	# Questions
	babfillerQ <- data.frame(Condition = rep("babble", length(chisubs)),
		Type = rep("Q", length(chisubs)),
		Subject = chisubs,
		Switch = rep(NA, length(chisubs)))
	neededbabbQ <- !(chisubs %in% subbabbQ$Subject)
	babinsertQ <- babfillerQ[neededbabbQ,]
	subbabbQ <- rbind(subbabbQ, babinsertQ)
	subbabbQ <- subbabbQ[order(subbabbQ$Subject),]
	# Non-questions
	babfillerS <- data.frame(Condition = rep("babble", length(chisubs)),
		Type = rep("S", length(chisubs)),
		Subject = chisubs,
		Switch = rep(NA, length(chisubs)))
	neededbabbS <- !(chisubs %in% subbabbS$Subject)
	babinsertS <- babfillerS[neededbabbS,]
	subbabbS <- rbind(subbabbS, babinsertS)
	subbabbS <- subbabbS[order(subbabbS$Subject),]
	# fill in missing data with NA for the muffled comparison
	# Questions
	norfillerQ <- data.frame(Condition = rep("normal", length(chisubs)),
		Type = rep("Q", length(chisubs)),
		Subject = chisubs,
		Switch = rep(NA, length(chisubs)))
	needednormQ <- !(chisubs %in% subnormQ$Subject)
	norinsertQ <- norfillerQ[needednormQ,]
	subnormQ <- rbind(subnormQ, norinsertQ)
	subnormQ <- subnormQ[order(subnormQ$Subject),]
	# Non-questions
	norfillerS <- data.frame(Condition = rep("normal", length(chisubs)),
		Type = rep("S", length(chisubs)),
		Subject = chisubs,
		Switch = rep(NA, length(chisubs)))
	needednormS <- !(chisubs %in% subnormS$Subject)
	norinsertS <- norfillerS[needednormS,]
	subnormS <- rbind(subnormS, norinsertS)
	subnormS <- subnormS[order(subnormS$Subject),]
	# Then continue finding the difference
	deltasN <- subnormQ$Switch - subnormS$Switch
	insertN <- data.frame(t(deltasN))
	colnames(insertN) <- chisubs
	chiAvgs[Run == run & Variable == "Condition=normal + Type" &
		Comparison == 1, (chisubs) := insertN]
	chiAvgs[Run == run & Variable == "Condition=normal + Type" &
		Comparison == 1, Variable := "ConditionN:QS"]
	deltasB <- subbabbQ$Switch - subbabbS$Switch
	insertB <- data.frame(t(deltasB))
	colnames(insertB) <- chisubs
	chiAvgs[Run == run & Variable == "Condition=normal + Type" &
		Comparison == 2, (chisubs) := insertB]
	chiAvgs[Run == run & Variable == "Condition=normal + Type" &
		Comparison == 2, Variable := "ConditionB:QS"]
}

aduAvgs.bu <- aduAvgs
chiAvgs.bu <- chiAvgs

aduAvgs[, Mean := rowMeans(aduAvgs[, adusubs, with=F], na.rm=TRUE)]
chiAvgs[, Mean := rowMeans(chiAvgs[, chisubs, with=F], na.rm=TRUE)]

# Distribution plots and percentages

## Adults
rand.data.A <- aduAvgs[Run > 0]
real.data.A <- aduAvgs[Run == 0]

variables.A <- unique(real.data.A$Variable)
for (variable in variables.A) {
	randvals <- abs(rand.data.A[Variable == variable, Mean])
	realval <- abs(real.data.A[Variable == variable, Mean])
	real.data.A[Variable == variable, GreaterThan := mean(randvals < realval)]
}

avars <- ggplot(rand.data.A, aes(x=Mean)) +
	facet_grid(.~Variable)+
	geom_density(aes(y=..scaled..), alpha=0.3) +
	ylab("Density") + xlab("Mean difference score") +
	geom_point(data=real.data.A, aes(x=Mean, y=0), size=6) +
	geom_text(data=real.data.A, aes(x=-0.2,y=0.95,
		label=paste(round(GreaterThan*100,2),"%", sep="")),
		size=10) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),     
    strip.text.x = element_text(size=20, color="gray20"),
    legend.justification=c(1,0), legend.position=c(1,0),
    legend.title = element_text(colour="gray20", size=24),
    legend.text = element_text(colour="gray20", size=26),
    legend.key.width = unit(4, "lines"), legend.key.height = unit(2, "lines"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "adult-randvsreal-ttest.png", sep=""),
    width=1300,height=600,units="px", bg = "transparent")
print(avars)
dev.off()

write.csv(rand.data.A, paste(processed.data.path,"summary.random.A.csv", sep=""))
write.csv(real.data.A, paste(processed.data.path,"summary.real.A.csv", sep=""))


## Children
rand.data.C <- chiAvgs[Run > 0]
real.data.C <- chiAvgs[Run == 0]

# Prep 2-way interaction
intlevel1 <- "ConditionN:QS"
intlevel2 <- "ConditionB:QS"
Int1 <- getDiffs(intlevel1, intlevel2, rand.data.C, chisubs, "Condition + Type N:QS-B:QS")
rand.data.C <- rand.data.C[Variable != intlevel1 & Variable != intlevel2]
rand.data.C <- rbind(rand.data.C, Int1)
Int1 <- getDiffs(intlevel1, intlevel2, real.data.C, chisubs, "Condition + Type N:QS-B:QS")
real.data.C <- real.data.C[Variable != intlevel1 & Variable != intlevel2]
real.data.C <- rbind(real.data.C, Int1)

# Prep age interactions
chisubinfo <- subject.info[AgeGroup == "CHI",c("Subject", "Age"), with=F]
chisubinfo <- subset(chisubinfo, Age < 7 & Subject != "C-2_P31" & Subject != "C-2_P16" & Subject != "C-1_P26")

# real data
y1 <- chisubinfo[Age == 1, "Subject", with=F]$Subject
y2 <- chisubinfo[Age == 2, "Subject", with=F]$Subject
y3 <- chisubinfo[Age == 3, "Subject", with=F]$Subject
y4 <- chisubinfo[Age == 4, "Subject", with=F]$Subject
y5 <- chisubinfo[Age == 5, "Subject", with=F]$Subject
y6 <- chisubinfo[Age == 6, "Subject", with=F]$Subject

avg1 <- rowMeans(rand.data.C[Variable == "ConditionMB", (y1), with=F], na.rm=T)
avg2 <- rowMeans(rand.data.C[Variable == "ConditionMB", (y2), with=F], na.rm=T)
avg3 <- rowMeans(rand.data.C[Variable == "ConditionMB", (y3), with=F], na.rm=T)
avg4 <- rowMeans(rand.data.C[Variable == "ConditionMB", (y4), with=F], na.rm=T)
avg5 <- rowMeans(rand.data.C[Variable == "ConditionMB", (y5), with=F], na.rm=T)
avg6 <- rowMeans(rand.data.C[Variable == "ConditionMB", (y6), with=F], na.rm=T)

newrows1 <- rand.data.C[Variable == "ConditionMB"]
newrows1$Variable <- "ConditionMB:65"
newrows1$Mean <- avg6 - avg5

newrows2 <- rand.data.C[Variable == "ConditionMB"]
newrows2$Variable <- "ConditionMB:64"
newrows2$Mean <- avg6 - avg4

newrows3 <- rand.data.C[Variable == "ConditionMB"]
newrows3$Variable <- "ConditionMB:63"
newrows3$Mean <- avg6 - avg3

newrows4 <- rand.data.C[Variable == "ConditionMB"]
newrows4$Variable <- "ConditionMB:62"
newrows4$Mean <- avg6 - avg2

newrows5 <- rand.data.C[Variable == "ConditionMB"]
newrows5$Variable <- "ConditionMB:61"
newrows5$Mean <- avg6 - avg1

newrows6 <- rand.data.C[Variable == "ConditionMB"]
newrows6$Variable <- "ConditionMB:54"
newrows6$Mean <- avg5 - avg4

newrows7 <- rand.data.C[Variable == "ConditionMB"]
newrows7$Variable <- "ConditionMB:53"
newrows7$Mean <- avg5 - avg3

newrows8 <- rand.data.C[Variable == "ConditionMB"]
newrows8$Variable <- "ConditionMB:52"
newrows8$Mean <- avg5 - avg3

newrows9 <- rand.data.C[Variable == "ConditionMB"]
newrows9$Variable <- "ConditionMB:51"
newrows9$Mean <- avg5 - avg1

newrows10 <- rand.data.C[Variable == "ConditionMB"]
newrows10$Variable <- "ConditionMB:43"
newrows10$Mean <- avg4 - avg3

newrows11 <- rand.data.C[Variable == "ConditionMB"]
newrows11$Variable <- "ConditionMB:42"
newrows11$Mean <- avg4 - avg2

newrows12 <- rand.data.C[Variable == "ConditionMB"]
newrows12$Variable <- "ConditionMB:41"
newrows12$Mean <- avg4 - avg1

newrows13 <- rand.data.C[Variable == "ConditionMB"]
newrows13$Variable <- "ConditionMB:32"
newrows13$Mean <- avg3 - avg2

newrows14 <- rand.data.C[Variable == "ConditionMB"]
newrows14$Variable <- "ConditionMB:31"
newrows14$Mean <- avg3 - avg1

newrows15 <- rand.data.C[Variable == "ConditionMB"]
newrows15$Variable <- "ConditionMB:21"
newrows15$Mean <- avg2 - avg1

rand.data.C <- rbind(rand.data.C, newrows1, newrows2, newrows3,
	newrows4, newrows5, newrows6, newrows7, newrows8, newrows9,
	newrows10, newrows11, newrows12, newrows13, newrows14, newrows15)

# real data
avg1 <- rowMeans(real.data.C[Variable == "ConditionMB", (y1), with=F], na.rm=T)
avg2 <- rowMeans(real.data.C[Variable == "ConditionMB", (y2), with=F], na.rm=T)
avg3 <- rowMeans(real.data.C[Variable == "ConditionMB", (y3), with=F], na.rm=T)
avg4 <- rowMeans(real.data.C[Variable == "ConditionMB", (y4), with=F], na.rm=T)
avg5 <- rowMeans(real.data.C[Variable == "ConditionMB", (y5), with=F], na.rm=T)
avg6 <- rowMeans(real.data.C[Variable == "ConditionMB", (y6), with=F], na.rm=T)

newrows1 <- real.data.C[Variable == "ConditionMB"]
newrows1$Variable <- "ConditionMB:65"
newrows1$Mean <- avg6 - avg5

newrows2 <- real.data.C[Variable == "ConditionMB"]
newrows2$Variable <- "ConditionMB:64"
newrows2$Mean <- avg6 - avg4

newrows3 <- real.data.C[Variable == "ConditionMB"]
newrows3$Variable <- "ConditionMB:63"
newrows3$Mean <- avg6 - avg3

newrows4 <- real.data.C[Variable == "ConditionMB"]
newrows4$Variable <- "ConditionMB:62"
newrows4$Mean <- avg6 - avg2

newrows5 <- real.data.C[Variable == "ConditionMB"]
newrows5$Variable <- "ConditionMB:61"
newrows5$Mean <- avg6 - avg1

newrows6 <- real.data.C[Variable == "ConditionMB"]
newrows6$Variable <- "ConditionMB:54"
newrows6$Mean <- avg5 - avg4

newrows7 <- real.data.C[Variable == "ConditionMB"]
newrows7$Variable <- "ConditionMB:53"
newrows7$Mean <- avg5 - avg3

newrows8 <- real.data.C[Variable == "ConditionMB"]
newrows8$Variable <- "ConditionMB:52"
newrows8$Mean <- avg5 - avg3

newrows9 <- real.data.C[Variable == "ConditionMB"]
newrows9$Variable <- "ConditionMB:51"
newrows9$Mean <- avg5 - avg1

newrows10 <- real.data.C[Variable == "ConditionMB"]
newrows10$Variable <- "ConditionMB:43"
newrows10$Mean <- avg4 - avg3

newrows11 <- real.data.C[Variable == "ConditionMB"]
newrows11$Variable <- "ConditionMB:42"
newrows11$Mean <- avg4 - avg2

newrows12 <- real.data.C[Variable == "ConditionMB"]
newrows12$Variable <- "ConditionMB:41"
newrows12$Mean <- avg4 - avg1

newrows13 <- real.data.C[Variable == "ConditionMB"]
newrows13$Variable <- "ConditionMB:32"
newrows13$Mean <- avg3 - avg2

newrows14 <- real.data.C[Variable == "ConditionMB"]
newrows14$Variable <- "ConditionMB:31"
newrows14$Mean <- avg3 - avg1

newrows15 <- real.data.C[Variable == "ConditionMB"]
newrows15$Variable <- "ConditionMB:21"
newrows15$Mean <- avg2 - avg1

real.data.C <- rbind(real.data.C, newrows1, newrows2, newrows3,
	newrows4, newrows5, newrows6, newrows7, newrows8, newrows9,
	newrows10, newrows11, newrows12, newrows13, newrows14, newrows15)


# Age interaction with Condition + Type
avg1 <- rowMeans(rand.data.C[Variable == "Condition + Type N:QS-B:QS", (y1), with=F], na.rm=T)
avg2 <- rowMeans(rand.data.C[Variable == "Condition + Type N:QS-B:QS", (y2), with=F], na.rm=T)
avg3 <- rowMeans(rand.data.C[Variable == "Condition + Type N:QS-B:QS", (y3), with=F], na.rm=T)
avg4 <- rowMeans(rand.data.C[Variable == "Condition + Type N:QS-B:QS", (y4), with=F], na.rm=T)
avg5 <- rowMeans(rand.data.C[Variable == "Condition + Type N:QS-B:QS", (y5), with=F], na.rm=T)
avg6 <- rowMeans(rand.data.C[Variable == "Condition + Type N:QS-B:QS", (y6), with=F], na.rm=T)

newrows1 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows1$Variable <- "Condition + Type N:QS-B:QS:65"
newrows1$Mean <- avg6 - avg5

newrows2 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows2$Variable <- "Condition + Type N:QS-B:QS:64"
newrows2$Mean <- avg6 - avg4

newrows3 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows3$Variable <- "Condition + Type N:QS-B:QS:63"
newrows3$Mean <- avg6 - avg3

newrows4 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows4$Variable <- "Condition + Type N:QS-B:QS:62"
newrows4$Mean <- avg6 - avg2

newrows5 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows5$Variable <- "Condition + Type N:QS-B:QS:61"
newrows5$Mean <- avg6 - avg1

newrows6 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows6$Variable <- "Condition + Type N:QS-B:QS:54"
newrows6$Mean <- avg5 - avg4

newrows7 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows7$Variable <- "Condition + Type N:QS-B:QS:53"
newrows7$Mean <- avg5 - avg3

newrows8 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows8$Variable <- "Condition + Type N:QS-B:QS:52"
newrows8$Mean <- avg5 - avg3

newrows9 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows9$Variable <- "Condition + Type N:QS-B:QS:51"
newrows9$Mean <- avg5 - avg1

newrows10 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows10$Variable <- "Condition + Type N:QS-B:QS:43"
newrows10$Mean <- avg4 - avg3

newrows11 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows11$Variable <- "Condition + Type N:QS-B:QS:42"
newrows11$Mean <- avg4 - avg2

newrows12 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows12$Variable <- "Condition + Type N:QS-B:QS:41"
newrows12$Mean <- avg4 - avg1

newrows13 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows13$Variable <- "Condition + Type N:QS-B:QS:32"
newrows13$Mean <- avg3 - avg2

newrows14 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows14$Variable <- "Condition + Type N:QS-B:QS:31"
newrows14$Mean <- avg3 - avg1

newrows15 <- rand.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows15$Variable <- "Condition + Type N:QS-B:QS:21"
newrows15$Mean <- avg2 - avg1

rand.data.C <- rbind(rand.data.C, newrows1, newrows2, newrows3,
	newrows4, newrows5, newrows6, newrows7, newrows8, newrows9,
	newrows10, newrows11, newrows12, newrows13, newrows14, newrows15)

avg1 <- rowMeans(real.data.C[Variable == "Condition + Type N:QS-B:QS", (y1), with=F], na.rm=T)
avg2 <- rowMeans(real.data.C[Variable == "Condition + Type N:QS-B:QS", (y2), with=F], na.rm=T)
avg3 <- rowMeans(real.data.C[Variable == "Condition + Type N:QS-B:QS", (y3), with=F], na.rm=T)
avg4 <- rowMeans(real.data.C[Variable == "Condition + Type N:QS-B:QS", (y4), with=F], na.rm=T)
avg5 <- rowMeans(real.data.C[Variable == "Condition + Type N:QS-B:QS", (y5), with=F], na.rm=T)
avg6 <- rowMeans(real.data.C[Variable == "Condition + Type N:QS-B:QS", (y6), with=F], na.rm=T)

newrows1 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows1$Variable <- "Condition + Type N:QS-B:QS:65"
newrows1$Mean <- avg6 - avg5

newrows2 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows2$Variable <- "Condition + Type N:QS-B:QS:64"
newrows2$Mean <- avg6 - avg4

newrows3 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows3$Variable <- "Condition + Type N:QS-B:QS:63"
newrows3$Mean <- avg6 - avg3

newrows4 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows4$Variable <- "Condition + Type N:QS-B:QS:62"
newrows4$Mean <- avg6 - avg2

newrows5 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows5$Variable <- "Condition + Type N:QS-B:QS:61"
newrows5$Mean <- avg6 - avg1

newrows6 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows6$Variable <- "Condition + Type N:QS-B:QS:54"
newrows6$Mean <- avg5 - avg4

newrows7 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows7$Variable <- "Condition + Type N:QS-B:QS:53"
newrows7$Mean <- avg5 - avg3

newrows8 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows8$Variable <- "Condition + Type N:QS-B:QS:52"
newrows8$Mean <- avg5 - avg3

newrows9 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows9$Variable <- "Condition + Type N:QS-B:QS:51"
newrows9$Mean <- avg5 - avg1

newrows10 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows10$Variable <- "Condition + Type N:QS-B:QS:43"
newrows10$Mean <- avg4 - avg3

newrows11 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows11$Variable <- "Condition + Type N:QS-B:QS:42"
newrows11$Mean <- avg4 - avg2

newrows12 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows12$Variable <- "Condition + Type N:QS-B:QS:41"
newrows12$Mean <- avg4 - avg1

newrows13 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows13$Variable <- "Condition + Type N:QS-B:QS:32"
newrows13$Mean <- avg3 - avg2

newrows14 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows14$Variable <- "Condition + Type N:QS-B:QS:31"
newrows14$Mean <- avg3 - avg1

newrows15 <- real.data.C[Variable == "Condition + Type N:QS-B:QS"]
newrows15$Variable <- "Condition + Type N:QS-B:QS:21"
newrows15$Mean <- avg2 - avg1

real.data.C <- rbind(real.data.C, newrows1, newrows2, newrows3,
	newrows4, newrows5, newrows6, newrows7, newrows8, newrows9,
	newrows10, newrows11, newrows12, newrows13, newrows14, newrows15)

real.data.C[, GreaterThan := 0]
rand.data.C <- rand.data.C[Variable != "Condition + Type N:QS-B:QS" & Variable != "ConditionMB"]
real.data.C <- real.data.C[Variable != "Condition + Type N:QS-B:QS" & Variable != "ConditionMB"]
rand.data.C$Variable <- factor(rand.data.C$Variable, level=sort(unique(rand.data.C$Variable)))
real.data.C$Variable <- factor(real.data.C$Variable, level=sort(unique(real.data.C$Variable)))


variables.C <- unique(real.data.C$Variable)
for (variable in variables.C) {
	randvals <- abs(rand.data.C[Variable == variable, Mean])
	realval <- abs(real.data.C[Variable == variable, Mean])
	real.data.C[Variable == variable, GreaterThan := mean(randvals < realval)]
}

cvars <- ggplot(rand.data.C, aes(x=Mean)) +
	facet_grid(.~Variable)+
	geom_density(aes(y=..scaled..), alpha=0.3) +
	ylab("Density") + xlab("Mean difference score") +
	geom_point(data=real.data.C, aes(x=Mean, y=0), size=6) +
	geom_text(data=real.data.C, aes(x=-0.2,y=0.95,
		label=paste(round(GreaterThan*100,2),"%", sep="")),
		size=10) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),     
    strip.text.x = element_text(size=20, color="gray20"),
    legend.justification=c(1,0), legend.position=c(1,0),
    legend.title = element_text(colour="gray20", size=24),
    legend.text = element_text(colour="gray20", size=26),
    legend.key.width = unit(4, "lines"), legend.key.height = unit(2, "lines"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "child-randvsreal-ttest.png", sep=""),
    width=10000,height=600,units="px", bg = "transparent")
print(cvars)
dev.off()

write.csv(rand.data.C, paste(processed.data.path,"summary.random.C.csv", sep=""))
write.csv(real.data.C, paste(processed.data.path,"summary.real.C.csv", sep=""))

real.data.C.ages <- real.data.C[Variable == "ConditionMB:54" |
	Variable == "ConditionMB:61" | Variable == "ConditionMB:62" |
	Variable == "ConditionMB:63" | Variable == "ConditionMB:64"]
rand.data.C.ages <- rand.data.C[Variable == "ConditionMB:54" |
	Variable == "ConditionMB:61" | Variable == "ConditionMB:62" |
	Variable == "ConditionMB:63" | Variable == "ConditionMB:64"]
real.data.C.ages$Variable <- factor(
	real.data.C.ages$Variable, labels=c("Muffled: Age 5 - Age 4",
	"Muffled: Age 6 - Age 1", "Muffled: Age 6 - Age 2",
	"Muffled: Age 6 - Age 3", "Muffled: Age 6 - Age 4"))
rand.data.C.ages$Variable <- factor(
	rand.data.C.ages$Variable, labels=c("Muffled: Age 5 - Age 4",
	"Muffled: Age 6 - Age 1", "Muffled: Age 6 - Age 2",
	"Muffled: Age 6 - Age 3", "Muffled: Age 6 - Age 4"))

real.data.C.ages[, GreaterThan := 0]
variables.C <- unique(real.data.C.ages$Variable)
for (variable in variables.C) {
	randvals <- abs(rand.data.C.ages[Variable == variable, Mean])
	realval <- abs(real.data.C.ages[Variable == variable, Mean])
	real.data.C.ages[Variable == variable, GreaterThan := mean(randvals < realval)]
}
muffledages <- ggplot(rand.data.C.ages, aes(x=Mean)) +
	facet_grid(.~Variable)+
	geom_density(aes(y=..scaled..), alpha=0.3) +
	ylab("Density") + xlab("Mean difference score") +
	geom_point(data= real.data.C.ages, aes(x=Mean, y=0), size=6) +
	geom_text(data= real.data.C.ages, aes(x=-0.15,y=0.95,
		label=paste(round(GreaterThan*100,2),"%", sep="")),
		size=10) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),     
    strip.text.x = element_text(size=20, color="gray20"),
    legend.justification=c(1,0), legend.position=c(1,0),
    legend.title = element_text(colour="gray20", size=24),
    legend.text = element_text(colour="gray20", size=26),
    legend.key.width = unit(4, "lines"), legend.key.height = unit(2, "lines"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "child-randvsreal-ttest-muffledages.png", sep=""),
    width=1700,height=600,units="px", bg = "transparent")
print(muffledages)
dev.off()

real.data.C.ages <- real.data.C[Variable == "Condition + Type N:QS-B:QS:41" |
	Variable == "Condition + Type N:QS-B:QS:42" | Variable == "Condition + Type N:QS-B:QS:54" |
	Variable == "Condition + Type N:QS-B:QS:61" | Variable == "Condition + Type N:QS-B:QS:62" |
	Variable == "Condition + Type N:QS-B:QS:65"]
rand.data.C.ages <- rand.data.C[Variable == "Condition + Type N:QS-B:QS:41" |
	Variable == "Condition + Type N:QS-B:QS:42" | Variable == "Condition + Type N:QS-B:QS:54" |
	Variable == "Condition + Type N:QS-B:QS:61" | Variable == "Condition + Type N:QS-B:QS:62" |
	Variable == "Condition + Type N:QS-B:QS:65"]
real.data.C.ages$Variable <- factor(
	real.data.C.ages$Variable, labels=c("Normal*Type: Age 4 - Age 1",
	"Normal*Type: Age 4 - Age 2", "Normal*Type: Age 5 - Age 4",
	"Normal*Type: Age 6 - Age 1", "Normal*Type: Age 6 - Age 2",
	"Normal*Type: Age 6 - Age 5"))
rand.data.C.ages$Variable <- factor(
	rand.data.C.ages$Variable, labels=c("Normal*Type: Age 4 - Age 1",
	"Normal*Type: Age 4 - Age 2", "Normal*Type: Age 5 - Age 4",
	"Normal*Type: Age 6 - Age 1", "Normal*Type: Age 6 - Age 2",
	"Normal*Type: Age 6 - Age 5"))

real.data.C.ages[, GreaterThan := 0]
variables.C <- unique(real.data.C.ages$Variable)
for (variable in variables.C) {
	randvals <- abs(rand.data.C.ages[Variable == variable, Mean])
	realval <- abs(real.data.C.ages[Variable == variable, Mean])
	real.data.C.ages[Variable == variable, GreaterThan := mean(randvals < realval)]
}
normalages <- ggplot(rand.data.C.ages, aes(x=Mean)) +
	facet_grid(.~Variable)+
	geom_density(aes(y=..scaled..), alpha=0.3) +
	ylab("Density") + xlab("Mean difference score") +
	geom_point(data= real.data.C.ages, aes(x=Mean, y=0), size=6) +
	geom_text(data= real.data.C.ages, aes(x=-0.25,y=0.95,
		label=paste(round(GreaterThan*100,2),"%", sep="")),
		size=10) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),     
    strip.text.x = element_text(size=20, color="gray20"),
    legend.justification=c(1,0), legend.position=c(1,0),
    legend.title = element_text(colour="gray20", size=24),
    legend.text = element_text(colour="gray20", size=26),
    legend.key.width = unit(4, "lines"), legend.key.height = unit(2, "lines"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "child-randvsreal-ttest-normaltypesages.png", sep=""),
    width=2000,height=600,units="px", bg = "transparent")
print(normalages)
dev.off()
