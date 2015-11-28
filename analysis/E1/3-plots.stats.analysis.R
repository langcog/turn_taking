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

# Color and line palettes
stoplightPalette <- c("dodgerblue3", "firebrick2")
stoplightPalette2 <- c("gray26", "dodgerblue3", "firebrick2")
stoplightPalette3 <- c("dodgerblue3", "firebrick2")
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
	data[, Run := as.integer(unlist(strsplit(file, "[.]"))[4])]
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

# Write combined data
write.csv(switch.all, paste(processed.data.path,
	"SwitchAll.csv", sep=""), row.names=FALSE)

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


###### DIFF BETWEEN RANDOM AND TRANSITION w/ TYPE  #############################
mean.agg <- aggregate(Switch ~ LgGroup + Sample + Type + Age, switch.all, mean)
langs <- mean.agg$LgGroup
ages <- mean.agg$Age
samples <- mean.agg$Sample
types <- mean.agg$Type
means <- mean.agg$Switch
errs <- aggregate(Switch ~ LgGroup + Sample + Type + Age, switch.all, sem)$Switch
linecolors <- c(
	"Random (both)", "Random (both)",
	"English", "Non-English",
	"Random (both)", "Random (both)",
	"English", "Non-English",
	"Random (both)", "Random (both)",
	"English", "Non-English",
	"Random (both)", "Random (both)",
	"English", "Non-English")

df <- data.frame(
    Age = factor(ages),
    Sample = factor(samples),
    Type = factor(types),
    Language = factor(langs),
    m = means,
    se = errs,
    Condition = factor(linecolors)
)
df$Sample <- factor(df$Sample, levels=rev(levels(df$Sample)))
df$Language <- factor(df$Language, labels=c("English", "Non-English"))

limits <- aes(ymax = m + se, ymin= m - se)
dodge <- position_dodge(width=0.9)

p2 <- qplot(Age,m,facets = . ~ Language, group=interaction(Sample,Type),
	ylim=c(-0.1,0.6), ymin= m - se, ymax= m + se,
	color=Condition, linetype=Type,
    xlab="Age (years)",ylab="Proportion gaze switches",
    position=position_dodge(width=.1), data=df) +
	geom_line(size=2, position=position_dodge(width=.1)) +
	geom_pointrange(size=1, position=position_dodge(width=.1)) +
    scale_colour_manual(name="Condition",
    	values=c("dodgerblue3", "firebrick2", "gray26")) +
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
    width=1200,height=600,units="px", bg = "transparent")
print(p2)
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
    (Type * LgGroup|Subject) + (1|Gap), data=switch.real.C,
    family=binomial, control = glmerControl(optimizer="bobyqa"))
#	Notes:
#		- Random slopes maximized but don't make qualitative difference
#		- Does not converge w/ duration interaction in fixed effects

# Adults
adu.max <- glmer(Switch ~ LgGroup * Type * Duration + 
    (Type * LgGroup|Subject) + (1|Gap), data=switch.real.A,
    family=binomial, control = glmerControl(optimizer="bobyqa"))
#	Notes:
#		- Random slopes maximized but don't make qualitative difference

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

# Overall random rate comparison by age, transition type, and condition
switch.all.A <- filter(switch.all, Age == "A")
switch.all.C <- filter(switch.all, Age != "A") %>%
                 mutate(Age = as.numeric(Age))

runs <- unique(switch.all$Run)
nruns <- length(runs)

adusubs <- unique(switch.all.A$Subject)
aduvars <- c("Type", "Type + LgGroup", "Type + Duration")
aduAvgs <- data.table(
	Variable = c(rep(aduvars[1], nruns),
		rep(aduvars[2], nruns*2),
		rep(aduvars[3], nruns)),
	Comparison = c(rep(1, nruns),
		rep(1, nruns), rep(2, nruns),
		rep(1, nruns)),
	Run = rep(as.character(runs),4))
for (sub in adusubs) {
	aduAvgs[,c(as.character(sub)) := rep(0, nrow(aduAvgs))]	
}

chisubs <- unique(switch.all.C$Subject)
chivars <- c("LgGroup", "Duration", "Type + LgGroup") # and Age + LgGroup
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

	# Adult: Type
	varagg <- aggregate(Switch ~ Type + Subject, currrun.A, mean)
	subQ <- subset(varagg, Type == "Q")
	subS <- subset(varagg, Type == "S")
	deltas <- subQ$Switch - subS$Switch
	insert <- data.frame(t(deltas))
	colnames(insert) <- adusubs
	aduAvgs[Run == run & Variable == "Type", (adusubs) := insert]
	aduAvgs[Run == run & Variable == "Type", Variable := "TypeQS"]
	
	# Adult: Type + LgGroup
	varagg <- aggregate(Switch ~ LgGroup + Type + Subject, currrun.A, mean)
	subEQ <- subset(varagg, LgGroup == "E" & Type == "Q")
	subES <- subset(varagg, LgGroup == "E" & Type == "S")
	subNEQ <- subset(varagg, LgGroup == "NE" & Type == "Q")
	subNES <- subset(varagg, LgGroup == "NE" & Type == "S")
	deltasE <- subEQ$Switch - subES$Switch
	deltasNE <- subNEQ$Switch - subNES$Switch
	insertE <- data.frame(t(deltasE))
	colnames(insertE) <- adusubs
	aduAvgs[Run == run & Variable == "Type + LgGroup" & Comparison == 1,
		(adusubs) := insertE]
	aduAvgs[Run == run & Variable == "Type + LgGroup" & Comparison == 1,
		Variable := "LgGroup + Type E:QS"]
	insertNE <- data.frame(t(deltasNE))
	colnames(insertNE) <- adusubs
	aduAvgs[Run == run & Variable == "Type + LgGroup" & Comparison == 2,
		(adusubs) := insertNE]
	aduAvgs[Run == run & Variable == "Type + LgGroup" & Comparison == 2,
		Variable := "LgGroup + Type NE:QS"]

	# Adult: "Type + Duration"
	varagg <- aggregate(Switch ~ Type + Duration, currrun.A, mean)
	subQ <- subset(varagg, Type == "Q")
	subS <- subset(varagg, Type == "S")
	diffcor <- cor(subQ$Switch, subQ$Duration) -
		cor(subS$Switch, subS$Duration)
	insert <- data.frame(t(rep(diffcor, length(adusubs))))
	colnames(insert) <- adusubs
	aduAvgs[Run == run & Variable == "Type + Duration", (adusubs) := insert]
	aduAvgs[Run == run & Variable == "Type + Duration",
		Variable := "Type + Duration Qcor - Scor"]

	#### CHILDREN ####
	currrun.C <- filter(switch.all.C, Run == run)

	# Child: "Duration"
	varagg <- aggregate(Switch ~ Duration, currrun.C, mean)
	durcor <- cor(varagg$Switch, varagg$Duration)
	insert <- data.frame(t(rep(durcor, length(chisubs))))
	colnames(insert) <- chisubs
	chiAvgs[Run == run & Variable == "Duration", (chisubs) := insert]
	chiAvgs[Run == run & Variable == "Duration", Variable := "Duration-cor"]

	# Child: "LgGroup"
	varagg <- aggregate(Switch ~ LgGroup + Subject, currrun.C, mean)
	subE <- subset(varagg, LgGroup == "E")
	subNE <- subset(varagg, LgGroup == "NE")
	deltas <- subE$Switch - subNE$Switch
	insert <- data.frame(t(deltas))
	colnames(insert) <- chisubs
	chiAvgs[Run == run & Variable == "LgGroup", (chisubs) := insert]
	chiAvgs[Run == run & Variable == "LgGroup", Variable := "LgGroupENE"]

	# Child: Type + LgGroup
	varagg <- aggregate(Switch ~ LgGroup + Type + Subject, currrun.C, mean)
	subEQ <- subset(varagg, LgGroup == "E" & Type == "Q")
	subES <- subset(varagg, LgGroup == "E" & Type == "S")
	subNEQ <- subset(varagg, LgGroup == "NE" & Type == "Q")
	subNES <- subset(varagg, LgGroup == "NE" & Type == "S")
	deltasE <- subEQ$Switch - subES$Switch
	deltasNE <- subNEQ$Switch - subNES$Switch
	insertE <- data.frame(t(deltasE))
	colnames(insertE) <- chisubs
	chiAvgs[Run == run & Variable == "Type + LgGroup" & Comparison == 1,
		(chisubs) := insertE]
	chiAvgs[Run == run & Variable == "Type + LgGroup" & Comparison == 1,
		Variable := "LgGroup + Type E:QS"]
	insertNE <- data.frame(t(deltasNE))
	colnames(insertNE) <- chisubs
	chiAvgs[Run == run & Variable == "Type + LgGroup" & Comparison == 2,
		(chisubs) := insertNE]
	chiAvgs[Run == run & Variable == "Type + LgGroup" & Comparison == 2,
		Variable := "LgGroup + Type NE:QS"]
}

aduAvgs.bu <- aduAvgs
chiAvgs.bu <- chiAvgs

aduAvgs[, Mean := rowMeans(aduAvgs[, adusubs, with=F], na.rm=TRUE)]
chiAvgs[, Mean := rowMeans(chiAvgs[, chisubs, with=F], na.rm=TRUE)]

# Distribution plots and percentages

## Adults
rand.data.A <- aduAvgs[Run > 0]
real.data.A <- aduAvgs[Run == 0]

intlevel1 <- "LgGroup + Type E:QS"
intlevel2 <- "LgGroup + Type NE:QS"
Int1 <- getDiffs(intlevel1, intlevel2, rand.data.A, adusubs,
	"LgGroup + Type E:QS - NE:QS")
rand.data.A <- rand.data.A[Variable != intlevel1 & Variable != intlevel2]
rand.data.A <- rbind(rand.data.A, Int1)
Int1 <- getDiffs(intlevel1, intlevel2, real.data.A, adusubs,
	"LgGroup + Type E:QS - NE:QS")
real.data.A <- real.data.A[Variable != intlevel1 & Variable != intlevel2]
real.data.A <- rbind(real.data.A, Int1)

real.data.A[, GreaterThan := 0]

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
	geom_text(data=real.data.A, aes(x=-0.4,y=0.95,
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

#Type + LgGroup
intlevel1 <- "LgGroup + Type E:QS"
intlevel2 <- "LgGroup + Type NE:QS"
Int1 <- getDiffs(intlevel1, intlevel2, rand.data.C, chisubs,
	"LgGroup + Type E:QS - NE:QS")
rand.data.C <- rand.data.C[Variable != intlevel1 & Variable != intlevel2]
rand.data.C <- rbind(rand.data.C, Int1)
Int1 <- getDiffs(intlevel1, intlevel2, real.data.C, chisubs,
 	"LgGroup + Type E:QS - NE:QS")
real.data.C <- real.data.C[Variable != intlevel1 & Variable != intlevel2]
real.data.C <- rbind(real.data.C, Int1)

#LgGroup + Age
chisubinfo <- subject.info[AgeGroup == "CHI",c("Subject", "Age"), with=F]
chisubinfo <- subset(chisubinfo, Subject != "A-CHI-P11")

y3 <- chisubinfo[Age == 3, "Subject", with=F]$Subject
y4 <- chisubinfo[Age == 4, "Subject", with=F]$Subject
y5 <- chisubinfo[Age == 5, "Subject", with=F]$Subject

avg3 <- rowMeans(rand.data.C[Variable == "LgGroupENE", (y3), with=F])
avg4 <- rowMeans(rand.data.C[Variable == "LgGroupENE", (y4), with=F])
avg5 <- rowMeans(rand.data.C[Variable == "LgGroupENE", (y5), with=F])

newrows1 <- rand.data.C[Variable == "LgGroupENE"]
newrows1$Variable <- "LgGroupENE:54"
newrows1$Mean <- avg5 - avg4

newrows2 <- rand.data.C[Variable == "LgGroupENE"]
newrows2$Variable <- "LgGroupENE:53"
newrows2$Mean <- avg5 - avg3

newrows3 <- rand.data.C[Variable == "LgGroupENE"]
newrows3$Variable <- "LgGroupENE:43"
newrows3$Mean <- avg4 - avg3

rand.data.C <- rbind(rand.data.C, newrows1, newrows2, newrows3)

# real data
avg3 <- rowMeans(real.data.C[Variable == "LgGroupENE", (y3), with=F])
avg4 <- rowMeans(real.data.C[Variable == "LgGroupENE", (y4), with=F])
avg5 <- rowMeans(real.data.C[Variable == "LgGroupENE", (y5), with=F])

newrow1 <- real.data.C[Variable == "LgGroupENE"]
newrow1$Variable <- "LgGroupENE:54"
newrow1$Mean <- avg5 - avg4

newrow2 <- real.data.C[Variable == "LgGroupENE"]
newrow2$Variable <- "LgGroupENE:53"
newrow2$Mean <- avg5 - avg3

newrow3 <- real.data.C[Variable == "LgGroupENE"]
newrow3$Variable <- "LgGroupENE:43"
newrow3$Mean <- avg4 - avg3

real.data.C <- rbind(real.data.C, newrow1, newrow2, newrow3)

real.data.C[, GreaterThan := 0]
rand.data.C$Variable <- factor(rand.data.C$Variable,
	level=sort(unique(rand.data.C$Variable)))
real.data.C$Variable <- factor(real.data.C$Variable,
	level=sort(unique(real.data.C$Variable)))

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
	geom_text(data=real.data.C, aes(x=-0.1,y=0.95,
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
    width=2500,height=600,units="px", bg = "transparent")
print(cvars)
dev.off()

write.csv(rand.data.C, paste(processed.data.path,"summary.random.C.csv", sep=""))
write.csv(real.data.C, paste(processed.data.path,"summary.real.C.csv", sep=""))

real.data.C.ages <- real.data.C[Variable == "LgGroupENE:54" |
	Variable == "LgGroupENE:53" | Variable == "LgGroupENE:43"]
rand.data.C.ages <-  rand.data.C[Variable == "LgGroupENE:54" |
	Variable == "LgGroupENE:53" | Variable == "LgGroupENE:43"]
real.data.C.ages$Variable <- factor(
	real.data.C.ages$Variable, labels=c("Age 4 - Age 3",
	"Age 5 - Age 3", "Age 5 - Age 4"))
rand.data.C.ages$Variable <- factor(
	rand.data.C.ages$Variable, labels=c("Age 4 - Age 3",
	"Age 5 - Age 3", "Age 5 - Age 4"))

lgages <- ggplot(rand.data.C.ages, aes(x=Mean)) +
	facet_grid(.~Variable)+
	geom_density(aes(y=..scaled..), alpha=0.3) +
	ylab("Density") + xlab("Language effect: Mean difference score") +
	geom_point(data= real.data.C.ages, aes(x=Mean, y=0), size=6) +
	geom_text(data= real.data.C.ages, aes(x=-0.07,y=0.95,
		label=paste(round(GreaterThan*100,2),"%", sep="")),
		size=10) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),     
    strip.text.x = element_text(size=25, color="gray20"),
    legend.justification=c(1,0), legend.position=c(1,0),
    legend.title = element_text(colour="gray20", size=24),
    legend.text = element_text(colour="gray20", size=26),
    legend.key.width = unit(4, "lines"), legend.key.height = unit(2, "lines"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "child-randvsreal-ttest-agebylg.png", sep=""),
    width=1300,height=600,units="px", bg = "transparent")
print(lgages)
dev.off()
