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
chi.coefs <- data.frame(t(sapply(fixef(chi.max),c)))
chi.coefs$Sample <- "real"
chi.coefs$Error <- ""
chi.coefs$Run <- 0

adu.coefs <- data.frame(t(sapply(fixef(adu.max),c)))
adu.coefs$Sample <- "real"
adu.coefs$Error <- ""
adu.coefs$Run <- 0

# Model results
write.csv(summary(chi.max)$coefficients, paste(
	processed.data.path, "chi.model.csv", sep=""),
	row.names=FALSE)
write.csv(summary(adu.max)$coefficients, paste(
	processed.data.path, "adu.model.csv", sep=""),
	row.names=FALSE)

# Model coefficients
write.csv(chi.coefs, paste(
	processed.data.path, "chi.coefs-0.csv", sep=""),
	row.names=FALSE)
write.csv(adu.coefs, paste(
	processed.data.path, "adu.coefs-0.csv", sep=""),
	row.names=FALSE)

# Overall random rate comparison by age, transition type, and condition
age3 <- filter(switch.all, Age == 3)
age3.eng <- filter(age3, LgGroup == "E")
age3.non <- filter(age3, LgGroup == "NE")
age3.eng.q <- filter(age3.eng, Type == "Q")
age3.non.q <- filter(age3.non, Type == "Q")
age3.eng.n <- filter(age3.eng, Type == "S")
age3.non.n <- filter(age3.non, Type == "S")

# a3 <- glmer(Switch ~ Sample + 
    # (1|Subject) + (1|Gap), data=age3,
    # family=binomial, control = glmerControl(optimizer="bobyqa"))
a3.e.q <- glmer(Switch ~ Sample + 
    (1|Subject) + (1|Gap), data=age3.eng.q,
    family=binomial, control = glmerControl(optimizer="bobyqa"))
a3.n.q <- glmer(Switch ~ Sample + 
    (1|Subject) + (1|Gap), data=age3.non.q,
    family=binomial, control = glmerControl(optimizer="bobyqa"))
a3.e.n <- glmer(Switch ~ Sample + 
    (1|Subject) + (1|Gap), data=age3.eng.n,
    family=binomial, control = glmerControl(optimizer="bobyqa"))
a3.n.n <- glmer(Switch ~ Sample + 
    (1|Subject) + (1|Gap), data=age3.non.n,
    family=binomial, control = glmerControl(optimizer="bobyqa"))
