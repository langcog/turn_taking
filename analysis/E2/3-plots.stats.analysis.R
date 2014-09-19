rm(list = ls())
library(ggplot2)
library(lme4)
library(gridExtra)
library(scales)  
library(Hmisc)
source("0-useful.R")

raw.data.path <- "tracker_data/"
info.path <- "info/"
processed.data.path <- "processed_data/"
plot.path <- "plots/"

# Color and line palettes
stoplightPalette <- c("dodgerblue3", "forestgreen", "green3", "firebrick2")
stoplightPalette2 <- c("gray26", "firebrick2", "forestgreen",
    "dodgerblue3", "green3")
stoplightPalette3 <- c("dodgerblue3", "forestgreen", "green3", "firebrick2")
lines <- c("solid", "dashed")
lines2 <- c("longdash", "dotted")


# Default setting for all.data in case you don't need the table values and
# aren't actually loading all.data
all.data <- 0

# Takes forever to load, only uncomment if you want the table values for looks 
# to current speaker (at the top)
#all.data <- read.csv(paste(processed.data.path, "all.data.before.analysis.csv",
#    sep=""))

# Read in gaze switch proportions
# Actual data
switch.final <- read.csv(paste(processed.data.path, "switch.final.csv", sep=""))
sw.f.drop <- c("X", "X.1", "notes")
switch.final <- switch.final[,!(names(switch.final) %in% sw.f.drop)]
# Random baseline
switch.final.r <- read.csv(paste(processed.data.path,
    "random.runs.anticipatory.csv", sep=""))
sw.f.r.drop <- c("X", "X.1", "X.2", "X.3")
switch.final.r <- switch.final.r[,!(names(switch.final.r) %in% sw.f.r.drop)]
# Combine them
switch.final <- switch.final[, order(names(switch.final))]
switch.final.r <- switch.final.r[, order(names(switch.final.r))]
switch.all.coarse <- rbind(switch.final, switch.final.r)

# Read in cumulative gaze switch data
cumulative.data <- read.csv(paste(processed.data.path,
    "cumulative.conditions.age.c.csv", sep=""))


################################################################################
###### TABLE VALUES: LOOKS TO CURRENT SPEAKER ##################################
if (length(all.data) > 1) {
    sp.f <- subset(all.data, True.Sp.F == "1")
    sp.m <- subset(all.data, True.Sp.M == "1")
    # Find looks...
    # To current speaker
    spk <- nrow(subset(sp.f, F.looks == TRUE)) +
        nrow(subset(sp.m, M.looks == TRUE))
    # To current addressee
    add <- nrow(subset(sp.f, M.looks == TRUE)) +
        nrow(subset(sp.m, F.looks == TRUE))
    # To elsewhere on screen
    elsew <- nrow(subset(sp.f, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    # Off screen
    lost <- nrow(subset(sp.f, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    # Totals
    total <- spk+add+elsew+lost
    looks.all <- c(spk/total, add/total, elsew/total, lost/total)

    #### BY AGE
    # Overall values
    sp.f.1 <- subset(sp.f, Age == 1)
    sp.f.2 <- subset(sp.f, Age == 2)
    sp.f.3 <- subset(sp.f, Age == 3)
    sp.f.4 <- subset(sp.f, Age == 4)
    sp.f.5 <- subset(sp.f, Age == 5)
    sp.f.6 <- subset(sp.f, Age == 6)
    sp.f.A <- subset(sp.f, Age == 21)
    sp.m.1 <- subset(sp.m, Age == 1)
    sp.m.2 <- subset(sp.m, Age == 2)
    sp.m.3 <- subset(sp.m, Age == 3)
    sp.m.4 <- subset(sp.m, Age == 4)
    sp.m.5 <- subset(sp.m, Age == 5)
    sp.m.6 <- subset(sp.m, Age == 6)
    sp.m.A <- subset(sp.m, Age == 21)

    # AGE 1
    spk <- nrow(subset(sp.f.1, F.looks == TRUE)) +
        nrow(subset(sp.m.1, M.looks == TRUE))
    add <- nrow(subset(sp.f.1, M.looks == TRUE)) +
        nrow(subset(sp.m.1, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.1, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.1, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.1, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.1, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.1 <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE 2
    spk <- nrow(subset(sp.f.2, F.looks == TRUE)) +
        nrow(subset(sp.m.2, M.looks == TRUE))
    add <- nrow(subset(sp.f.2, M.looks == TRUE)) +
        nrow(subset(sp.m.2, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.2, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.2, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.2, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.2, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.2 <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE 3
    spk <- nrow(subset(sp.f.3, F.looks == TRUE)) +
        nrow(subset(sp.m.3, M.looks == TRUE))
    add <- nrow(subset(sp.f.3, M.looks == TRUE)) +
        nrow(subset(sp.m.3, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.3, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.3, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.3, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.3, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.3 <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE 4
    spk <- nrow(subset(sp.f.4, F.looks == TRUE)) +
        nrow(subset(sp.m.4, M.looks == TRUE))
    add <- nrow(subset(sp.f.4, M.looks == TRUE)) +
        nrow(subset(sp.m.4, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.4, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.4, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.4, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.4, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.4 <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE 5
    spk <- nrow(subset(sp.f.5, F.looks == TRUE)) +
        nrow(subset(sp.m.5, M.looks == TRUE))
    add <- nrow(subset(sp.f.5, M.looks == TRUE)) +
        nrow(subset(sp.m.5, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.5, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.5, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.5, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.5, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.5 <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE 6
    spk <- nrow(subset(sp.f.6, F.looks == TRUE)) +
        nrow(subset(sp.m.6, M.looks == TRUE))
    add <- nrow(subset(sp.f.6, M.looks == TRUE)) +
        nrow(subset(sp.m.6, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.6, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.6, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.6, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.6, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.6 <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE ADULT
    spk <- nrow(subset(sp.f.A, F.looks == TRUE)) +
        nrow(subset(sp.m.A, M.looks == TRUE))
    add <- nrow(subset(sp.f.A, M.looks == TRUE)) +
        nrow(subset(sp.m.A, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.A, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.A, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.A, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.A, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.21 <- c(spk/total, add/total, elsew/total, lost/total)

    # Summary table
    looks.by.age <- rbind(looks.1, looks.2, looks.3, looks.4, looks.5,
        looks.6, looks.21, looks.all)
    colnames(looks.by.age) <- c("speaker", "addressee", "other-screen", "offscreen")

    #### BY CONDITION
    # Overall values
    sp.f.N <- subset(sp.f, Condition == "normal")
    sp.f.M <- subset(sp.f, Condition == "muffled")
    sp.f.R <- subset(sp.f, Condition == "robot")
    sp.f.B <- subset(sp.f, Condition == "babble")

    sp.m.N <- subset(sp.m, Condition == "normal")
    sp.m.M <- subset(sp.m, Condition == "muffled")
    sp.m.R <- subset(sp.m, Condition == "robot")
    sp.m.B <- subset(sp.m, Condition == "babble")

    # COND NORMAL
    spk <- nrow(subset(sp.f.N, F.looks == TRUE)) +
        nrow(subset(sp.m.N, M.looks == TRUE))
    add <- nrow(subset(sp.f.N, M.looks == TRUE)) +
        nrow(subset(sp.m.N, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.N, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.N, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.N, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.N, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.N <- c(spk/total, add/total, elsew/total, lost/total)


    # COND MUFFLED
    spk <- nrow(subset(sp.f.M, F.looks == TRUE)) +
        nrow(subset(sp.m.M, M.looks == TRUE))
    add <- nrow(subset(sp.f.M, M.looks == TRUE)) +
        nrow(subset(sp.m.M, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.M, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.M, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.M, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.M, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.M <- c(spk/total, add/total, elsew/total, lost/total)

    # COND ROBOT
    spk <- nrow(subset(sp.f.R, F.looks == TRUE)) +
        nrow(subset(sp.m.R, M.looks == TRUE))
    add <- nrow(subset(sp.f.R, M.looks == TRUE)) +
        nrow(subset(sp.m.R, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.R, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.R, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.R, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.R, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.R <- c(spk/total, add/total, elsew/total, lost/total)

    # COND BABBLE
    spk <- nrow(subset(sp.f.B, F.looks == TRUE)) +
        nrow(subset(sp.m.B, M.looks == TRUE))
    add <- nrow(subset(sp.f.B, M.looks == TRUE)) +
        nrow(subset(sp.m.B, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.B, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.B, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.B, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.B, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.B <- c(spk/total, add/total, elsew/total, lost/total)

    # Summary table
    looks.by.cond <- rbind(looks.N, looks.M, looks.R, looks.B, looks.all)
    colnames(looks.by.cond) <- c("speaker", "addressee", "other-screen",
        "offscreen")


    #### ONE YEAR OLDS
    # Overall values
    sp.f.N.1 <- subset(sp.f.N, Age == 1)
    sp.f.M.1 <- subset(sp.f.M, Age == 1)
    sp.f.R.1 <- subset(sp.f.R, Age == 1)
    sp.f.B.1 <- subset(sp.f.B, Age == 1)

    sp.m.N.1 <- subset(sp.m.N, Age == 1)
    sp.m.M.1 <- subset(sp.m.M, Age == 1)
    sp.m.R.1 <- subset(sp.m.R, Age == 1)
    sp.m.B.1 <- subset(sp.m.B, Age == 1)
    
    # COND NORMAL - ONE YEAR OLDS
    spk <- nrow(subset(sp.f.N.1, F.looks == TRUE)) +
        nrow(subset(sp.m.N.1, M.looks == TRUE))
    add <- nrow(subset(sp.f.N.1, M.looks == TRUE)) +
        nrow(subset(sp.m.N.1, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.N.1, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.N.1, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.N.1, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.N.1, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.N.1 <- c(spk/total, add/total, elsew/total, lost/total)


    # COND MUFFLED - ONE YEAR OLDS
    spk <- nrow(subset(sp.f.M.1, F.looks == TRUE)) +
        nrow(subset(sp.m.M.1, M.looks == TRUE))
    add <- nrow(subset(sp.f.M.1, M.looks == TRUE)) +
        nrow(subset(sp.m.M.1, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.M.1, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.M.1, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.M.1, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.M.1, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.M.1 <- c(spk/total, add/total, elsew/total, lost/total)

    # COND ROBOT - ONE YEAR OLDS
    spk <- nrow(subset(sp.f.R.1, F.looks == TRUE)) +
        nrow(subset(sp.m.R.1, M.looks == TRUE))
    add <- nrow(subset(sp.f.R.1, M.looks == TRUE)) +
        nrow(subset(sp.m.R.1, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.R.1, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.R.1, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.R.1, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.R.1, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.R.1 <- c(spk/total, add/total, elsew/total, lost/total)

    # COND BABBLE - ONE YEAR OLDS
    spk <- nrow(subset(sp.f.B.1, F.looks == TRUE)) +
        nrow(subset(sp.m.B.1, M.looks == TRUE))
    add <- nrow(subset(sp.f.B.1, M.looks == TRUE)) +
        nrow(subset(sp.m.B.1, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.B.1, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.B.1, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.B.1, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.B.1, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.B.1 <- c(spk/total, add/total, elsew/total, lost/total)

    # Summary table
    looks.by.cond.1 <- rbind(looks.N.1, looks.M.1, looks.R.1, looks.B.1)
    colnames(looks.by.cond.1) <- c("speaker", "addressee", "other-screen",
        "offscreen")


    # Print the summary table
    print(looks.by.age)
    print(looks.by.cond)
    print(looks.by.cond.1)
}
################################################################################




################################################################################
###### DIFF BETWEEN RANDOM AND TRANSITION SAMPLE SETS ##########################
errs.agg <- aggregate(Switch ~ Condition + SampleType + Age,
    switch.all.coarse, sem)
errs <- errs.agg$Switch
means.agg <- aggregate(Switch ~ Condition + SampleType + Age,
    switch.all.coarse, mean)
means <- means.agg$Switch
ages <- as.character(means.agg$Age)
ages[ages == "21"] <- "A"
conds <- means.agg$Condition
samples <- means.agg$SampleType
linecolors <- c(
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
	"blue", "green1", "green2", "red",
	"black", "black", "black", "black")

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
df$Age <- factor(df$Age, labels=c("1","2","3","4","5","6","Adult"))
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
      



################################################################################
###### CORRECTED CONDITION MEANS ###############################################
# get random values to subtract from transition values
means.agg.r <- subset(means.agg, SampleType == "RANDOM")
means.agg.t <- subset(means.agg, SampleType == "TRANSITION")
errs <- subset(errs.agg, SampleType == "TRANSITION")$Switch
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
df$Age <- factor(df$Age, labels=c("1","2","3","4","5","6","Adult"))
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
################################################################################




################################################################################
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
df$Age <- factor(df$Age, labels=c("1","2","3","4","5","6","Adult"))
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




################################################################################
###### CORRECTED CONDITION MEANS WITH Q-EFFECTS ################################
# get random values to subtract from transition values
new.qs.means <- means - aggregate(Switch ~ Ttype + Condition + Age,
    switch.final.r, mean)$Switch

df <- data.frame(
    Age = factor(ages),
    Condition = factor(conds),
    Transition = factor(ttypes),
    m = new.qs.means,
    se = errs
)
df$Condition <- factor(df$Condition, labels=c("No speech", "Prosody only",
    "Normal speech", "Words only"))
df$Transition <- factor(df$Transition, labels=c("Question", "Non-Question"))
df$Age <- factor(df$Age, labels=c("1","2","3","4","5","6","Adult"))
df$Condition <- factor(df$Condition, levels=c("Normal speech", "Prosody only",
    "Words only", "No speech"))

limits <- aes(ymax = m + se, ymin= m - se)

p4 <- qplot(Age,m,facets = . ~ Condition, group=Transition,
    ylim=c(-0.1,0.6), ymin= m - se, ymax= m + se, color=Condition,
    linetype=Transition, xlab="Age (years)",ylab="Baseline-corrected proportion switches",
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
    legend.justification=c(0.98,0.95), legend.position=c(0.98,0.95),
    legend.title = element_text(colour="gray20", size=24),
    legend.text = element_text(colour="gray20", size=26),
    legend.key.width = unit(4, "lines"), legend.key.height = unit(2, "lines"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "transitions-by-conditions.png", sep=""),
    width=2000,height=600,units="px", bg = "transparent")
print(p4)
dev.off()
################################################################################




################################################################################
###### CUMULATIVE PROBABILITY PLOT AT TIME OF PLANNING #########################
# Grayscale friendly
colorPalette <- c("#C1B0CF", "#6DA2AD", "#395921", "#1C1C1C")
# Values of median, min, mean utt2 onset values for the anticipation window
utt.vals <- read.csv(paste(info.path, "ResponseStartTimes.csv", sep=""))

# Relevel the condition data so it will display with the right labels
cumulative.data$Condition <- factor(cumulative.data$Condition, labels=c(
    "No speech", "Prosody only", "Normal speech", "Words only"))
cumulative.data$Condition <- factor(cumulative.data$Condition, levels=c(
    "Normal speech", "Prosody only", "Words only", "No speech"))

# Correct the cumulative scores to reflect the time when the eye movements were 
# planned, i.e., subtract the assumed planning time
cumulative.data.A <- subset(cumulative.data, Age == "Adult")
cumulative.data.C <- subset(cumulative.data, Age != "Adult")
cumulative.data.A$Time2 <- cumulative.data.A$TimeIncrement - 0.3
cumulative.data.C$Time2 <- cumulative.data.C$TimeIncrement - 0.433
cumulative.data <- rbind(cumulative.data.A, cumulative.data.C)
cumulative.data <- subset(cumulative.data, Time2 >= -0.333)
cumulative.data$Time2MS <- cumulative.data$Time2 * 1000
cumulative.data$ones <- rep(1, nrow(cumulative.data))

ages_text <- data.frame(
    Time2MS = c(950,950,950,1000,880,880,880,950,
                910,910,910,980,880,880,880,950),
    Proportion = c(0.274,0.246,0.382,0.339,
                   0.133,0.256,0.320,0.290,
                   0.205,0.230,0.302,0.367,
                   0.160,0.130,0.288,0.197),
    lab = c("1 & 2", "3 & 4", "5 & 6", "A",
            "1 & 2", "3 & 4", "5 & 6", "A",
            "1 & 2", "3 & 4", "5 & 6", "A",
            "1 & 2", "3 & 4", "5 & 6", "A"),
    Condition = factor(c(
        "Normal speech", "Normal speech", "Normal speech", "Normal speech",
        "Words only", "Words only", "Words only", "Words only",
        "Prosody only", "Prosody only", "Prosody only", "Prosody only",
        "No speech", "No speech", "No speech", "No speech")),
    Age = factor(c(
            "1 & 2", "3 & 4", "5 & 6", "Adult",
            "1 & 2", "3 & 4", "5 & 6", "Adult",
            "1 & 2", "3 & 4", "5 & 6", "Adult",
            "1 & 2", "3 & 4", "5 & 6", "Adult")),
    ones = rep(1,16))

p5 <- qplot(Time2MS, Proportion, colour=Age,ylim=c(-0.1,0.6), ymin = Proportion,
    ymax = Proportion, geom="pointrange", size=ones,
    facets = . ~ Condition, xlab="Time (ms)",
    ylab="Proportion gaze switches", data=cumulative.data) +
  	scale_colour_manual(name = "Age", values=colorPalette) +
    scale_size_continuous(name = "ones", range=c(0.01,3)) +
    scale_x_continuous(breaks=seq(-400, 1050, 200), limits=c(-400, 1050)) +
    # solid line at the offset of the prior turn, dashed line at the
    # median onset of the response
    geom_vline(aes(xintercept=MedianOrig*1000), lty=2, data=utt.vals) +
  	geom_vline(aes(xintercept=0), lty=1) +
  	plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(size=28, color="gray40"),
    axis.text.y = element_text(size=30, color="gray40"),
    axis.title.x = element_text(size=30, color="gray20"),
    axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),     
    strip.text.x = element_text(size=30, color="gray20"),
    legend.position = "none", plot.margin=unit(c(10,10,10,10),"mm")) +
    geom_text(data = ages_text,
        aes(x = Time2MS, y = Proportion, label = lab, facet = Condition),
        size = 8, color = "gray20")
png(paste(plot.path, "cumulative-looks.png", sep=""),
    width=2000,height=600,units="px", bg = "transparent")
print(p5)
dev.off()
################################################################################




################################################################################
###### TABLE OF LOOKS PLANNED BEFORE OFFSET OF T1 ##############################
# "Zero" times are 0.00 for adults and -0.008 for children
end.of.t1 <- subset(cumulative.data, Time2 < 0 & Time2 > -0.01)[,3:5]
end.of.t1
################################################################################




################################################################################
###### EFFECT OF GAP DURATION ##################################################
# Duration by switch across conditions
switch.final$DurationRound <- round(switch.final$Duration, 2)
durmeans <- aggregate(Switch ~ DurationRound + Condition, switch.final,
    mean)$Switch
durvals <- aggregate(Switch ~ DurationRound + Condition, switch.final, sem)
durdurs <- durvals$DurationRound
durerrs <- durvals$Switch
durconds <- durvals$Condition

df <- data.frame(
    Duration = durdurs,
    Condition = factor(durconds),
    m = durmeans,
    se = durerrs
)
df$Condition <- factor(df$Condition, levels=c("normal", "muffled", "robot",
    "babble"))

# Get correlation of gap duration and switches across conditions
dfnorm <- subset(df, Condition == "normal")
dfmuff <- subset(df, Condition == "muffled")
dfrobo <- subset(df, Condition == "robot")
dfbabb <- subset(df, Condition == "babble")
ncor <- rcorr(dfnorm$Duration, dfnorm$m)
mcor <- rcorr(dfmuff$Duration, dfmuff$m)
rcor <- rcorr(dfrobo$Duration, dfrobo$m)
bcor <- rcorr(dfbabb$Duration, dfbabb$m)


p6 <- qplot(Duration,m, facets = . ~ Condition, group=Condition,
    ylim=c(0,1), ymin= m - se, ymax= m + se,
    xlab="Gap duration (sec)", ylab="Proportion gaze switches",
    geom="point", data=df) + geom_smooth(method="lm")
png(paste(plot.path, "duration-effects.png",
    sep=""),width=1100,height=600,units="px",bg = "transparent")
print(p6)
dev.off()
################################################################################




################################################################################
###### QUESTION EFFECT EMERGENCE ###############################################
sfr.lex <- subset(switch.final.r, Condition == "normal" | Condition == "robot")
sf.lex <- subset(switch.final, Condition == "normal" | Condition == "robot")

rand.subject.means.lex <- aggregate(Switch ~ Ttype + Condition + Age + Subject,
    sfr.lex, mean)
trans.subject.means.lex <- aggregate(Switch ~ Ttype + Condition + Age + Subject,
    sf.lex, mean)
merged.means.lex <- merge(trans.subject.means.lex, rand.subject.means.lex,
   by=c("Subject", "Condition", "Ttype"), all.x=TRUE)
merged.means.lex$CorrSwitch <- merged.means.lex$Switch.x - merged.means.lex$Switch.y
merged.means.lex <- subset(merged.means.lex, select=-c(Switch.x, Age.y, Switch.y))
colnames(merged.means.lex)[4] <- "Age"
merged.means.lex.1 <- subset(merged.means.lex, Age == 1)
merged.means.lex.2 <- subset(merged.means.lex, Age == 2)
merged.means.lex.3 <- subset(merged.means.lex, Age == 3)
merged.means.lex.4 <- subset(merged.means.lex, Age == 4)
merged.means.lex.5 <- subset(merged.means.lex, Age == 5)
merged.means.lex.6 <- subset(merged.means.lex, Age == 6)
merged.means.lex.A <- subset(merged.means.lex, Age == 21)
################################################################################




################################################################################
###### STATISTICS ##############################################################
# Get corrected switching values by subtracting the random baseline from the 
# actual switch proportions by subject and each relevant predictor
rand.subject.means <- aggregate(Switch ~ Ttype + Condition + Age + Subject +
    Gap + Duration, switch.final.r, mean)
trans.subject.means <- aggregate(Switch ~ Ttype + Condition + Age + Subject +
    Gap + Duration, switch.final, mean)
merged.means <- merge(trans.subject.means, rand.subject.means,
   by=c("Subject", "Condition", "Ttype", "Gap"), all.x=TRUE)
merged.means$CorrSwitch <- merged.means$Switch.x - merged.means$Switch.y
merged.means <- subset(merged.means, select=-c(Switch.x, Age.y, Switch.y, Duration.y))
colnames(merged.means)[5] <- "Age"
colnames(merged.means)[6] <- "Duration"
merged.means$Condition <- factor(merged.means$Condition, levels=c("normal",
    "muffled", "robot", "babble"))

merged.means.C <- subset(merged.means, Age < 7)
merged.means.A <- subset(merged.means, Age == 21)

# Models
# Children
chi.max <- lmer(CorrSwitch ~ Age * Condition * Ttype + Duration +
    (Condition + Ttype|Subject) + (1|Gap), data=merged.means.C)
summary(chi.max)


# Adults
adu.max <- lmer(CorrSwitch ~ Condition * Ttype + Duration +
    (Condition + Ttype|Subject) + (1|Gap), data=merged.means.A)
summary(adu.max)

## Post hoc (correction 0.05/2 = 0.025)
u3.r <- subset(switch.final.r, Age < 3)
u3.a <- subset(switch.final, Age < 3)
u3.lex.r <- subset(u3.r, Condition == "robot")
u3.lex.a <- subset(u3.a, Condition == "robot")
u3.pro.r <- subset(u3.r, Condition == "muffled")
u3.pro.a <- subset(u3.a, Condition == "muffled")

u3.lex.r.agg <- aggregate(Switch ~ Subject + Gap, u3.lex.r, mean)
u3.lex.a.agg <- aggregate(Switch ~ Subject + Gap, u3.lex.a, mean)
u3.pro.r.agg <- aggregate(Switch ~ Subject + Gap, u3.pro.r, mean)
u3.pro.a.agg <- aggregate(Switch ~ Subject + Gap, u3.pro.a, mean)

t.test(u3.lex.r.agg$Switch, u3.lex.a.agg$Switch)
t.test(u3.pro.r.agg$Switch, u3.pro.a.agg$Switch)

################################################################################