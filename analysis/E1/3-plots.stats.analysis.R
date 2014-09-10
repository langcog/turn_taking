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
stoplightPalette <- c("dodgerblue3", "firebrick2")
stoplightPalette2 <- c("gray26", "dodgerblue3", "firebrick2")
stoplightPalette3 <- c("dodgerblue3", "firebrick2")
lines <- c("solid", "dashed")
lines2 <- c("longdash", "dotted")


# Default setting for all.data in case you don't need the table values and
# aren't actually loading all.data
all.data <- 0

# Takes forever to load, only uncomment if you want the table values for looks 
# to current speaker (at the top)
#all.data <- read.csv(paste(processed.data.path, "all.data.before.analysis.csv",
#    sep=""))
#all.data <- subset(all.data, Subject != "D-P01-CHI")


# Read in gaze switch proportions
# Actual data
switch.final <- read.csv(paste(processed.data.path, "switch.final.csv", sep=""))
sw.f.drop <- c("X", "X.1", "notes", "Ttype")
switch.final <- switch.final[,!(names(switch.final) %in% sw.f.drop)]
# Random baseline
switch.final.r <- read.csv(paste(processed.data.path,
    "random.runs.anticipatory.csv", sep=""))
sw.f.r.drop <- c("X", "X.1", "X.2")
switch.final.r <- switch.final.r[,!(names(switch.final.r) %in% sw.f.r.drop)]
# Combine them
switch.final <- switch.final[, order(names(switch.final))]
switch.final.r <- switch.final.r[, order(names(switch.final.r))]
switch.all <- rbind(switch.final, switch.final.r)
# Exclude K-E bilingual child
switch.all <- subset(switch.all, Subject != "D-P01-CHI")

# Read in cumulative gaze switch data
cumulative.data <- read.csv(paste(processed.data.path,
    "cumulative.conditions.age.csv", sep=""))

# Supplementary data to be used at the end
vid.info <- read.csv(paste(info.path,"VideoSegmentInfo.csv",sep=""))
gap.info <- read.csv(paste(info.path,"GapInfo.csv",sep=""))

gap.info = within(gap.info, {
  Onset = round(Onset, 3)
  Offset = round(Offset, 3)
})


################################################################################
###### TABLE VALUES: LOOKS TO CURRENT SPEAKER ##################################
if (length(all.data) > 1) {
    # Times when the left or right speaker is speaking
    sp.l <- subset(all.data, True.Sp.L == "1")
    sp.r <- subset(all.data, True.Sp.R == "1")
    # Find looks...
    # To current speaker
    spk <- nrow(subset(sp.l, L.looks == TRUE)) +
        nrow(subset(sp.r, R.looks == TRUE))
    # To current addressee
    add <- nrow(subset(sp.l, R.looks == TRUE)) +
        nrow(subset(sp.r, L.looks == TRUE))
    # To elsewhere on screen
    elsew <- nrow(subset(sp.l, L.looks == FALSE & R.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.r, L.looks == FALSE &
        R.looks == FALSE & Events == TRUE))
    # Off screen
    lost <- nrow(subset(sp.l, L.looks == FALSE & R.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.r, L.looks == FALSE &
        R.looks == FALSE & Events == FALSE))
    # Totals
    total <- spk + add + elsew + lost
    looks.all <- c(spk/total, add/total, elsew/total, lost/total)

    #### BY AGE
    # Data during speech, by age
    sp.l.3 <- subset(sp.l, Age == "3")
    sp.l.4 <- subset(sp.l, Age == "4")
    sp.l.5 <- subset(sp.l, Age == "5")
    sp.l.A <- subset(sp.l, Age == "A")
    sp.r.3 <- subset(sp.r, Age == "3")
    sp.r.4 <- subset(sp.r, Age == "4")
    sp.r.5 <- subset(sp.r, Age == "5")
    sp.r.A <- subset(sp.r, Age == "A")

    # AGE 3
    spk <- nrow(subset(sp.l.3, L.looks == TRUE)) +
        nrow(subset(sp.r.3, R.looks == TRUE))
    add <- nrow(subset(sp.l.3, R.looks == TRUE)) +
        nrow(subset(sp.r.3, L.looks == TRUE))
    elsew <- nrow(subset(sp.l.3, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.r.3, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.l.3, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.r.3, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.3 <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE 4
    spk <- nrow(subset(sp.l.4, L.looks == TRUE)) +
        nrow(subset(sp.r.4, R.looks == TRUE))
    add <- nrow(subset(sp.l.4, R.looks == TRUE)) +
        nrow(subset(sp.r.4, L.looks == TRUE))
    elsew <- nrow(subset(sp.l.4, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.r.4, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.l.4, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.r.4, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.4 <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE 5
    spk <- nrow(subset(sp.l.5, L.looks == TRUE)) +
        nrow(subset(sp.r.5, R.looks == TRUE))
    add <- nrow(subset(sp.l.5, R.looks == TRUE)) +
        nrow(subset(sp.r.5, L.looks == TRUE))
    elsew <- nrow(subset(sp.l.5, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.r.5, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.l.5, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.r.5, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.5 <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE ADULT
    spk <- nrow(subset(sp.l.A, L.looks == TRUE)) +
        nrow(subset(sp.r.A, R.looks == TRUE))
    add <- nrow(subset(sp.l.A, R.looks == TRUE)) +
        nrow(subset(sp.r.A, L.looks == TRUE))
    elsew <- nrow(subset(sp.l.A, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.r.A, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.l.A, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.r.A, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.21 <- c(spk/total, add/total, elsew/total, lost/total)

    # Summary table
    looks.by.age <- rbind(looks.3, looks.4, looks.5, looks.21, looks.all)
    colnames(looks.by.age) <- c("speaker", "addressee", "other-screen",
        "offscreen")


    #### BY CONDITION
    # Data during speech, by language group condition
    sp.l.E <- subset(sp.l, VideoSegment == "AE1" | VideoSegment == "AE2")
    sp.l.NE <- subset(sp.l, VideoSegment == "German" |
        VideoSegment == "Japanese" | VideoSegment == "Hebrew" |
        VideoSegment == "Korean")
    sp.r.E <- subset(sp.r, VideoSegment == "AE1" | VideoSegment == "AE2")
    sp.r.NE <- subset(sp.r, VideoSegment == "German" |
        VideoSegment == "Japanese" | VideoSegment == "Hebrew" |
        VideoSegment == "Korean")

    # COND ENGLISH
    spk <- nrow(subset(sp.l.E, L.looks == TRUE)) +
        nrow(subset(sp.r.E, R.looks == TRUE))
    add <- nrow(subset(sp.l.E, R.looks == TRUE)) +
        nrow(subset(sp.r.E, L.looks == TRUE))
    elsew <- nrow(subset(sp.l.E, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.r.E, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.l.E, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.r.E, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.E <- c(spk/total, add/total, elsew/total, lost/total)

    # COND NON-ENGLISH
    spk <- nrow(subset(sp.l.NE, L.looks == TRUE)) +
        nrow(subset(sp.r.NE, R.looks == TRUE))
    add <- nrow(subset(sp.l.NE, R.looks == TRUE)) +
        nrow(subset(sp.r.E, L.looks == TRUE))
    elsew <- nrow(subset(sp.l.NE, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.r.NE, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.l.NE, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.r.NE, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.N <- c(spk/total, add/total, elsew/total, lost/total)

    # Summary table
    looks.by.cond <- rbind(looks.E, looks.N, looks.all)
    colnames(looks.by.cond) <- c("speaker", "addressee", "other-screen",
        "offscreen")


    #### BY CONDITION AND AGE
    E.sp.l.3 <- subset(sp.l.E, Age == "3")
    E.sp.l.4 <- subset(sp.l.E, Age == "4")
    E.sp.l.5 <- subset(sp.l.E, Age == "5")
    E.sp.l.A <- subset(sp.l.E, Age == "A")
    E.sp.r.3 <- subset(sp.r.E, Age == "3")
    E.sp.r.4 <- subset(sp.r.E, Age == "4")
    E.sp.r.5 <- subset(sp.r.E, Age == "5")
    E.sp.r.A <- subset(sp.r.E, Age == "A")
    NE.sp.l.3 <- subset(sp.l.NE, Age == "3")
    NE.sp.l.4 <- subset(sp.l.NE, Age == "4")
    NE.sp.l.5 <- subset(sp.l.NE, Age == "5")
    NE.sp.l.A <- subset(sp.l.NE, Age == "A")
    NE.sp.r.3 <- subset(sp.r.NE, Age == "3")
    NE.sp.r.4 <- subset(sp.r.NE, Age == "4")
    NE.sp.r.5 <- subset(sp.r.NE, Age == "5")
    NE.sp.r.A <- subset(sp.r.NE, Age == "A")

    # AGE 3
    spk <- nrow(subset(E.sp.l.3, L.looks == TRUE)) +
        nrow(subset(E.sp.r.3, R.looks == TRUE))
    add <- nrow(subset(E.sp.l.3, R.looks == TRUE)) +
        nrow(subset(E.sp.r.3, L.looks == TRUE))
    elsew <- nrow(subset(E.sp.l.3, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(E.sp.r.3, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(E.sp.l.3, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(E.sp.r.3, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.3E <- c(spk/total, add/total, elsew/total, lost/total)

    spk <- nrow(subset(NE.sp.l.3, L.looks == TRUE)) +
        nrow(subset(NE.sp.r.3, R.looks == TRUE))
    add <- nrow(subset(NE.sp.l.3, R.looks == TRUE)) +
        nrow(subset(NE.sp.r.3, L.looks == TRUE))
    elsew <- nrow(subset(NE.sp.l.3, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(NE.sp.r.3, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(NE.sp.l.3, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(NE.sp.r.3, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.3NE <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE 4
    spk <- nrow(subset(E.sp.l.4, L.looks == TRUE)) +
        nrow(subset(E.sp.r.4, R.looks == TRUE))
    add <- nrow(subset(E.sp.l.4, R.looks == TRUE)) +
        nrow(subset(E.sp.r.4, L.looks == TRUE))
    elsew <- nrow(subset(E.sp.l.4, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(E.sp.r.4, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(E.sp.l.4, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(E.sp.r.4, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.4E <- c(spk/total, add/total, elsew/total, lost/total)

    spk <- nrow(subset(NE.sp.l.4, L.looks == TRUE)) +
        nrow(subset(NE.sp.r.4, R.looks == TRUE))
    add <- nrow(subset(NE.sp.l.4, R.looks == TRUE)) +
        nrow(subset(NE.sp.r.4, L.looks == TRUE))
    elsew <- nrow(subset(NE.sp.l.4, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(NE.sp.r.4, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(NE.sp.l.4, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(NE.sp.r.4, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.4NE <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE 5
    spk <- nrow(subset(E.sp.l.5, L.looks == TRUE)) +
        nrow(subset(E.sp.r.5, R.looks == TRUE))
    add <- nrow(subset(E.sp.l.5, R.looks == TRUE)) +
        nrow(subset(E.sp.r.5, L.looks == TRUE))
    elsew <- nrow(subset(E.sp.l.5, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(E.sp.r.5, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(E.sp.l.5, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(E.sp.r.5, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.5E <- c(spk/total, add/total, elsew/total, lost/total)

    spk <- nrow(subset(NE.sp.l.5, L.looks == TRUE)) +
        nrow(subset(NE.sp.r.5, R.looks == TRUE))
    add <- nrow(subset(NE.sp.l.5, R.looks == TRUE)) +
        nrow(subset(NE.sp.r.5, L.looks == TRUE))
    elsew <- nrow(subset(NE.sp.l.5, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(NE.sp.r.5, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(NE.sp.l.5, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(NE.sp.r.5, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.5NE <- c(spk/total, add/total, elsew/total, lost/total)

    # AGE ADULT
    spk <- nrow(subset(E.sp.l.A, L.looks == TRUE)) +
        nrow(subset(E.sp.r.A, R.looks == TRUE))
    add <- nrow(subset(E.sp.l.A, R.looks == TRUE)) +
        nrow(subset(E.sp.r.A, L.looks == TRUE))
    elsew <- nrow(subset(E.sp.l.A, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(E.sp.r.A, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(E.sp.l.A, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(E.sp.r.A, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.AE <- c(spk/total, add/total, elsew/total, lost/total)

    spk <- nrow(subset(NE.sp.l.A, L.looks == TRUE)) +
        nrow(subset(NE.sp.r.A, R.looks == TRUE))
    add <- nrow(subset(NE.sp.l.A, R.looks == TRUE)) +
        nrow(subset(NE.sp.r.A, L.looks == TRUE))
    elsew <- nrow(subset(NE.sp.l.A, R.looks == FALSE & L.looks == FALSE &
        Events == TRUE)) + nrow(subset(NE.sp.r.A, R.looks == FALSE &
        L.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(NE.sp.l.A, R.looks == FALSE & L.looks == FALSE &
        Events == FALSE)) + nrow(subset(NE.sp.r.A, R.looks == FALSE &
        L.looks == FALSE & Events == FALSE))
    total <- spk + add + elsew + lost
    looks.ANE <- c(spk/total, add/total, elsew/total, lost/total)

    # Summary table
    looks.by.age.and.cond <- rbind(looks.3E, looks.3NE, looks.4E, looks.4NE,
        looks.5E, looks.5NE, looks.AE, looks.ANE, looks.all)
    colnames(looks.by.age.and.cond) <- c("speaker", "addressee", "other-screen",
        "offscreen")


    # Print the summary tables
    print(looks.by.age)
    print(looks.by.cond)
    print(looks.by.age.and.cond)
}
################################################################################




################################################################################
###### DIFF BETWEEN RANDOM AND TRANSITION SAMPLE SETS ##########################
errs <- aggregate(Switch ~ SampleType + Age, switch.all, sem)$Switch
means <- aggregate(Switch ~ SampleType + Age, switch.all, mean)$Switch

df <- data.frame(
    Age = factor(c("3", "3", "4", "4", "5", "5", "A", "A")),
    Sample = factor(c(
        "Transition", "Random",
        "Transition", "Random",
        "Transition", "Random",
        "Transition", "Random")),
    m = means,
    se = errs
)
df$Sample <- factor(df$Sample, levels=rev(levels(df$Sample)))

limits <- aes(ymax = m + se, ymin= m - se)

dodge <- position_dodge(width=0.9)

p0 <- ggplot(df, aes(Age,m)) +
    geom_freqpoly(stat="identity", color="black",
    aes(group=Sample, linetype=Sample)) +
    geom_errorbar(limits, position=dodge, width=0.25) +
    ylim(-0.1,0.5) +	ylab("Proportion gaze switches") + xlab("Age (year)")
png(paste(plot.path, "overall-sample-type.png",
    sep=""),width=900,height=700,units="px",bg = "transparent")
print(p0)
dev.off()
################################################################################




################################################################################
###### BY CONDITION & CONDENSED AGE SWITCHES ###################################
errs.agg <- aggregate(Switch ~ LgGroup + SampleType + Age, switch.all, sem)
errs <- errs.agg$Switch
means.agg <- aggregate(Switch ~ LgGroup + SampleType + Age, switch.all, mean)
means <- means.agg$Switch
ages <- as.character(means.agg$Age)
lgrps <- means.agg$LgGroup
samples <- means.agg$SampleType
linecolors <- c(
	"blue", "red",
	"black", "black",
	"blue", "red",
	"black", "black",
	"blue", "red",
	"black", "black",
	"blue", "red",
	"black", "black")

df <- data.frame(
    Age = factor(ages),
    Condition = factor(lgrps),
    Sample = factor(samples),
    LnColors = factor(linecolors),
    m = means,
    se = errs
)
df$Condition <- factor(df$Condition, labels=c("English", "Non-English"))
df$Sample <- factor(df$Sample, labels=c("Transition", "Random"))

limits <- aes(ymax = m + se, ymin= m - se)

p1 <- qplot(Age,m,facets = . ~ Condition, group=Sample, ylim=c(-0.1,0.5),
    ymin= m - se, ymax= m + se, color=LnColors, linetype=Sample,
    xlab="Age (years)",ylab="Proportion gaze switches",
    position=position_dodge(width=.1), data=df) +
    geom_line(size=2, position=position_dodge(width=.1)) +
    geom_pointrange(size=1, position=position_dodge(width=.1)) +
    scale_colour_manual(name="Condition", values=stoplightPalette2, guide=FALSE) +
    scale_linetype_manual(name="", values=lines,
    labels = c("Transition", "Random")) +
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
png(paste(plot.path, "samples-by-conditions.png",
    sep=""),width=1100,height=600,units="px",bg = "transparent")
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
ages <- means.agg.t$Age
conds <- means.agg.t$LgGroup

df <- data.frame(
    Age = factor(ages),
    Condition = factor(conds),
    m = means,
    se = errs
)
df$Condition <- factor(df$Condition, labels=c("English", "Non-English"))

limits <- aes(ymax = m + se, ymin= m - se)

p2 <- qplot(Age,m, group=Condition, ylim=c(-0.1,0.5),
    ymin= m - se, ymax= m + se, color=Condition,
    xlab="Age (years)", ylab="Proportion gaze switches",
    position=position_dodge(width=.1), data=df) +
    geom_line(size=2, position=position_dodge(width=.1)) +
    geom_pointrange(size=1, position=position_dodge(width=.1)) +
    scale_colour_manual(name = "Condition", values=stoplightPalette) +
    annotate("text", label = "English", x = 1.3, y = 0.24, size = 10, colour = "dodgerblue3") +
    annotate("text", label = "Non-English", x = 1.5, y = 0.04, size = 10, colour = "firebrick2") +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(size=30, color="gray40"),
    axis.text.y = element_text(size=30, color="gray40"),
    axis.title.x = element_text(size=30, color="gray20"),
    axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.position = "none", plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "all-conditions.png",
    sep=""),width=900,height=700,units="px",bg = "transparent")
print(p2)
dev.off()


p2bars <- qplot(Age,m, group=Condition, ylim=c(-0.1,0.5),
    ymin= m - se, ymax= m + se, fill=Condition,
    xlab="Age (years)", ylab="Proportion gaze switches",
    geom="bar", stat="identity", position=position_dodge(), data=df) +
    geom_errorbar(limits, position=position_dodge(width=0.9), width=0.25) +
    scale_fill_manual(name = "", values=stoplightPalette, labels = c("English  ", "Non-English")) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(size=30, color="gray40"),
    axis.text.y = element_text(size=30, color="gray40"),
    axis.title.x = element_text(size=30, color="gray20"),
    axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.justification=c(0.92,0.95), legend.position=c(0.92,0.95),
    legend.text = element_text(colour="gray20", size=30),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.direction = "horizontal",
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "all-conditions-bars.png", sep=""),
    width=900,height=700,units="px", bg = "transparent")
print(p2bars)
dev.off()
################################################################################




################################################################################
###### BY CONDITION & CONDENSED AGE & Q-EFFECTS SWITCHES #######################
errs.agg <- aggregate(Switch ~ Type + LgGroup + Age, switch.final, sem)
errs <- errs.agg$Switch
means.agg <- aggregate(Switch ~ Type + LgGroup + Age, switch.final, mean)
means <- means.agg$Switch
ages <- means.agg$Age
conds <- means.agg$LgGroup
ttypes <- means.agg$Type

df <- data.frame(
    Age = factor(ages),
    Condition = factor(conds),
    Transition = factor(ttypes),
    m = means,
    se = errs
)
df$Condition <- factor(df$Condition, labels=c("English", "Non-English"))
df$Transition <- factor(df$Transition, labels=c("Question", "Non-Question"))

limits <- aes(ymax = m + se, ymin= m - se)

p3 <- qplot(Age,m,facets = . ~ Condition, group=Transition, ylim=c(-0.1,0.5),
    ymin= m - se, ymax= m + se, color=Condition, linetype=Transition,
    xlab="Age (years)",ylab="Proportion gaze switches",
    position=position_dodge(width=.1), data=df) +
    geom_line(size=2, position=position_dodge(width=.1)) +
    geom_pointrange(size=1, position=position_dodge(width=.1)) +
    scale_colour_manual(name="Condition", values=stoplightPalette3, guide = FALSE) +
    scale_linetype_manual(name="", values=lines2) +
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
png(paste(plot.path, "transitions-by-conditions-uncorrected.png",
    sep=""),width=1100,height=600,units="px",bg = "transparent")
print(p3)
dev.off()
################################################################################




################################################################################
###### CORRECTED CONDITION MEANS WITH Q-EFFECTS ################################
# get random values to subtract from transition values
new.qs.means <- means - aggregate(Switch ~ Type + LgGroup + Age,
    switch.final.r, mean)$Switch

df <- data.frame(
  Age = factor(ages),
  Condition = factor(conds),
  Transition = factor(ttypes),
  m = new.qs.means,
  se = errs
)
df$Condition <- factor(df$Condition, labels=c("English", "Non-English"))
df$Transition <- factor(df$Transition, labels=c("Question", "Non-Question"))

limits <- aes(ymax = m + se, ymin= m - se)

p4 <- qplot(Age,m,facets = . ~ Condition, group=Transition, ylim=c(-0.1,0.5),
    ymin= m - se, ymax= m + se, color=Condition, linetype=Transition,
    xlab="Age (years)",ylab="Proportion gaze switches",
    position=position_dodge(width=.1), data=df) +
    geom_line(size=2, position=position_dodge(width=.1)) +
    geom_pointrange(size=1, position=position_dodge(width=.1)) +
    scale_colour_manual(name="Condition", values=stoplightPalette3, guide = FALSE) +
    scale_linetype_manual(name="", values=lines2) +
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
png(paste(plot.path, "transitions-by-conditions.png",
    sep=""),width=1100,height=600,units="px",bg = "transparent")
print(p4)
dev.off()
################################################################################




################################################################################
###### CUMULATIVE PROBABILITY PLOT AT TIME OF PLANNING #########################
# Grayscale friendly
colorPalette <- c("#C1B0CF", "#6DA2AD", "#395921", "#1C1C1C", "black")
# Values of median, min, mean utt2 onset values for the anticipation window
utt.vals <- read.csv(paste(info.path, "ResponseStartTimes.csv", sep=""))

# Relevel the condition data so it will display with the right labels
cumulative.data$Condition <- factor(cumulative.data$Condition,
    labels=c("English", "Non-English"))

# Correct the cumulative scores to reflect the time when the eye movements were 
# planned, i.e., subtract the assumed planning time
cumulative.data.A <- subset(cumulative.data, Age == "A")
cumulative.data.C <- subset(cumulative.data, Age != "A")
cumulative.data.A$Time2 <- cumulative.data.A$TimeIncrement - 0.3
cumulative.data.C$Time2 <- cumulative.data.C$TimeIncrement - 0.433
cumulative.data <- rbind(cumulative.data.A, cumulative.data.C)
cumulative.data <- subset(cumulative.data, Time2 >= -0.333)
cumulative.data$Time2MS <- cumulative.data$Time2 * 1000
cumulative.data$ones <- rep(1, nrow(cumulative.data))

ages_text <- data.frame(
    Time2MS = c(740,740,740,740,800,800,800,800),
    Proportion = c(0.388,0.349,0.322,0.260,0.194,0.168,0.219,0.245),
    lab = c("3", "4", "5", "A", "3", "4", "5", "A"),
    Condition = factor(c(
        "English", "English", "English", "English",
        "Non-English", "Non-English", "Non-English", "Non-English")),
    Age = factor(c(
        "3", "4", "5", "A", "3", "4", "5", "A")),
    ones = rep(1,8))

p5 <- qplot(Time2MS, Proportion, colour=Age,ylim=c(-0.1,0.5), ymin = Proportion,
    ymax = Proportion, geom="pointrange", size=ones,
    facets = . ~ Condition, xlab="Time (ms)",
    ylab="Proportion gaze switches", data=cumulative.data) +
    scale_colour_manual(name = "Age", values=colorPalette) +
    scale_size_continuous(name = "ones", range=c(0.01,3)) +
    scale_x_continuous(breaks=seq(-400, 800, 200), limits=c(-400, 800)) +
    # solid line at the offset of the prior turn, dashed line at the
    # median onset of the response
    geom_vline(aes(xintercept=MedianOrig*1000), lty=2, data=utt.vals) +
    geom_vline(aes(xintercept=0), lty=1) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.text.x = element_text(size=30, color="gray40"),
    axis.text.y = element_text(size=30, color="gray40"),
    axis.title.x = element_text(size=30, color="gray20"),
    axis.title.y = element_text(size=30, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),     
    strip.text.x = element_text(size=30, color="gray20"),
    legend.position = "none", plot.margin=unit(c(10,10,10,10),"mm")) +
    geom_text(data = ages_text,
        aes(x = Time2MS, y = Proportion, label = lab, facet = Condition),
        size = 8, color = "gray20")
png(paste(plot.path, "cumulative-looks.png",
    sep=""),width=1100,height=600,units="px",bg = "transparent")
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
durmeans <- aggregate(Switch ~ DurationRound + LgGroup, switch.final,
    mean)$Switch
durvals <- aggregate(Switch ~ DurationRound + LgGroup, switch.final, sem)
durdurs <- durvals$DurationRound
durerrs <- durvals$Switch
durconds <- durvals$LgGroup

df <- data.frame(
  Duration = durdurs,
  Condition = factor(durconds),
  m = durmeans,
  se = durerrs
)
df$Condition <- factor(df$Condition, labels=c("English", "Non-English"))

# Get correlation of gap duration and switches across conditions
dfeng <- subset(df, Condition == "English")
dfneng <- subset(df, Condition == "Non-English")
ecor <- rcorr(dfeng$Duration, dfeng$m)
ncor <- rcorr(dfneng$Duration, dfneng$m)

p6 <- qplot(Duration,m, facets = . ~ Condition, group=Condition, ylim=c(0,1),
    ymin= m - se, ymax= m + se,xlab="Gap duration (sec)",
    ylab="Proportion gaze switches",
    geom="point", data=df) + geom_smooth(method="lm")
png(paste(plot.path, "duration-effects.png",
    sep=""),width=1100,height=600,units="px",bg = "transparent")
print(p6)
dev.off()
################################################################################




################################################################################
###### STATISTICS ##############################################################
# Get corrected switching values by subtracting the random baseline from the 
# actual switch proportions by subject and each relevant predictor
rand.subject.means <- aggregate(Switch ~ Type + LgGroup + Age + Subject +
    Gap + Duration, switch.final.r, mean)
trans.subject.means <- aggregate(Switch ~ Type + LgGroup + Age + Subject +
    Gap + Duration, switch.final, mean)
merged.means <- merge(trans.subject.means, rand.subject.means,
    by=c("Subject", "LgGroup", "Type", "Gap"), all.x=TRUE)
merged.means$CorrSwitch <- merged.means$Switch.x - merged.means$Switch.y
merged.means <- subset(merged.means, select=-c(Switch.x, Age.y, Switch.y, Duration.y))
colnames(merged.means)[2] <- "Condition"
colnames(merged.means)[3] <- "Ttype"
colnames(merged.means)[5] <- "Age"
colnames(merged.means)[6] <- "Duration"
merged.means <- subset(merged.means, Subject != "D-P01-CHI")

# Change the age numbers to numeric values
merged.means$AgeNum <- rep(NA, nrow(merged.means))
merged.means$AgeNum[merged.means$Age == "3"] <- 3
merged.means$AgeNum[merged.means$Age == "4"] <- 4
merged.means$AgeNum[merged.means$Age == "5"] <- 5

merged.means.C <- subset(merged.means, AgeNum < 6)
merged.means.A <- subset(merged.means, Age == "A")

# Models
# Children
chi.max <- lmer(CorrSwitch ~ AgeNum * Condition * Ttype + Duration +
    (Condition * Ttype|Subject) + (1|Gap), data=merged.means.C)
summary(chi.max)

# Adults
adu.max <- lmer(CorrSwitch ~ Condition * Ttype + Duration +
    (Condition * Ttype|Subject), data=merged.means.A)
summary(adu.max)
################################################################################