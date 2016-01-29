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
puppetPalette <- c("gray20", "gray40", "gray80", "white")
puppetPaletteline <- c("black", "black", "black", "black")

# Default setting for all.data in case you don't need the table values and
# aren't actually loading all.data
all.data <- 0

# Takes forever to load, only uncomment if you want the table values for looks 
# to current speaker (at the top)
all.data <- read.csv(paste(processed.data.path, "all.data.before.analysis.csv",
    sep=""))

# Read in gaze switch proportions
# Actual data
switch.final <- read.csv(paste(processed.data.path, "switch.final.csv", sep=""))
switch.all <- switch.final

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

    #### BY CONDITION
    # Overall values
    sp.f.bl <- subset(sp.f, Condition == "blue")
    sp.f.me <- subset(sp.f, Condition == "mermaid")
    sp.f.pa <- subset(sp.f, Condition == "party")
    sp.f.re <- subset(sp.f, Condition == "red")
    sp.f.ro <- subset(sp.f, Condition == "robot")
    sp.f.ye <- subset(sp.f, Condition == "yellow")

    sp.m.bl <- subset(sp.m, Condition == "blue")
    sp.m.me <- subset(sp.m, Condition == "mermaid")
    sp.m.pa <- subset(sp.m, Condition == "party")
    sp.m.re <- subset(sp.m, Condition == "red")
    sp.m.ro <- subset(sp.m, Condition == "robot")
    sp.m.ye <- subset(sp.m, Condition == "yellow")

    # COND BLUE
    spk <- nrow(subset(sp.f.bl, F.looks == TRUE)) +
        nrow(subset(sp.m.bl, M.looks == TRUE))
    add <- nrow(subset(sp.f.bl, M.looks == TRUE)) +
        nrow(subset(sp.m.bl, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.bl, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.bl, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.bl, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.bl, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.bl <- c(spk/total, add/total, elsew/total, lost/total)

    # COND MERMAID
    spk <- nrow(subset(sp.f.me, F.looks == TRUE)) +
        nrow(subset(sp.m.me, M.looks == TRUE))
    add <- nrow(subset(sp.f.me, M.looks == TRUE)) +
        nrow(subset(sp.m.me, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.me, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.me, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.me, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.me, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.me <- c(spk/total, add/total, elsew/total, lost/total)
    
    # COND PARTY
    spk <- nrow(subset(sp.f.pa, F.looks == TRUE)) +
        nrow(subset(sp.m.pa, M.looks == TRUE))
    add <- nrow(subset(sp.f.pa, M.looks == TRUE)) +
        nrow(subset(sp.m.pa, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.pa, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.pa, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.pa, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.pa, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.pa <- c(spk/total, add/total, elsew/total, lost/total)

    # COND RED
    spk <- nrow(subset(sp.f.re, F.looks == TRUE)) +
        nrow(subset(sp.m.re, M.looks == TRUE))
    add <- nrow(subset(sp.f.re, M.looks == TRUE)) +
        nrow(subset(sp.m.re, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.re, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.re, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.re, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.re, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.re <- c(spk/total, add/total, elsew/total, lost/total)

    # COND ROBOT
    spk <- nrow(subset(sp.f.ro, F.looks == TRUE)) +
        nrow(subset(sp.m.ro, M.looks == TRUE))
    add <- nrow(subset(sp.f.ro, M.looks == TRUE)) +
        nrow(subset(sp.m.ro, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.ro, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.ro, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.ro, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.ro, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.ro <- c(spk/total, add/total, elsew/total, lost/total)

    # COND YELLOW
    spk <- nrow(subset(sp.f.ye, F.looks == TRUE)) +
        nrow(subset(sp.m.ye, M.looks == TRUE))
    add <- nrow(subset(sp.f.ye, M.looks == TRUE)) +
        nrow(subset(sp.m.ye, F.looks == TRUE))
    elsew <- nrow(subset(sp.f.ye, M.looks == FALSE & F.looks == FALSE &
        Events == TRUE)) + nrow(subset(sp.m.ye, M.looks == FALSE &
        F.looks == FALSE & Events == TRUE))
    lost <- nrow(subset(sp.f.ye, M.looks == FALSE & F.looks == FALSE &
        Events == FALSE)) + nrow(subset(sp.m.ye, M.looks == FALSE &
        F.looks == FALSE & Events == FALSE))
    total <- spk+add+elsew+lost
    looks.ye <- c(spk/total, add/total, elsew/total, lost/total)

    # Summary table
    looks.by.cond <- rbind(looks.bl, looks.me, looks.pa, looks.re, looks.ro, looks.ye, looks.all)
    colnames(looks.by.cond) <- c("speaker", "addressee", "other-screen",
        "offscreen")

    # Print the summary table
    print(looks.by.cond)
}
################################################################################
      



################################################################################
###### CORRECTED CONDITION MEANS ###############################################
switch.all$LgCond <- as.character(switch.all$Condition)
normalcond <- which(switch.all$LgCond == "red" |
	switch.all$LgCond == "blue" |
	switch.all$LgCond == "yellow")
switch.all$LgCond[normalcond] <- "normal"

means.agg <- aggregate(Switch ~ LgCond, switch.all, mean)
means <- means.agg$Switch
conds <- means.agg$LgCond
errs <- aggregate(Switch ~ LgCond, switch.all, sem)$Switch

df <- data.frame(
    LgCond = factor(conds),
    m = means,
    se = errs
)
df$LgCond <- factor(df$LgCond, labels=c("Mermaids\n(prosody only)", "Various colors\n(normal)",
    "Fancy dress\n(no speech)", "Robots\n(words only)"))
df$LgCond <- factor(df$LgCond, levels=c("Various colors\n(normal)", "Mermaids\n(prosody only)", 
    "Robots\n(words only)", "Fancy dress\n(no speech)"))

limits <- aes(ymax = m + se, ymin= m - se)

p2bars <- qplot(LgCond,m, ylim=c(0,1),
    ymin= m - se, ymax= m + se, fill= LgCond, color = "black",
    xlab="\nPuppet dyad", ylab="Proportion switches\n",
    geom="bar", stat="identity", position=position_dodge(), data=df) +
    geom_errorbar(limits, position=position_dodge(width=0.9),
    		width=0.25, colour="black") +
    scale_fill_manual(name="", values=puppetPalette) +
    scale_colour_manual(name="", values=puppetPaletteline) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
  	axis.text.x = element_text(size=26, color="gray40"),
	axis.text.y = element_text(size=26, color="gray40"),
	axis.title.x = element_text(size=26, color="gray20"),
	axis.title.y = element_text(size=26, color="gray20"),
    plot.background = element_rect(fill = "transparent",colour = NA),
	legend.position="none",
#    legend.justification=c(0.05,0.95), legend.position=c(0.75,0.95),
#    legend.text = element_text(colour="gray20", size=20),
#    legend.key = element_rect(fill = "transparent", colour = "transparent"),
#    legend.background = element_rect(fill="transparent"),
#    legend.key.height = unit(20, "mm"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "all-puppetdyads-bars.png", sep=""),
    width=900,height=700,units="px", bg = "transparent")
print(p2bars)
dev.off()
################################################################################




################################################################################
###### STATISTICS ##############################################################
switch.all$LgCond <- as.factor(switch.all$LgCond)
switch.all$LgCondNor <- factor(switch.all$LgCond, levels=c("normal", "mermaid", 
    "party", "robot"))
switch.all$LgCondRob <- factor(switch.all$LgCond, levels=c("robot", "normal",  
    "mermaid", "party"))
switch.all$LgCondPar <- factor(switch.all$LgCond, levels=c("party", "mermaid", 
    "normal", "robot"))

# Models
normal <- glmer(Switch ~ LgCondNor + (1|Subject) + (1|Gap), data=switch.all, family=binomial,
    control = glmerControl(optimizer="bobyqa"))
mermaid <- glmer(Switch ~ LgCond + (1|Subject) + (1|Gap), data=switch.all, family=binomial,
    control = glmerControl(optimizer="bobyqa"))
robot <- glmer(Switch ~ LgCondRob + (1|Subject) + (1|Gap), data=switch.all, family=binomial,
    control = glmerControl(optimizer="bobyqa"))
party <- glmer(Switch ~ LgCondPar + (1|Subject) + (1|Gap), data=switch.all, family=binomial,
    control = glmerControl(optimizer="bobyqa"))

summary(normal)
summary(mermaid)
summary(robot)
summary(party)

################################################################################