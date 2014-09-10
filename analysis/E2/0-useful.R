library(grid)
library(ggplot2)
library(bootstrap)
library(lme4)
library(stringr)
library(plotrix)
library(reshape)
library(plyr)
library(car)

# Add some style elements for ggplot2
plot.style <- theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour="black",size=.5),
    axis.ticks = element_line(size=.5),
    axis.title.x = element_text(vjust=-.5),
    axis.title.y = element_text(angle=90,vjust=0.25),
    panel.margin = unit(1.5,"lines"))

theme_set(theme_bw())

# Standard error of the mean
sem <- function (x) {
    sd(x) / sqrt(length(x))
}

na.mean <- function(x) {mean(x,na.rm=T)}

to.n <- function(x) {
    as.numeric(as.character(x))
}

# Number of unique subs
n.unique <- function (x) {
    length(unique(x))
}

# For bootstrapping 95% confidence intervals
theta <- function(x,xdata) {mean(xdata[x])}
ci.low <- function(x) {
    mean(x) - quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.025)}
ci.high <- function(x) {
    quantile(bootstrap(1:length(x),1000,theta,x)$thetastar,.975) - mean(x)}
ci95 <- function(x) {sem(x)*1.96}

# For basic plots, add linear models with correlations
lm.txt <- function (p1,p2,x=7.5,yoff=.05,lt=2,c="black",data=data) {
    l <- lm(p2 ~ p1)
    regLine(l,lty=lt,col=c)
    cl <- coef(l)
    text(x,cl[1] + cl[2] * x + yoff,
        paste("r = ",sprintf("%2.2f",sqrt(summary(l)$r.squared)),
        getstars(anova(l)$"Pr(>F)"[1]),sep=""), xpd="n")
}

getstars <- function(x) {
    if (x > .1) {return("")}
    if (x < .001) {return("***")}
    if (x < .01) {return("**")}
    if (x < .05) {return("*")}
}