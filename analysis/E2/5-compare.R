rm(list=ls())
source("0-helper.R")

################################################################################
# Read in data in processed_data/ path and set plotting variables
################################################################################
# Set paths
processed.data.path <- "processed_data/" #"../../data/E1/processed_data/"
model.path <- "processed_data/models/" #"../../data/E1/processed_data/"
plot.path <- "plots/" #"plots/"

# Read in all the random runs and combine them
adu.r.i.files <- dir(model.path, pattern="adu.max.r.i-[0-9]+.csv$")
adu.r.m.files <- dir(model.path, pattern="adu.max.r.m-[0-9]+.csv$")
chi.r.i.files <- dir(model.path, pattern="chi.max.r.i-[0-9]+.csv$")
chi.r.m.files <- dir(model.path, pattern="chi.max.r.m-[0-9]+.csv$")

adu.model.i <- read.csv(paste(model.path, "adu.max.i-0.csv", sep=""))
adu.model.m <- read.csv(paste(model.path, "adu.max.m-0.csv", sep=""))
chi.model.i <- read.csv(paste(model.path, "chi.max.i-0.csv", sep=""))
chi.model.m <- read.csv(paste(model.path, "chi.max.m-0.csv", sep=""))

### Initialize empty data tables
# Model output tables
aduvars <- as.character(adu.model.m$Predictor)
chivars <- as.character(chi.model.m$Predictor)
aruns <- c(0:length(adu.r.m.files))
cruns <- c(0:length(chi.r.m.files))
aduB <- data.table(Run = aruns)
for (var in aduvars) {
	aduB[,c(as.character(var)) := rep(0, nrow(aduB))]	
}
aduSE <- data.table(Run = aruns)
for (var in aduvars) {
	aduSE[,c(as.character(var)) := rep(0, nrow(aduSE))]	
}
aduT <- data.table(Run = aruns)
for (var in aduvars) {
	aduT[,c(as.character(var)) := rep(0, nrow(aduT))]	
}
chiB <- data.table(Run = cruns)
for (var in chivars) {
	chiB[,c(as.character(var)) := rep(0, nrow(chiB))]	
}
chiSE <- data.table(Run = cruns)
for (var in chivars) {
	chiSE[,c(as.character(var)) := rep(0, nrow(chiSE))]	
}
chiT <- data.table(Run = cruns)
for (var in chivars) {
	chiT[,c(as.character(var)) := rep(0, nrow(chiT))]	
}
# Model fit table
aduFit <- data.table(Run = aruns,
	LogLik = rep(0, length(aruns)),
	AIC = rep(0, length(aruns)),
	Error = rep("", length(aruns)))
chiFit <- data.table(Run = cruns,
	LogLik = rep(0, length(cruns)),
	AIC = rep(0, length(cruns)),
	Error = rep("", length(cruns)))

aduruns <- combine.runs(aduB, aduSE, aduT, aduFit,
	adu.r.m.files, adu.r.i.files, "adu.max.m-0.csv", "adu.max.i-0.csv")
aduruns[[4]]$ErrorBin <- ifelse(aduruns[[4]]$Error == "", 0, 1)
chiruns <- combine.runs(chiB, chiSE, chiT, chiFit,
	chi.r.m.files, chi.r.i.files, "chi.max.m-0.csv", "chi.max.i-0.csv")
chiruns[[4]]$ErrorBin <- ifelse(chiruns[[4]]$Error == "", 0, 1)

adunoerrors <- data.table(Run = aduruns[[4]][aduruns[[4]]$ErrorBin == 0,]$Run)
chinoerrors <- data.table(Run = chiruns[[4]][chiruns[[4]]$ErrorBin == 0,]$Run)

aduruns <- lapply(aduruns, merge, adunoerrors, by="Run")
chiruns <- lapply(chiruns, merge, chinoerrors, by="Run")

########## Plots ##########
### Adults ###
aduBetas <- melt(aduruns[[1]], names(aduruns[[1]])[1])
aduSEs <- melt(aduruns[[2]], names(aduruns[[2]])[1])
aduTs <- melt(aduruns[[3]], names(aduruns[[3]])[1])
aduFits <- melt(aduruns[[4]], names(aduruns[[4]])[1])
aduLL <- aduFits[variable == "LogLik"]
aduAIC <- aduFits[variable == "AIC"]

chiBetas <- melt(chiruns[[1]], names(chiruns[[1]])[1])
chiSEs <- melt(chiruns[[2]], names(chiruns[[2]])[1])
chiTs <- melt(chiruns[[3]], names(chiruns[[3]])[1])
chiFits <- melt(chiruns[[4]], names(chiruns[[4]])[1])
chiLL <- chiFits[variable == "LogLik"]
chiAIC <- chiFits[variable == "AIC"]

# Run, then run loop below

# Group 1: Adult B & t (normal vals)
valtypes <- c("betas", "t-vals")
testtypes <- c("normal", "normal")
groups <- c("adu", "adu")
graphdists <- list(B=aduBetas,T=aduTs)
for (i in 1:length(graphdists)) {
	valtype <- valtypes[i]
	tograph <- graphdists[[i]]
	test <- testtypes[i]
	group <- groups[i]
	melt.rand <- subset(tograph, Run > 0)
	melt.rand95 <- getSig(melt.rand, test, valtype)
	if (test == "absolute") {
		if (valtype == "SEs") {
			ymax <- sd(melt.rand95$value)*10						
		} else {
			ymax <- max(melt.rand95$value)*2			
		}
		ymin <- 0
	} else {
		lmax <- max(abs(subset(melt.rand95,
			variable != "(Intercept)")$value.l), na.rm=T)
		umax <- max(abs(subset(melt.rand95,
			variable != "(Intercept)")$value.u), na.rm=T)
		ymax <- ifelse(lmax > umax, lmax, umax)*1.75
		ymin <- -(ymax)
	}
	melt.real <- subset(tograph, Run == 0)
	melt.real.perc <- getPercentile(melt.real,
		melt.rand, test, valtype)
	melt.real.perc$textloc <- rep(ymax*0.75, nrow(melt.real.perc))
	
	# re-order variables for % betas exceeded
	orderedvars <- order(melt.real.perc$percent)
	melt.rand95 <- melt.rand95[orderedvars,]
	if (test == "absolute") {
		melt.rand95 <- rbind(melt.rand95,
		data.frame(variable="", value=NA))
	} else {
		melt.rand95 <- rbind(melt.rand95,
		data.frame(variable="", value.l=NA, value.u=NA))
	}
	melt.real <- melt.real[orderedvars,]
	melt.real <- rbind(melt.real,
		data.frame(Run=0, variable="", value=NA))
	melt.real.perc <- melt.real.perc[orderedvars,]
	melt.real.perc$percent <- paste("   ", melt.real.perc$percent,"%",sep="")
	melt.real.perc <- rbind(melt.real.perc,
		data.frame(variable="",
			percent=paste("% ", valtype, "\nexceeded", sep=""),
			textloc=ymax*0.75))
	melt.rand <- rbind(melt.rand,
		data.frame(Run=0, variable=as.factor(""), value=NA))
	melt.rand$variable <- factor(melt.rand$variable,
		levels=melt.real.perc$variable)
	
	if (test == "absolute") {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray") +
			geom_boxplot(outlier.shape=NA, alpha=0) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=10) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="gray20", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
		#	ggtitle("Beta values by predictor: lts") +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="gray40"),
			axis.text.y = element_text(size=40, color="gray40"),
			axis.title.x = element_text(size=40, color="gray20"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="gray20"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=1300,units="px", bg = "transparent")
		print(a1)
		dev.off()
	} else {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray") +
			geom_boxplot(outlier.shape=NA, alpha=0) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=10) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="gray20", size=4, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="gray20", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
		#	ggtitle("Beta values by predictor: lts") +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="gray40"),
			axis.text.y = element_text(size=40, color="gray40"),
			axis.title.x = element_text(size=40, color="gray20"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="gray20"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=1300,units="px", bg = "transparent")
		print(a1)
		dev.off()
	}
}

# Group 2: Adults B, SE, & t (absolute vals)
valtypes <- c("betas", "SEs", "t-vals")
testtypes <- c("absolute", "absolute", "absolute")
groups <- c("adu", "adu", "adu")
# Make B and t absolute (SE is already all positive)
aduBetas.abs <- copy(aduBetas)
aduBetas.abs[, value := abs(value)]
aduTs.abs <- copy(aduTs)
aduTs.abs[, value := abs(value)]
graphdists <- list(B= aduBetas.abs,SE=aduSEs,T=aduTs.abs)
for (i in 1:length(graphdists)) {
	valtype <- valtypes[i]
	tograph <- graphdists[[i]]
	test <- testtypes[i]
	group <- groups[i]
	melt.rand <- subset(tograph, Run > 0)
	melt.rand95 <- getSig(melt.rand, test, valtype)
	if (test == "absolute") {
		if (valtype == "SEs") {
			ymax <- sd(melt.rand95$value)*10						
		} else {
			ymax <- max(melt.rand95$value)*2			
		}
		ymin <- 0
	} else {
		lmax <- max(abs(subset(melt.rand95,
			variable != "(Intercept)")$value.l), na.rm=T)
		umax <- max(abs(subset(melt.rand95,
			variable != "(Intercept)")$value.u), na.rm=T)
		ymax <- ifelse(lmax > umax, lmax, umax)*1.75
		ymin <- -(ymax)
	}
	melt.real <- subset(tograph, Run == 0)
	melt.real.perc <- getPercentile(melt.real,
		melt.rand, test, valtype)
	melt.real.perc$textloc <- rep(ymax*0.75, nrow(melt.real.perc))
	
	# re-order variables for % betas exceeded
	orderedvars <- order(melt.real.perc$percent)
	melt.rand95 <- melt.rand95[orderedvars,]
	if (test == "absolute") {
		melt.rand95 <- rbind(melt.rand95,
		data.frame(variable="", value=NA))
	} else {
		melt.rand95 <- rbind(melt.rand95,
		data.frame(variable="", value.l=NA, value.u=NA))
	}
	melt.real <- melt.real[orderedvars,]
	melt.real <- rbind(melt.real,
		data.frame(Run=0, variable="", value=NA))
	melt.real.perc <- melt.real.perc[orderedvars,]
	melt.real.perc$percent <- paste("   ", melt.real.perc$percent,"%",sep="")
	melt.real.perc <- rbind(melt.real.perc,
		data.frame(variable="",
			percent=paste("% ", valtype, "\nexceeded", sep=""),
			textloc=ymax*0.75))
	melt.rand <- rbind(melt.rand,
		data.frame(Run=0, variable=as.factor(""), value=NA))
	melt.rand$variable <- factor(melt.rand$variable,
		levels=melt.real.perc$variable)
	
	if (test == "absolute") {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray") +
			geom_boxplot(outlier.shape=NA, alpha=0) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=10) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="gray20", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
		#	ggtitle("Beta values by predictor: lts") +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="gray40"),
			axis.text.y = element_text(size=40, color="gray40"),
			axis.title.x = element_text(size=40, color="gray20"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="gray20"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=1300,units="px", bg = "transparent")
		print(a1)
		dev.off()
	} else {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray") +
			geom_boxplot(outlier.shape=NA, alpha=0) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=10) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="gray20", size=4, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="gray20", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
		#	ggtitle("Beta values by predictor: lts") +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="gray40"),
			axis.text.y = element_text(size=40, color="gray40"),
			axis.title.x = element_text(size=40, color="gray20"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="gray20"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=1300,units="px", bg = "transparent")
		print(a1)
		dev.off()
	}
}

# Group 3: Child B, SE, t (normal vals)
valtypes <- c("betas", "t-vals")
testtypes <- c("normal", "normal")
groups <- c("chi", "chi")
graphdists <- list(B=chiBetas,T=chiTs)
for (i in 1:length(graphdists)) {
	valtype <- valtypes[i]
	tograph <- graphdists[[i]]
	test <- testtypes[i]
	group <- groups[i]
	melt.rand <- subset(tograph, Run > 0)
	melt.rand95 <- getSig(melt.rand, test, valtype)
	if (test == "absolute") {
		if (valtype == "SEs") {
			ymax <- sd(melt.rand95$value)*10						
		} else {
			ymax <- max(melt.rand95$value)*2			
		}
		ymin <- 0
	} else {
		lmax <- max(abs(subset(melt.rand95,
			variable != "(Intercept)")$value.l), na.rm=T)
		umax <- max(abs(subset(melt.rand95,
			variable != "(Intercept)")$value.u), na.rm=T)
		ymax <- ifelse(lmax > umax, lmax, umax)*1.75
		ymin <- -(ymax)
	}
	melt.real <- subset(tograph, Run == 0)
	melt.real.perc <- getPercentile(melt.real,
		melt.rand, test, valtype)
	melt.real.perc$textloc <- rep(ymax*0.75, nrow(melt.real.perc))
	
	# re-order variables for % betas exceeded
	orderedvars <- order(melt.real.perc$percent)
	melt.rand95 <- melt.rand95[orderedvars,]
	if (test == "absolute") {
		melt.rand95 <- rbind(melt.rand95,
		data.frame(variable="", value=NA))
	} else {
		melt.rand95 <- rbind(melt.rand95,
		data.frame(variable="", value.l=NA, value.u=NA))
	}
	melt.real <- melt.real[orderedvars,]
	melt.real <- rbind(melt.real,
		data.frame(Run=0, variable="", value=NA))
	melt.real.perc <- melt.real.perc[orderedvars,]
	melt.real.perc$percent <- paste("   ", melt.real.perc$percent,"%",sep="")
	melt.real.perc <- rbind(melt.real.perc,
		data.frame(variable="",
			percent=paste("% ", valtype, "\nexceeded", sep=""),
			textloc=ymax*0.75))
	melt.rand <- rbind(melt.rand,
		data.frame(Run=0, variable=as.factor(""), value=NA))
	melt.rand$variable <- factor(melt.rand$variable,
		levels=melt.real.perc$variable)
	
	if (test == "absolute") {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray") +
			geom_boxplot(outlier.shape=NA, alpha=0) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=10) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="gray20", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
		#	ggtitle("Beta values by predictor: lts") +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="gray40"),
			axis.text.y = element_text(size=40, color="gray40"),
			axis.title.x = element_text(size=40, color="gray20"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="gray20"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=2000,units="px", bg = "transparent")
		print(a1)
		dev.off()
	} else {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray") +
			geom_boxplot(outlier.shape=NA, alpha=0) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=10) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="gray20", size=4, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="gray20", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
		#	ggtitle("Beta values by predictor: lts") +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="gray40"),
			axis.text.y = element_text(size=40, color="gray40"),
			axis.title.x = element_text(size=40, color="gray20"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="gray20"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=2000,units="px", bg = "transparent")
		print(a1)
		dev.off()
	}
}

# Group 4: Child B, SE, t (normal vals)
valtypes <- c("betas", "SEs", "t-vals")
testtypes <- c("absolute", "absolute", "absolute")
groups <- c("chi", "chi", "chi")
chiBetas.abs <- copy(chiBetas)
chiBetas.abs[, value := abs(value)]
chiTs.abs <- copy(chiTs)
chiTs.abs[, value := abs(value)]
graphdists <- list(B= chiBetas.abs,SE=chiSEs,T= chiTs.abs)
for (i in 1:length(graphdists)) {
	valtype <- valtypes[i]
	tograph <- graphdists[[i]]
	test <- testtypes[i]
	group <- groups[i]
	melt.rand <- subset(tograph, Run > 0)
	melt.rand95 <- getSig(melt.rand, test, valtype)
	if (test == "absolute") {
		if (valtype == "SEs") {
			ymax <- sd(melt.rand95$value)*10						
		} else {
			ymax <- max(melt.rand95$value)*2			
		}
		ymin <- 0
	} else {
		lmax <- max(abs(subset(melt.rand95,
			variable != "(Intercept)")$value.l), na.rm=T)
		umax <- max(abs(subset(melt.rand95,
			variable != "(Intercept)")$value.u), na.rm=T)
		ymax <- ifelse(lmax > umax, lmax, umax)*1.75
		ymin <- -(ymax)
	}
	melt.real <- subset(tograph, Run == 0)
	melt.real.perc <- getPercentile(melt.real,
		melt.rand, test, valtype)
	melt.real.perc$textloc <- rep(ymax*0.75, nrow(melt.real.perc))
	
	# re-order variables for % betas exceeded
	orderedvars <- order(melt.real.perc$percent)
	melt.rand95 <- melt.rand95[orderedvars,]
	if (test == "absolute") {
		melt.rand95 <- rbind(melt.rand95,
		data.frame(variable="", value=NA))
	} else {
		melt.rand95 <- rbind(melt.rand95,
		data.frame(variable="", value.l=NA, value.u=NA))
	}
	melt.real <- melt.real[orderedvars,]
	melt.real <- rbind(melt.real,
		data.frame(Run=0, variable="", value=NA))
	melt.real.perc <- melt.real.perc[orderedvars,]
	melt.real.perc$percent <- paste("   ", melt.real.perc$percent,"%",sep="")
	melt.real.perc <- rbind(melt.real.perc,
		data.frame(variable="",
			percent=paste("% ", valtype, "\nexceeded", sep=""),
			textloc=ymax*0.75))
	melt.rand <- rbind(melt.rand,
		data.frame(Run=0, variable=as.factor(""), value=NA))
	melt.rand$variable <- factor(melt.rand$variable,
		levels=melt.real.perc$variable)
	
	if (test == "absolute") {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray") +
			geom_boxplot(outlier.shape=NA, alpha=0) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=10) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="gray20", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
		#	ggtitle("Beta values by predictor: lts") +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="gray40"),
			axis.text.y = element_text(size=40, color="gray40"),
			axis.title.x = element_text(size=40, color="gray20"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="gray20"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=2000,units="px", bg = "transparent")
		print(a1)
		dev.off()
	} else {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray") +
			geom_boxplot(outlier.shape=NA, alpha=0) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=10) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="gray20", size=4, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=6, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="gray20", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
		#	ggtitle("Beta values by predictor: lts") +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="gray40"),
			axis.text.y = element_text(size=40, color="gray40"),
			axis.title.x = element_text(size=40, color="gray20"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="gray20"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=2000,units="px", bg = "transparent")
		print(a1)
		dev.off()
	}
}

aduLL[, value := as.numeric(value)]
aduLL.real <- aduLL[Run == 0]
aduLL.rand <- aduLL[Run > 0]
afit1 <- ggplot(data=aduLL.rand, aes(x=value)) + geom_density() +
	ylab("Density") + xlab("Log Likelihood") +
	geom_point(data=aduLL.real, aes(x= value, y=0), size=6) +
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
png(paste(plot.path, "adu-loglik-dist.png", sep=""),
    width=600,height=600,units="px", bg = "transparent")
print(afit1)
dev.off()

aduAIC[, value := as.numeric(value)]
aduAIC.real <- aduAIC[Run == 0]
aduAIC.rand <- aduAIC[Run > 0]
afit2 <- ggplot(data= aduAIC.rand, aes(x=value)) + geom_density() +
	ylab("Density") + xlab("AIC") +
	geom_point(data= aduAIC.real, aes(x= value, y=0), size=6) +
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
png(paste(plot.path, "adu-AIC-dist.png", sep=""),
    width=600,height=600,units="px", bg = "transparent")
print(afit2)
dev.off()

chiLL[, value := as.numeric(value)]
chiLL.real <- chiLL[Run == 0]
chiLL.rand <- chiLL[Run > 0]
cfit1 <- ggplot(data=chiLL.rand, aes(x=value)) + geom_density() +
	ylab("Density") + xlab("Log Likelihood") +
	geom_point(data=chiLL.real, aes(x= value, y=0), size=6) +
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
png(paste(plot.path, "chi-loglik-dist.png", sep=""),
    width=600,height=600,units="px", bg = "transparent")
print(cfit1)
dev.off()

chiAIC[, value := as.numeric(value)]
chiAIC.real <- chiAIC[Run == 0]
chiAIC.rand <- chiAIC[Run > 0]
cfit2 <- ggplot(data= chiAIC.rand, aes(x=value)) + geom_density() +
	ylab("Density") + xlab("AIC") +
	geom_point(data= chiAIC.real, aes(x= value, y=0), size=6) +
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
png(paste(plot.path, "chi-AIC-dist.png", sep=""),
    width=600,height=600,units="px", bg = "transparent")
print(cfit2)
dev.off()

