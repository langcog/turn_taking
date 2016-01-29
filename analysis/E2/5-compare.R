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
aduZ <- data.table(Run = aruns)
for (var in aduvars) {
	aduZ[,c(as.character(var)) := rep(0, nrow(aduZ))]	
}
chiB <- data.table(Run = cruns)
for (var in chivars) {
	chiB[,c(as.character(var)) := rep(0, nrow(chiB))]	
}
chiSE <- data.table(Run = cruns)
for (var in chivars) {
	chiSE[,c(as.character(var)) := rep(0, nrow(chiSE))]	
}
chiZ <- data.table(Run = cruns)
for (var in chivars) {
	chiZ[,c(as.character(var)) := rep(0, nrow(chiZ))]	
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

aduruns <- combine.runs(aduB, aduSE, aduZ, aduFit,
	adu.r.m.files, adu.r.i.files, "adu.max.m-0.csv", "adu.max.i-0.csv")
aduruns[[4]]$ErrorBin <- ifelse(aduruns[[4]]$Error == "", 0, 1)
chiruns <- combine.runs(chiB, chiSE, chiZ, chiFit,
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
aduZs <- melt(aduruns[[3]], names(aduruns[[3]])[1])
aduFits <- melt(aduruns[[4]], names(aduruns[[4]])[1])
aduLL <- aduFits[variable == "LogLik"]
aduAIC <- aduFits[variable == "AIC"]

chiBetas <- melt(chiruns[[1]], names(chiruns[[1]])[1])
chiSEs <- melt(chiruns[[2]], names(chiruns[[2]])[1])
chiZs <- melt(chiruns[[3]], names(chiruns[[3]])[1])
chiFits <- melt(chiruns[[4]], names(chiruns[[4]])[1])
chiLL <- chiFits[variable == "LogLik"]
chiAIC <- chiFits[variable == "AIC"]


aduBetas$variable <- factor(aduBetas$variable, labels=c(
	"(Intercept)", "Prosody only", "Normal speech",       
	"Words only", "Type", "Duration", "Prosody only*Type",
	"Normal speech*Type", "Words only*Type" )) 
aduSEs$variable <- factor(aduSEs$variable, labels=c(
	"(Intercept)", "Prosody only", "Normal speech",       
	"Words only", "Type", "Duration", "Prosody only*Type",
	"Normal speech*Type", "Words only*Type" )) 
aduZs$variable <- factor(aduZs$variable, labels=c(
	"(Intercept)", "Prosody only", "Normal speech",       
	"Words only", "Type", "Duration", "Prosody only*Type",
	"Normal speech*Type", "Words only*Type" )) 

chiBetas$variable <- factor(chiBetas$variable, labels=c(
	 "(Intercept)", "Age", "Prosody only", "Normal speech",
	 "Words only", "Type", "Duration", "Age*Prosody only",
	 "Age*Normal speech", "Age*Words only", "Age*Type",
	 "Prosody only*Type", "Normal speech*Type", "Words only*Type",
	 "Age*Prosody only*Type", "Age*Normal speech*Type",
	 "Age*Words only*Type"))
chiSEs$variable <- factor(chiSEs$variable, labels=c(
	 "(Intercept)", "Age", "Prosody only", "Normal speech",
	 "Words only", "Type", "Duration", "Age*Prosody only",
	 "Age*Normal speech", "Age*Words only", "Age*Type",
	 "Prosody only*Type", "Normal speech*Type", "Words only*Type",
	 "Age*Prosody only*Type", "Age*Normal speech*Type",
	 "Age*Words only*Type"))
chiZs$variable <- factor(chiZs$variable, labels=c(
	 "(Intercept)", "Age", "Prosody only", "Normal speech",
	 "Words only", "Type", "Duration", "Age*Prosody only",
	 "Age*Normal speech", "Age*Words only", "Age*Type",
	 "Prosody only*Type", "Normal speech*Type", "Words only*Type",
	 "Age*Prosody only*Type", "Age*Normal speech*Type",
	 "Age*Words only*Type"))

# Group 1: Adult B & z (normal vals)
valtypes <- c("betas", "z-vals")
testtypes <- c("normal", "normal")
groups <- c("adu", "adu")
graphdists <- list(B=aduBetas,Z=aduZs)
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
		ymax <- ifelse(lmax > umax, lmax, umax)*4
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
			geom_jitter(color="gray60") +
			geom_boxplot(outlier.shape=NA, alpha=0, lwd=2) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=12) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="black"),
			axis.text.y = element_text(size=40, color="black"),
			axis.title.x = element_text(size=40, color="black"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="black"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=1300,units="px", bg = "transparent")
		print(a1)
		dev.off()
	} else {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray60") +
			geom_boxplot(outlier.shape=NA, alpha=0, lwd=2) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=12) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=4, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="black"),
			axis.text.y = element_text(size=40, color="black"),
			axis.title.x = element_text(size=40, color="black"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="black"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=1300,units="px", bg = "transparent")
		print(a1)
		dev.off()
	}
}

# Group 2: Adults B, SE, & z (absolute vals)
valtypes <- c("betas", "SEs", "z-vals")
testtypes <- c("absolute", "absolute", "absolute")
groups <- c("adu", "adu", "adu")
# Make B and z absolute (SE is already all positive)
aduBetas.abs <- copy(aduBetas)
aduBetas.abs[, value := abs(value)]
aduZs.abs <- copy(aduZs)
aduZs.abs[, value := abs(value)]
graphdists <- list(B= aduBetas.abs,SE=aduSEs,Z=aduZs.abs)
for (i in 1:length(graphdists)) {
	valtype <- valtypes[i]
	tograph <- graphdists[[i]]
	test <- testtypes[i]
	group <- groups[i]
	melt.rand <- subset(tograph, Run > 0)
	melt.rand95 <- getSig(melt.rand, test, valtype)
	if (test == "absolute") {
		if (valtype == "SEs") {
			ymax <- sd(melt.rand95$value)*20						
		} else {
			ymax <- max(melt.rand95$value)*3			
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
			geom_jitter(color="gray60") +
			geom_boxplot(outlier.shape=NA, alpha=0, lwd=2) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=12) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="black"),
			axis.text.y = element_text(size=40, color="black"),
			axis.title.x = element_text(size=40, color="black"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="black"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=1300,units="px", bg = "transparent")
		print(a1)
		dev.off()
	} else {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray60") +
			geom_boxplot(outlier.shape=NA, alpha=0, lwd=2) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=12) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=4, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="black"),
			axis.text.y = element_text(size=40, color="black"),
			axis.title.x = element_text(size=40, color="black"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="black"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=1300,units="px", bg = "transparent")
		print(a1)
		dev.off()
	}
}

# Group 3: Child B, SE, z (normal vals)
valtypes <- c("betas", "z-vals")
testtypes <- c("normal", "normal")
groups <- c("chi", "chi")
graphdists <- list(B=chiBetas,Z=chiZs)
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
		ymax <- ifelse(lmax > umax, lmax, umax)*5
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
			geom_jitter(color="gray60") +
			geom_boxplot(outlier.shape=NA, alpha=0, lwd=2) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=12) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="black"),
			axis.text.y = element_text(size=40, color="black"),
			axis.title.x = element_text(size=40, color="black"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="black"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=2000,units="px", bg = "transparent")
		print(a1)
		dev.off()
	} else {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray60") +
			geom_boxplot(outlier.shape=NA, alpha=0, lwd=2) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=12) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=4, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="black"),
			axis.text.y = element_text(size=40, color="black"),
			axis.title.x = element_text(size=40, color="black"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="black"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=2000,units="px", bg = "transparent")
		print(a1)
		dev.off()
	}
}

# Group 4: Child B, SE, z (absolute vals)
valtypes <- c("betas", "SEs", "z-vals")
testtypes <- c("absolute", "absolute", "absolute")
groups <- c("chi", "chi", "chi")
chiBetas.abs <- copy(chiBetas)
chiBetas.abs[, value := abs(value)]
chiZs.abs <- copy(chiZs)
chiZs.abs[, value := abs(value)]
graphdists <- list(B= chiBetas.abs,SE=chiSEs,Z=chiZs.abs)
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
			geom_jitter(color="gray60") +
			geom_boxplot(outlier.shape=NA, alpha=0, lwd=2) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=12) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value),
				color="black", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="black"),
			axis.text.y = element_text(size=40, color="black"),
			axis.title.x = element_text(size=40, color="black"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="black"),
		    plot.margin=unit(c(5,5,5,5),"mm"))
			png(paste(plot.path, group, "-", "randrun-",
				valtype, "-", test, ".png", sep=""),
			width=1300,height=2000,units="px", bg = "transparent")
		print(a1)
		dev.off()
	} else {
		a1 <- ggplot(melt.rand, aes(variable,value)) +
			coord_flip() +
			geom_jitter(color="gray60") +
			geom_boxplot(outlier.shape=NA, alpha=0, lwd=2) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="black", size=12) +
			geom_point(data=melt.real,
				aes(variable,value),
				color="white", size=8) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.l),
				color="black", size=4, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=8, shape=17) +
			geom_point(data=melt.rand95,
				aes(variable,value.u),
				color="black", size=4, shape=17) +
			geom_text(data=melt.real.perc,
				aes(variable, textloc), size=18,
				label=melt.real.perc$percent) +
			xlab("Modeled predictor") +
			ylab(paste("Absolute ", substr(valtype, 1,
				nchar(valtype)-1), " estimate", sep="")) +
			ylim(ymin, ymax) +
		    plot.style + theme(
		    panel.background = element_rect(fill = "transparent",colour = NA),
		  	axis.text.x = element_text(size=40, color="black"),
			axis.text.y = element_text(size=40, color="black"),
			axis.title.x = element_text(size=40, color="black"),
			axis.title.y = element_blank(),
		    plot.background = element_rect(fill = "transparent",colour = NA),
		    strip.text.x = element_text(size=40, color="black"),
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


### Non-converging models
aduruns <- combine.runs(aduB, aduSE, aduZ, aduFit,
	adu.r.m.files, adu.r.i.files, "adu.max.m-0.csv", "adu.max.i-0.csv")
aduruns[[4]]$ErrorBin <- ifelse(aduruns[[4]]$Error == "", 0, 1)
chiruns <- combine.runs(chiB, chiSE, chiZ, chiFit,
	chi.r.m.files, chi.r.i.files, "chi.max.m-0.csv", "chi.max.i-0.csv")
chiruns[[4]]$ErrorBin <- ifelse(chiruns[[4]]$Error == "", 0, 1)

aduZs <- melt(aduruns[[3]], names(aduruns[[3]])[1])
chiZs <- melt(chiruns[[3]], names(chiruns[[3]])[1])

aduZs$Error <- rep(1, nrow(aduZs))
aduZs$Error[which(aduZs$Run %in% adunoerrors$Run)] <- 0
chiZs$Error <- rep(1, nrow(chiZs))
chiZs$Error[which(chiZs$Run %in% chinoerrors$Run)] <- 0

aggregate(value ~ variable + Error, aduZs, mean)
aggregate(value ~ variable + Error, aduZs, median)
aggregate(value ~ variable + Error, aduZs, sd)
aggregate(value ~ variable + Error, aduZs, min)
aggregate(value ~ variable + Error, aduZs, max)

aggregate(value ~ variable + Error, chiZs, mean)
aggregate(value ~ variable + Error, chiZs, median)
aggregate(value ~ variable + Error, chiZs, sd)
aggregate(value ~ variable + Error, chiZs, min)
aggregate(value ~ variable + Error, chiZs, max)