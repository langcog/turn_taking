rm(list=ls())
source("0-helper.R")

################################################################################
# Read in data in processed_data/ path and set plotting variables
################################################################################
# Set paths
processed.data.path <- "processed_data/" #"../../data/E1/processed_data/"
plot.path <- "plots/" #"plots/"

# Read in all the random runs and combine them
adu.r.files <- dir(processed.data.path, pattern="adu.coefs.r-[0-9]+.csv")
chi.r.files <- dir(processed.data.path, pattern="chi.coefs.r-[0-9]+.csv")
adu.l.r.files <- dir(processed.data.path, pattern="adu.coefs.l.r-[0-9]+.csv")
chi.l.r.files <- dir(processed.data.path, pattern="chi.coefs.l.r-[0-9]+.csv")

adu.r.model <- read.csv(paste(processed.data.path, adu.r.files[1], sep=""))
chi.r.model <- read.csv(paste(processed.data.path, chi.r.files[1], sep=""))
adu.model <- data.table(read.csv(paste(processed.data.path, pattern="adu.coefs-0.csv", sep="")))
chi.model <- data.table(read.csv(paste(processed.data.path, pattern="chi.coefs-0.csv", sep="")))

# Initialize an empty data table
aduN <- length(adu.r.files)+1
chiN <- length(chi.r.files)+1

aduDT <- data.table(
	matrix(0, aduN, length(adu.r.model),
	dimnames=list(c(), colnames(adu.r.model))))
aduDT$Sample <- rep("", aduN)
aduDT$Error <- rep("", aduN)

chiDT <- data.table(
	matrix(0, chiN, length(chi.r.model),
	dimnames=list(c(), colnames(chi.r.model))))
chiDT$Sample <- rep("", chiN)
chiDT$Error <- rep("", chiN)

aduruns <- combine.runs(aduDT, adu.r.files, "adu")
adu.model$Sample <- as.character(adu.model$Sample) 
adu.model$Error <- as.character(adu.model$Error) 
adu.model$Run <- as.numeric(adu.model$Run)
aduruns[1, names(aduruns) := adu.model]
aduruns.all <- aduruns
aduruns <- subset(aduruns, Error == "" | Sample == "real")
nrow(aduruns)/nrow(aduruns.all)

chiruns <- combine.runs(chiDT, chi.r.files, "chi")
chi.model$Sample <- as.character(chi.model$Sample) 
chi.model$Error <- as.character(chi.model$Error) 
chi.model$Run <- as.numeric(chi.model$Run)
chiruns[1, names(chiruns) := chi.model]
chiruns.all <- chiruns
chiruns <- subset(chiruns, Error == "" | Sample == "real")
nrow(chiruns)/nrow(chiruns.all)

# Derive the absolute values of the coefficients for
adurunsabs <- cbind(abs(aduruns[, !c("Sample", "Error", "Run"), with=FALSE]),
	aduruns[, c("Sample", "Error", "Run"), with=FALSE])

chirunsabs <- cbind(abs(chiruns[, !c("Sample", "Error", "Run"), with=FALSE]),
	chiruns[, c("Sample", "Error", "Run"), with=FALSE])

########## Plots ##########


### Adults ###
newadunames <- c("X-Intercept", "Muffled", "Normal", "Robot",
	"TurnType", "Duration", "Muffled:TurnType", "Normal:TurnType",
	"Robot:TurnType", "Sample", "Error", "Run")
setnames(aduruns, names(aduruns), newadunames)
adumelt <- melt(aduruns, names(aduruns)[(length(aduruns)-2):length(aduruns)])
setnames(adumelt, "value", "tvalue")

adumelt.abs <- adumelt
adumelt.abs$tvalue <- abs(adumelt.abs$tvalue)

aduymax <- 15
adumelt.abs.rand <- subset(adumelt.abs, Sample == "random")
adumelt.abs.rand95 <- get95ths(adumelt.abs.rand, 1, aduruns)
adumelt.abs.real <- subset(adumelt.abs, Sample == "real")
adumelt.abs.real.perc <- getPercentile(adumelt.abs.real,
	adumelt.abs.rand, 1)
adumelt.abs.real.perc$textloc <- rep(aduymax-1, nrow(adumelt.abs.real.perc))

# re-order variables for % betas exceeded
aduorderedvars.abs <- order(adumelt.abs.real.perc$percent)
adumelt.abs.rand95 <- adumelt.abs.rand95[aduorderedvars.abs,]
adumelt.abs.real <- adumelt.abs.real[aduorderedvars.abs,]
adumelt.abs.real.perc <- adumelt.abs.real.perc[aduorderedvars.abs,]
adumelt.abs.rand$variable <- factor(adumelt.abs.rand$variable,
	levels=adumelt.abs.real.perc$variable)

a1 <- ggplot(adumelt.abs.rand, aes(variable,tvalue)) +
	coord_flip() +
	geom_jitter(color="gray") +
	geom_boxplot(outlier.shape=NA, alpha=0) +
	geom_point(data=adumelt.abs.real,
		aes(variable,tvalue),
		color="black", size=5) +
	geom_point(data=adumelt.abs.real,
		aes(variable,tvalue),
		color="red3", size=4) +
	geom_point(data=adumelt.abs.rand95,
		aes(variable,tvalue),
		color="black", size=3, shape=8) +
	geom_text(data=adumelt.abs.real.perc,
		aes(variable, textloc), size=10,
		label=paste(adumelt.abs.real.perc$percent,"%",sep="")) +
	xlab("Modeled predictor") +
	ylab("Absolute beta estimate") +
	ggtitle("Beta values by predictor: Adults") +
	ylim(0, aduymax) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
	title = element_text(size=34, color="black", vjust=5),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20", vjust=3),
    plot.background = element_rect(fill = "transparent",colour = NA),
    strip.text.x = element_text(size=30, color="gray20"),
    legend.justification=c(1,0), legend.position=c(1,0),
    legend.title = element_text(colour="gray20", size=24),
    legend.text = element_text(colour="gray20", size=26),
    legend.key.width = unit(4, "lines"), legend.key.height = unit(2, "lines"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "adu-randrun-dist.png", sep=""),
    width=1300,height=800,units="px", bg = "transparent")
print(a1)
dev.off()

# adumelt.rand <- subset(adumelt, Sample == "random")
# adumelt.rand95 <- get95ths(adumelt.rand, 2, aduruns)
# adumelt.real <- subset(adumelt, Sample == "real")
# adumelt.real.perc <- getPercentile(adumelt.real, adumelt.rand, 2)

# ggplot(adumelt.rand, aes(variable,tvalue)) +
	# coord_flip() +
	# geom_jitter(color="gray") +
	# geom_boxplot(outlier.shape=NA) +
	# geom_point(data=adumelt.rand95,
		# aes(variable,tvalue.l),
		# color="black", size=1, shape=8) +
	# geom_point(data=adumelt.rand95,
		# aes(variable,tvalue.u),
		# color="black", size=1, shape=8) +
	# geom_point(data=adumelt.real,
		# aes(variable,tvalue),
		# color="black", size=4) +
	# geom_point(data=adumelt.real,
		# aes(variable,tvalue),
		# color="red3", size=3) +
	# xlab("Modeled predictor") +
	# ylab("t-value") +
	# ylim(-50,50)
	
####################
newchinames <- c("X-Intercept", "Age", "Muffled", "Normal", "Robot",
	"TurnType", "Duration", "Age:Muffled", "Age:Normal", "Age:Robot", "Age:TurnType",
	"Muffled:TurnType", "Normal:TurnType", "Robot:TurnType", 
	"Age:Muffled:TurnType", "Age:Normal:TurnType", "Age:Robot:TurnType",
	"Sample", "Error", "Run")
setnames(chiruns, names(chiruns), newchinames)
chimelt <- melt(chiruns, names(chiruns)[(length(chiruns)-2):length(chiruns)])
setnames(chimelt, "value", "tvalue")

chimelt.abs <- chimelt
chimelt.abs$tvalue <- abs(chimelt.abs$tvalue)

chiymax <- 10
chimelt.abs.rand <- subset(chimelt.abs, Sample == "random")
chimelt.abs.rand95 <- get95ths(chimelt.abs.rand, 1, chiruns)
chimelt.abs.real <- subset(chimelt.abs, Sample == "real")
chimelt.abs.real.perc <- getPercentile(chimelt.abs.real,
	chimelt.abs.rand, 1)
chimelt.abs.real.perc$textloc <- rep(chiymax-1, nrow(chimelt.abs.real.perc))

# re-order variables for % betas exceeded
chiorderedvars.abs <- order(chimelt.abs.real.perc$percent)
chimelt.abs.rand95 <- chimelt.abs.rand95[chiorderedvars.abs,]
chimelt.abs.real <- chimelt.abs.real[chiorderedvars.abs,]
chimelt.abs.real.perc <- chimelt.abs.real.perc[chiorderedvars.abs,]
chimelt.abs.rand$variable <- factor(chimelt.abs.rand$variable,
	levels=chimelt.abs.real.perc$variable)

c1 <- ggplot(chimelt.abs.rand, aes(variable,tvalue)) +
	coord_flip() +
	geom_jitter(color="gray") +
	geom_boxplot(outlier.shape=NA, alpha=0) +
	geom_point(data=chimelt.abs.real,
		aes(variable,tvalue),
		color="black", size=5) +
	geom_point(data=chimelt.abs.real,
		aes(variable,tvalue),
		color="red3", size=4) +
	geom_point(data=chimelt.abs.rand95,
		aes(variable,tvalue),
		color="black", size=3, shape=8) +
	geom_text(data=chimelt.abs.real.perc,
		aes(variable, textloc), size=10,
		label=paste(chimelt.abs.real.perc$percent,"%",sep="")) +
	xlab("Modeled predictor") +
	ylab("Absolute beta estimate") +
	ggtitle("Beta values by predictor: Children") +
	ylim(0, chiymax) +
    plot.style + theme(
    panel.background = element_rect(fill = "transparent",colour = NA),
	title = element_text(size=34, color="black", vjust=5),
  	axis.text.x = element_text(size=30, color="gray40"),
	axis.text.y = element_text(size=30, color="gray40"),
	axis.title.x = element_text(size=30, color="gray20"),
	axis.title.y = element_text(size=30, color="gray20", vjust=3),
    plot.background = element_rect(fill = "transparent",colour = NA),
    strip.text.x = element_text(size=30, color="gray20"),
    legend.justification=c(1,0), legend.position=c(1,0),
    legend.title = element_text(colour="gray20", size=24),
    legend.text = element_text(colour="gray20", size=26),
    legend.key.width = unit(4, "lines"), legend.key.height = unit(2, "lines"),
    legend.key = element_rect(fill = "transparent", colour = "transparent"),
    legend.background = element_rect(fill="transparent"),
    plot.margin=unit(c(10,10,10,10),"mm"))
png(paste(plot.path, "chi-randrun-dist.png", sep=""),
    width=1300,height=800,units="px", bg = "transparent")
print(c1)
dev.off()

# chimelt.rand <- subset(chimelt, Sample == "random")
# chimelt.rand95 <- get95ths(chimelt.rand, 2, chiruns)
# chimelt.real <- subset(chimelt, Sample == "real")
# chimelt.real.perc <- getPercentile(chimelt.real, chimelt.rand, 2)

# ggplot(chimelt.rand, aes(variable,tvalue)) +
	# coord_flip() +
	# geom_jitter(color="gray") +
	# geom_boxplot(outlier.shape=NA) +
	# geom_point(data=chimelt.rand95,
		# aes(variable,tvalue.l),
		# color="black", size=1, shape=8) +
	# geom_point(data=chimelt.rand95,
		# aes(variable,tvalue.u),
		# color="black", size=1, shape=8) +
	# geom_point(data=chimelt.real,
		# aes(variable,tvalue),
		# color="black", size=4) +
	# geom_point(data=chimelt.real,
		# aes(variable,tvalue),
		# color="red3", size=3) +
	# xlab("Modeled predictor") +
	# ylab("t-value") +
	# ylim(-10,10)
	