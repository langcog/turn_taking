switch.rand <- filter(switch.all, Sample != "Transition")
switch.real <- filter(switch.all, Sample == "Transition") %>%
               select(-Sample)
switch.real2 <- filter(switch.all, Sample == "Transition")
randswitchrate <- aggregate(Switch ~ Run, switch.rand, mean)
realswitchrate <- aggregate(Switch ~ Run, switch.real2, mean)
switch.rand.C <- filter(switch.rand, Age != "A") %>%
                 mutate(Age = as.numeric(Age))
switch.rand.A <- filter(switch.rand, Age == "A")


ggplot(randswitchrate, aes(x=Switch)) + geom_density() +
	geom_point(data=realswitchrate, aes(x=Switch, y=0), size=4, color="black") +
	geom_point(data=realswitchrate, aes(x=Switch, y=0), size=3, color="red") +
	ggtitle("Anticipatory switches overall\n")

# Set paths
processed.data.path <- "processed_data/" #"../../data/E1/processed_data/"
plot.path <- "plots/" #"plots/"

# Read in all the random runs and combine them
adu.r.files <- dir(processed.data.path, pattern="adu.coefs.r-[0-9]+.csv")
chi.r.files <- dir(processed.data.path, pattern="chi.coefs.r-[0-9]+.csv")

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
aduerrors <- aduruns[Run > 0, c("Error", "Run"), with=F]
chiruns <- combine.runs(chiDT, chi.r.files, "chi")
chierrors <- chiruns[Run > 0, c("Error", "Run"), with=F]

aduerrors <- data.table(aduerrors)
setkey(switch.rand.A, "Run")
setkey(aduerrors, "Run")
newdt.A <- aduerrors[switch.rand.A]
newdt.A$ErrorBin <- ifelse(newdt.A$Error == "", 0, 1)

chierrors <- data.table(chierrors)
setkey(switch.rand.C, "Run")
setkey(chierrors, "Run")
newdt.C <- chierrors[switch.rand.C]
newdt.C$ErrorBin <- ifelse(newdt.C$Error == "", 0, 1)

aggregate(Switch ~ ErrorBin, newdt.C, mean)
#  ErrorBin    Switch
#1        0 0.1164840
#2        1 0.1098459
aggregate(Switch ~ ErrorBin, newdt.A, mean)

runavgs.A <- data.table(aggregate(Switch ~ Run, newdt.A, mean))
aduerrors$ErrorBin <- ifelse(aduerrors$Error == "", 0, 1)
setkey(runavgs.A, "Run")
runavgs.A <- aduerrors[runavgs.A]
runavgs.A$ErrorBin <- as.factor(runavgs.A$ErrorBin)
runavgs.A$ErrorBin <- ifelse(runavgs.A$ErrorBin == 0, "No", "Yes")

runavgs.C <- data.table(aggregate(Switch ~ Run, newdt.C, mean))
chierrors$ErrorBin <- ifelse(chierrors$Error == "", 0, 1)
setkey(runavgs.C, "Run")
runavgs.C <- chierrors[runavgs.C]
runavgs.C$ErrorBin <- as.factor(runavgs.C$ErrorBin)
runavgs.C$ErrorBin <- ifelse(runavgs.C$ErrorBin == 0, "No", "Yes")

ggplot(runavgs.A, aes(x=Switch, group=ErrorBin, fill=ErrorBin)) +
	geom_density(alpha=0.3) +
	ylab("Density") + xlab("Proportion Anticipatory Switches") +
	guides(fill=guide_legend(title="Convergence Error"))
	
ggplot(runavgs.C, aes(x=Switch, group=ErrorBin, fill=ErrorBin)) +
	geom_density(alpha=0.3) +
	ylab("Density") + xlab("Proportion Anticipatory Switches") +
	guides(fill=guide_legend(title="Convergence Error"))

runavgs.A$ErrorBin <- as.factor(runavgs.A$ErrorBin)
mA <- glmer(ErrorBin ~ Switch + (1|Run), data=runavgs.A, family="binomial")

runavgs.C$ErrorBin <- as.factor(runavgs.C$ErrorBin)
mC <- glmer(ErrorBin ~ Switch + (1|Run), data=runavgs.C, family="binomial")
