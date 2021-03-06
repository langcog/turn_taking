source("1-prep.data.R")
source("2-find.switches.R")

# Find switches in the real data
get.switches(1, FALSE)

# Find switches in randomly-shuffled gap versions
rand.runs <- 1:5000
make.rand.gaps(rand.runs)
get.switches(rand.runs, TRUE)

source("3-plots.stats.analysis.R")
source("4-model-functions.R")
compare.models()