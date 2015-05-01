source("1-prep.data.R")
source("2-find.switches.R")

get.switches(1,0) # Run on the real gaps
get.switches(1:10,1) # Run on 10 randomly-shuffled gap versions

source("3-plots.stats.analysis.R")