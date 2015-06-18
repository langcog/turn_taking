source("1-prep.data.R")
source("2-find.switches.R")

# Find switches in the real data
get.switches(1, FALSE)

# Find switches in randomly-shuffled gap versions
rand.runs <- 1:50
make.rand.gaps(rand.runs)
get.switches(rand.runs, TRUE)

source("3-plots.stats.analysis.R")
run.models(rand.runs)

#### BENCHMARKING LIBRARIES
# library(devtools)
# devtools::install_github("hadley/lineprof")
# library(lineprof)
# library(microbenchmark)

# ###########
# l <- lineprof(get.switches(1, FALSE))
# microbenchmark(get.switches(1, FALSE), times = 1)

# ########### Middy did not try to fix parallelization yet
# library(foreach)
# library(doParallel)

# start <- Sys.time()

# #setup parallel backend 
# cl<-makeCluster(4, method="FORK")
# registerDoParallel(cl, cores = 4)

# foreach (i = 1:4, .packages = "dplyr") %dopar% {
  # make.replicate(all.data.A, all.data.C, i, info.path, processed.data.path) 
# }

# stopCluster(cl)
# stoptime <- Sys.time() - start
