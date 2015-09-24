source("2-find.switches.R")

ns <- as.integer(commandArgs(trailingOnly = TRUE))
make.rand.gaps(ns)
get.switches(ns, random = TRUE)
