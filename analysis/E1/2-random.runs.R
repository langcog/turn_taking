rm(list = ls())
source("0-useful.R")
source("0-helper.R")
# source("0-find.transitions.R")

#### BENCHMARKING LIBRARIES
# library(devtools)
# devtools::install_github("hadley/lineprof")
library(lineprof)
library(microbenchmark)

# back to other things
raw.data.path <- "../../data/E1/tracker_data/"
info.path <- "../../data/E1/info/"
processed.data.path <- "../../data/E1/processed_data/"

# Read in supplemental data and prep
vid.info <- fread(paste(info.path,"VideoSegmentInfo.csv",sep=""))
subject.info <- fread(paste(info.path,"SubjectInfo.csv",sep=""))

all.data <- fread(paste(processed.data.path,"r.all.data.csv",sep=""))

# Separate adult and child data for finding anticipatory switches
# because adults have an assumed faster gaze planning time of 200 msec
# compared to our assumed 333 msec for children
all.data.A <- subset(all.data, AgeGroup == "ADU")
all.data.C <- subset(all.data, AgeGroup == "CHI")

# Anticipatory switch identification variables
fixation.window = 0.1
# could change utt settings to "reaction" rather than "anticipation" values
utt1.overlap.A = 0.2
utt2.overlap.A = 0.2
utt1.overlap.C = 0.333
utt2.overlap.C = 0.333

###
source("2-replicate_source.R")
l <- lineprof(make.replicate(all.data.A, all.data.C, i, info.path, processed.data.path))
microbenchmark(make.replicate(all.data.A, all.data.C, i, info.path, processed.data.path),
               times = 1)

# time     alloc   release     dups                     ref                                    src
# 1   0.007     0.716     0.000      317 2-replicate_source.R#14 make.replicate/read.csv               
# 2   8.069  4157.738  4008.718  5843236 2-replicate_source.R#22 make.replicate/get.windows.categorical
# 3   0.001     0.127     0.000      266 2-replicate_source.R#24 make.replicate/==                     
# 4   0.017     5.131     0.000     2334 2-replicate_source.R#24 make.replicate/$<-                    
# 5   0.022     0.922     0.000     1217 2-replicate_source.R#27 make.replicate/ifelse                 
# 6   0.024     5.796     0.000     3552 2-replicate_source.R#27 make.replicate/$<-                    
# 7  26.872  6007.791  6122.486  5842109 2-replicate_source.R#33 make.replicate/get.windows.categorical
# 8   0.003     0.000     0.000      925 2-replicate_source.R#35 make.replicate/==                     
# 9   0.001     0.536     0.000        0 2-replicate_source.R#35 make.replicate/as.integer             
# 10  0.093    24.446     0.000     2334 2-replicate_source.R#35 make.replicate/$<-                    
# 11  0.174     6.229    38.503     1217 2-replicate_source.R#38 make.replicate/ifelse                 
# 12  0.097    25.390     0.000     2396 2-replicate_source.R#38 make.replicate/$<-                    
# 13  0.001     0.109     0.000     1207 2-replicate_source.R#45 make.replicate/print                  
# 14 18.370 25950.507 25998.201 28533459 2-replicate_source.R#46 make.replicate/grab.transition.info   
# 15  0.001     0.748     0.000      174 2-replicate_source.R#53 make.replicate/print                  
# 16  0.004     1.898     0.000     2256 2-replicate_source.R#54 make.replicate/merge                  
# 17  0.001     0.362     0.000      409 2-replicate_source.R#57 make.replicate/[                      
# 18  0.001     0.125     0.000      364 2-replicate_source.R#62 make.replicate                        
# 19  0.048     0.292     0.000      505 2-replicate_source.R#64 make.replicate/write.csv              

microbenchmark(get.windows.categorical(r.all.data.C,
                                 r.gap.info, utt1.overlap.C, utt2.overlap.C, fixation.window),
               times = 2)
microbenchmark(get.windows.categorical2(r.all.data.C,
                                 r.gap.info, utt1.overlap.C, utt2.overlap.C, fixation.window),
              times = 2)

td <- get.windows.categorical(r.all.data.A,
                              r.gap.info, utt1.overlap.A, utt2.overlap.A, fixation.window)
td2 <- get.windows.categorical2(r.all.data.A,
                         r.gap.info, utt1.overlap.A, utt2.overlap.A, fixation.window)

foo <- get.gap(gaps$Condition[1], gaps$Onset[1], gaps$Offset[1], gaps$SpeakerPrev[1], 
        gaps$Type[1], gaps$Gap[1],
        obs, utt1.window, utt2.window, fixation.window)

foo2 <- get.gap2(gaps$Condition[1], gaps$Onset[1], gaps$Offset[1], gaps$SpeakerPrev[1], 
         gaps$Type[1], gaps$Gap[1],
        obs, utt1.window, utt2.window, fixation.window)

microbenchmark(get.gap(gaps$Condition[1], gaps$Onset[1], gaps$Offset[1], gaps$SpeakerPrev[1], 
               gaps$Type[1], gaps$Gap[1],
               obs, utt1.window, utt2.window, fixation.window))

microbenchmark(get.gap2(gaps$Condition[1], gaps$Onset[1], gaps$Offset[1], gaps$SpeakerPrev[1], 
                 gaps$Type[1], gaps$Gap[1],
                 obs, utt1.window, utt2.window, fixation.window))


# transition info optimization
tis <- grab.transition.info(r.target.windows.proport.A, fixation.window)
microbenchmark(grab.transition.info(r.target.windows.proport.A, fixation.window), 
                      times = 2)

tis2 <- grab.transition.info2(r.target.windows.proport.A, fixation.window)
microbenchmark(grab.transition.info2(r.target.windows.proport.A, fixation.window), 
               times = 2)


library(foreach)
library(doParallel)

start <- Sys.time()

#setup parallel backend 
cl<-makeCluster(4, method="FORK")
registerDoParallel(cl, cores = 4)

foreach (i = 1:4, .packages = "dplyr") %dopar% {
  make.replicate(all.data.A, all.data.C, i, info.path, processed.data.path) 
}

stopCluster(cl)
stoptime <- Sys.time() - start





# Read in all the switch results from the random runs and combine them
files <- dir(processed.data.path, pattern="switch.final.+.csv")
rr.data <- data.frame()
for (file.name in files) {
    print(file.name)
    data <- read.csv(paste(processed.data.path, file.name, sep=""))
    rr.data <- rbind(rr.data, data)
}

# If the random runs are for the anticipatory window:
write.csv(rr.data, paste(processed.data.path,"random.runs.anticipatory.csv",sep=""))
# If the random runs are for the reactive window:
#write.csv(rr.data, paste(processed.data.path,"random.runs.reactive.csv",sep=""))