# Create version of the analysis with the random gap info files we made
# (using the random.runs.R script)

make.replicate <- function (all.data.A, all.data.C, i,
                            info.path, processed.data.path) {

  source("0-useful.R")
  source("0-helper.R")
  
  # Read in one version of the random gaps
  # note - casting to numeric here because of the NA being lower-case in the files 
  r.gap.info <- read.csv(paste(info.path,"GapInfoRandom",i,".csv", sep="")) %>%
    mutate(Gap = as.numeric(as.character(r.gap.info$Gap)), 
           Onset = round(Onset, 3), 
           Offset = round(Offset, 3))
  
  # Extract time windows for switch analysis and prepare new looking columns
  r.target.windows.proport.A <- get.windows.categorical2(all.data.A,  # For adults
                                                         r.gap.info, 
                                                         utt1.overlap.A, 
                                                         utt2.overlap.A, 
                                                         fixation.window) %>%
    mutate(Looks.Origin = Origin == Look.Dir, 
           Looks.Destination = ifelse(((Origin == "L" & Look.Dir == "R") |
                                          (Origin == "R" & Look.Dir == "L")), 1, 0))
  
  r.target.windows.proport.C <- get.windows.categorical2(all.data.C, # For children
                                                         r.gap.info, 
                                                         utt1.overlap.C, 
                                                         utt2.overlap.C, 
                                                         fixation.window) %>%
    mutate(Looks.Origin = Origin == Look.Dir,
           Looks.Destination = ifelse(((Origin == "L" & Look.Dir == "R") |
                                         (Origin == "R" & Look.Dir == "L")), 1, 0))
  
  # Identify anticipatory switches in the data and combine it again
  r.trans.info <- bind_rows(grab.transition.info2(r.target.windows.proport.A,
                                          fixation.window), 
                            grab.transition.info2(r.target.windows.proport.C,
                                          fixation.window))

  # Merge gap and subject info back in for analysis
  switch.final <- r.trans.info %>% 
    left_join(filter(r.gap.info, Condition == "A")) %>%
    left_join(subject.info, by = "Subject") %>%
    arrange(Subject, Gap) %>%
    mutate(SampleType = "RANDOM")

  write.csv(switch.final, paste(processed.data.path,
                                "switch.final.optim.",i,".csv", sep=""))
}
