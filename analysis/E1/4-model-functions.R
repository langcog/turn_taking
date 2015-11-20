source("0-helper.R")
library(dplyr)
library(ggplot2)
library(lme4)
library(gridExtra)
library(scales)  
library(Hmisc)
library(data.table)

################################################################################
# Read in data in processed_data/ path and set plotting variables
################################################################################
processed.data.path <- "processed_data/" #"../../data/E1/processed_data/"
switch.all <- fread(paste(processed.data.path,"SwitchAll.csv", sep=""))
chi.coefs <- fread(paste(processed.data.path,"chi.coefs-0.csv", sep=""))
adu.coefs <- fread(paste(processed.data.path,"adu.coefs-0.csv", sep=""))

run.models <- function(ns) {
    # Split the data into adults and children for separate models
    switch.rand <- filter(switch.all, Sample == "Random") %>%
                   select(-Sample)
                   
    switch.rand.C <- filter(switch.rand, Age != "A") %>%
                     mutate(Age = as.numeric(Age))
    switch.rand.A <- filter(switch.rand, Age == "A")

    withCallingHandlers({
        for(i in ns){
            w <- length(warnings())
            errors <- ""
	    	# Update
    	    print(i)

    		# Retrieve the random gap info data for this particular run
	    	rand.data.C <- switch.rand.C[Run == i,]
	    	chi.coefs.r <- chi.coefs[0,]

    		# Models
	    	# Children
			chi.max.r <- glmer(Switch ~ Age * LgGroup * Type + Duration + 
			    (Type * LgGroup|Subject) + (1|Gap), data=rand.data.C,
			    family=binomial, control = glmerControl(optimizer="bobyqa"))
    		chi.coefs.r <- rbind(chi.coefs.r,
    			cbind(data.frame(t(sapply(fixef(chi.max.r),c))),
    			data.frame(Sample = "random", Error = errors, Run = i)))
    		write.csv(chi.coefs.r, paste(
				processed.data.path, "chi.coefs.r-", i, ".csv", sep=""),
				row.names=FALSE)		
        }
    }, warning = function(w){
	    if (errors == "") {
	        errors <<- w$message
        }
        invokeRestart("muffleWarning")
    })

    withCallingHandlers({
        for(i in ns){
            w <- length(warnings())
            errors <- ""
    		# Update
        	print(i)

    		# Retrieve the random gap info data for this particular run
	    	rand.data.A <- switch.rand.A[Run == i,]	
	    	adu.coefs.r <- adu.coefs[0,]
		
		    # Models
    		# Adults
			adu.max.r <- glmer(Switch ~ LgGroup * Type * Duration + 
			    (Type * LgGroup|Subject) + (1|Gap), data=rand.data.A,
    			family=binomial, control = glmerControl(optimizer="bobyqa"))
    		adu.coefs.r <- rbind(adu.coefs.r,
    			cbind(data.frame(t(sapply(fixef(adu.max.r),c))),
    			data.frame(Sample = "random", Error = errors, Run = i)))
    		write.csv(adu.coefs.r, paste(
				processed.data.path, "adu.coefs.r-", i, ".csv", sep=""),
				row.names=FALSE)
        }
    }, warning = function(w){
	    if (errors == "") {
	        errors <<- w$message
        }
        invokeRestart("muffleWarning")
    })
}

