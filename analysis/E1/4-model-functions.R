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
model.path <- "processed_data/models/"
switch.all <- fread(paste(processed.data.path,"SwitchAll.csv", sep=""))

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

			chi.max.r <- glmer(Switch ~ Age * LgGroup * Type + Duration + 
			    (Type * LgGroup|Subject) + (1|Gap), data=rand.data.C,
			    family=binomial, control = glmerControl(optimizer="bobyqa"))
			ll.cmr <- logLik(chi.max.r)
			    
			chi.max.r.m <- data.frame(
				B = fixef(chi.max.r),
				SE = SEstat(chi.max.r),
				z = zstat(chi.max.r))
			chi.max.r.m <- cbind(Predictor = rownames(chi.max.r.m), chi.max.r.m)
			rownames(chi.max.r.m) <- NULL
				
			chi.max.r.i <- data.frame(
				LogLik = head(ll.cmr), AIC = AIC(ll.cmr),
				NumObs = as.numeric(unlist(attributes(ll.cmr))[1]),
				Sample = "random", Error = errors, Run = i)
				
			chi.max.r.r <- resid(chi.max.r)
			 
    		write.csv(chi.max.r.m, paste(
				model.path, "chi.max.r.m-", i, ".csv", sep=""),
				row.names=FALSE)

    		write.csv(chi.max.r.i, paste(
				model.path, "chi.max.r.i-", i, ".csv", sep=""),
				row.names=FALSE)

    		write.csv(chi.max.r.r, paste(
				model.path, "chi.max.r.r-", i, ".csv", sep=""),
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
		
			adu.max.r <- glmer(Switch ~ LgGroup * Type * Duration + 
			(Type * LgGroup|Subject) + (1|Gap), data=rand.data.A,
    			family=binomial, control = glmerControl(optimizer="bobyqa"))
			ll.amr <- logLik(adu.max.r)
			    
			adu.max.r.m <- data.frame(
				B = fixef(adu.max.r),
				SE = SEstat(adu.max.r),
				z = zstat(adu.max.r))
			adu.max.r.m <- cbind(Predictor = rownames(adu.max.r.m), adu.max.r.m)
			rownames(adu.max.r.m) <- NULL
				
			adu.max.r.i <- data.frame(
				LogLik = head(ll.amr), AIC = AIC(ll.amr),
				NumObs = as.numeric(unlist(attributes(ll.amr))[1]),
				Sample = "random", Error = errors, Run = i)
				
			adu.max.r.r <- resid(adu.max.r)
			 
    		write.csv(adu.max.r.m, paste(
				model.path, "adu.max.r.m-", i, ".csv", sep=""),
				row.names=FALSE)

    		write.csv(adu.max.r.i, paste(
				model.path, "adu.max.r.i-", i, ".csv", sep=""),
				row.names=FALSE)

    		write.csv(adu.max.r.r, paste(
				model.path, "adu.max.r.r-", i, ".csv", sep=""),
				row.names=FALSE)

        }
    }, warning = function(w){
	    if (errors == "") {
	        errors <<- w$message
        }
        invokeRestart("muffleWarning")
    })
}

