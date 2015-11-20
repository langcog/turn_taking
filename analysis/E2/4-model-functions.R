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
switch.all <- fread(paste(processed.data.path,"SwitchAll.csv",sep=""))
chi.coef <- fread(paste(processed.data.path,"chi.coefs-0.csv",sep=""))
chi.coef.l <- fread(paste(processed.data.path,"chi.coefs.l-0.csv",sep=""))
adu.coef <- fread(paste(processed.data.path,"adu.coefs-0.csv",sep=""))
adu.coef.l <- fread(paste(processed.data.path,"adu.coefs.l-0.csv",sep=""))

run.models <- function(ns) {
    # Split the data into adults and children for separate models
    switch.rand <- filter(switch.all, Sample == "Random") %>%
                   select(-Sample) %>%
				   mutate(Condition = as.factor(Condition),
		   		          Type = as.factor(Type))

	switch.rand$Cond2 <- ifelse((switch.rand$Condition == "normal" |
		switch.rand$Condition == "robot"),"lexical", "non-lexical")

	switch.rand.C <- filter(switch.rand, Age != 21) %>%
         			        mutate(Age = as.numeric(Age))
	switch.rand.A <- filter(switch.rand, Age == 21)

    withCallingHandlers({
        for(i in ns){
            w <- length(warnings())
            errors <- ""
	    	# Update
    	    print(i)

    		# Retrieve the random gap info data for this particular run
	    	rand.data.C <- switch.rand.C[Run == i,]
	    	chi.coef.r <- chi.coef[0,]

    		# Models
	    	# Children
			chi.max.r <- glmer(Switch ~ Age * Condition * Type + Duration + 
 			   (Type|Subject) + (1|Gap), data=rand.data.C,
 			   family=binomial, control = glmerControl(optimizer="bobyqa"))
    		chi.coef.r <- rbind(chi.coef.r,
    			cbind(data.frame(t(sapply(fixef(chi.max.r),c))),
    			data.frame(Sample = "random", Error = errors, Run = i)))
    		write.csv(chi.coef.r, paste(
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
	    	rand.data.C <- switch.rand.C[Run == i,]
	    	chi.coef.l.r <- chi.coef.l[0,]

    		# Models
	    	# Children
			chi.max.l.r <- glmer(Switch ~ Age * Cond2 * Type + Duration + 
			    (Type|Subject) + (1|Gap), data=rand.data.C,
			    family=binomial, control = glmerControl(optimizer="bobyqa"))
    		chi.coef.l.r <- rbind(chi.coef.l.r,
    			cbind(data.frame(t(sapply(fixef(chi.max.l.r),c))),
    			data.frame(Sample = "random", Error = errors, Run = i)))
    		write.csv(chi.coef.l.r, paste(
				processed.data.path, "chi.coefs.l.r-", i, ".csv", sep=""),
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
	    	adu.coef.r <- adu.coef[0,]

		    # Models
    		# Adults
			adu.max.r <- glmer(Switch ~ Condition * Type + Duration + 
			    (Type+Condition|Subject) + (1|Gap), data=rand.data.A,
			    family=binomial, control = glmerControl(optimizer="bobyqa"))
    		adu.coef.r <- rbind(adu.coef.r,
    			cbind(data.frame(t(sapply(fixef(adu.max.r),c))),
    			data.frame(Sample = "random", Error = errors, Run = i)))
    		write.csv(adu.coef.r, paste(
				processed.data.path, "adu.coefs.r-", i, ".csv", sep=""),
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
	    	adu.coef.l.r <- adu.coef.l[0,]

		    # Models
    		# Adults
			adu.max.l.r <- glmer(Switch ~ Cond2 * Type + Duration + 
			    (Type*Cond2|Subject) + (1|Gap), data=rand.data.A,
			    family=binomial, control = glmerControl(optimizer="bobyqa"))
    		adu.coef.l.r <- rbind(adu.coef.l.r,
    			cbind(data.frame(t(sapply(fixef(adu.max.l.r),c))),
    			data.frame(Sample = "random", Error = errors, Run = i)))
    		write.csv(adu.coef.l.r, paste(
				processed.data.path, "adu.coefs.l.r-", i, ".csv", sep=""),
				row.names=FALSE)
        }
    }, warning = function(w){
	    if (errors == "") {
	        errors <<- w$message
        }
        invokeRestart("muffleWarning")
    })
}