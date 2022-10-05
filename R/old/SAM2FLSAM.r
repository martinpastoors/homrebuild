# ---------------------------------------------------------------------------------------------------
# SAM2FLSAM.r
# 
# Convert SAM fit object to FLSAM object
# ---------------------------------------------------------------------------------------------------

library(FLCore)
library(FLSAM)
library(stockassessment)


SAM2FLSAM <- function(sao.name) {
  
  ## Get fit from web ###############################################################################
  
  fit      <- stockassessment::fitfromweb(sao.name, character.only=TRUE) 
  
  ## Create FLSAM object ##############################################################################
  
  res                    <- new("FLSAM")
  res@n.states           <- as.integer(fit$data$noYears)
  res@name               <- sao.name
  res@desc               <- paste(attributes(fit)$class, 
                                  attributes(fit)$Version, 
                                  "RemoteSha:",
                                  attributes(fit)$RemoteSha, 
                                  "FLSAM object generated:",
                                  date(), sep=" ")
  # Extract ages and plusgroup
  res@range["min"]       <- fit$conf$minAge
  res@range["max"]       <- fit$conf$maxAge
  res@range["plusgroup"] <- ifelse(fit$conf$maxAgePlusGroup[1]==1,fit$conf$maxAge,NA)
  
  # Extract years
  res@range["minyear"] <- min(fit$data$years)
  res@range["maxyear"] <- max(fit$data$years)
  
  # Extract the bindings
  res@states             <- fit$conf$keyLogFsta   
  colnames(res@states)   <- res@range["min"]:res@range["max"]
  rownames(res@states)   <- c("catch",seq(nrow(res@states))[-1])
  
  # Extract the fbar ranges
  res@range["minfbar"] <- fit$conf$fbarRange[1]
  res@range["maxfbar"] <- fit$conf$fbarRange[2]
  
  # Get number of parameters
  res@nopar            <-  length(names(fit$sdrep$par.fixed))
  
  # Get negative log likelihood
  res@nlogl            <-  fit$opt$objective
  
  # Read standard deviation report (if it exists)
  p                    <- summary(fit$sdrep)
  p                    <- data.frame(1:dim(p)[1],rownames(p),p)
  names(p)             <-   c("index","name","value","std.dev")
  res@params           <- p
  res@params$index     <- NULL
  
  
  #Extract the state variables and store in appropriate stock.n slots
  u                    <- subset(res@params,name=="logN")  
  stateEst             <- matrix(u$value,
                                 ncol = res@n.states, 
                                 dimnames = list(state=NULL, year=res@range["minyear"]:res@range["maxyear"]))
  stateSd              <- matrix(u$std.dev,
                                 ncol=res@n.states,
                                 dimnames=list(state=NULL, year=res@range["minyear"]:res@range["maxyear"]))
  n.ages               <- length(fit$conf$minAge:fit$conf$maxAge)
  res@stock.n          <- FLQuant(NA, dimnames=list(age=res@range["min"]:res@range["max"], 
                                                    year=res@range["minyear"]:res@range["maxyear"]))
  res@stock.n[,]       <- exp(stateEst[1:n.ages,])
  
  # Extract the harvest slot and store in appropriate harvest slot
  u                    <- subset(res@params,name=="logF")  
  stateEst             <- matrix(u$value,
                                 ncol = res@n.states, 
                                 dimnames = list(state=NULL, year=res@range["minyear"]:res@range["maxyear"]))
  stateSd              <- matrix(u$std.dev,
                                 ncol=res@n.states,
                                 dimnames=list(state=NULL, year=res@range["minyear"]:res@range["maxyear"]))
  
  # calculate number of ages for which harvest has been calculated
  n.years              <- fit$data$noYears  
  n.ages               <- dim(u)[1]/n.years
  res@harvest          <- FLQuant(NA, dimnames=list(age=res@range["min"]:res@range["max"], 
                                                    year=res@range["minyear"]:res@range["maxyear"]))
  
  res@harvest[1:n.ages,]      <- exp(stateEst[1:n.ages,])
  res@harvest[(n.ages+1),]    <- res@harvest[n.ages,]
  units(res@harvest)                   <-  "f"
  
  #Populate the info slot
  info                 <- data.frame(FLSAM.version   = packageDescription("FLSAM")$Version,
                                     FLCore.version = packageDescription("FLCore")$Version,
                                     R.version      = R.version$version.string,
                                     platform       = R.version$platform,
                                     run.date       = Sys.time())
  res@info <- t(info)
  colnames(res@info) <- "_"
  
  # to do: include the variance-covariance matrix
  
  return(res)
  
  }

# EXAMPLES
# t        <- SAM2FLSAM(sao.name="WHOM_2019")

