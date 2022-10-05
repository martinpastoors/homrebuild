# ============================================================================
# HOM SAM simulator
# ============================================================================

rm(list=ls())
gc()

# install.packages("FLCore", repos="http://flr-project.org/R")
library(FLCore)

# install.packages("FLSAM", repos="http://flr-project.org/R")
# library(FLSAM)

# devtools::install_github('fishfollower/SAM/stockassessment', ref='components') - version for FLSAM
library(stockassessment)

library(tidyverse)

sao.name <- "WHOM_2020"
tempdir  <- file.path("D:/temp",sao.name) 
dir.create(tempdir, showWarnings = FALSE)

setwd("D:/TEMP/WHOM_2020")

cn<-read.ices("data/cn.dat")
cw<-read.ices("data/cw.dat")
dw<-read.ices("data/dw.dat")
lf<-read.ices("data/lf.dat")
lw<-read.ices("data/lw.dat")
mo<-read.ices("data/mo.dat")
nm<-read.ices("data/nm.dat")
pf<-read.ices("data/pf.dat")
pm<-read.ices("data/pm.dat")
sw<-read.ices("data/sw.dat")
surveys<-read.ices("data/survey.dat")

# Read input in sam format
dat<-setup.sam.data(surveys=surveys,
                    residual.fleet=cn, 
                    prop.mature=mo, 
                    stock.mean.weight=sw, 
                    catch.mean.weight=cw, 
                    dis.mean.weight=dw, 
                    land.mean.weight=lw,
                    prop.f=pf, 
                    prop.m=pm, 
                    natural.mortality=nm, 
                    land.frac=lf)



conf <- loadConf(dat,"conf/model.cfg", patch=TRUE)
par  <- defpar(dat, conf)
fit  <- sam.fit(dat, conf, par)

# read input in VPA file format
inputs <- c(landings.fraction='lf.dat', catch.n='cn.dat', catch.wt='cw.dat',
            discards.wt='dw.dat', landings.wt='lw.dat', stock.wt='sw.dat',
            mat='mo.dat', m='nm.dat', harvest.spwn='pf.dat', m.spwn='pm.dat')
fqs <- as.list(inputs)
for (i in seq(inputs)) {
  file <- file.path("data",inputs[i])
  fqs[[names(inputs[i])]] <- readVPAFile(file)
}

# Generate FLStock object
FLS  <- FLStock()

# set generate properties
FLS@desc               <- paste("FLStock object generated from SAM:", date(), sep=" ")
FLS@name               <- sao.name

units(FLS)             <- FLCore::standardUnits(FLS)

FLS@range["min"]       <- fit$data$minAge[[1]]
FLS@range["max"]       <- fit$data$maxAge[[1]]
FLS@range["plusgroup"] <- ifelse(fit$conf$maxAgePlusGroup[1]==1,fit$data$maxAge[[1]],NA)
FLS@range["minyear"]   <- min(fit$data$years)
FLS@range["maxyear"]   <- max(fit$data$years)-1
FLS@range["minfbar"]   <- fit$conf$fbarRange[1]
FLS@range["maxfbar"]   <- fit$conf$fbarRange[2]


FLS@catch.n            <- fqs[["catch.n"]] 
FLS@landings.n         <- fqs[["catch.n"]] * fqs[["landings.fraction"]]
FLS@discards.n         <- fqs[["catch.n"]] * (1-fqs[["landings.fraction"]])
FLS@m                  <- fqs[["m"]] %>% FLCore::window(., end=FLS@range["maxyear"])
FLS@mat                <- fqs[["mat"]] %>% FLCore::window(., end=FLS@range["maxyear"])
FLS@catch.wt           <- fqs[["catch.wt"]]
FLS@landings.wt        <- fqs[["landings.wt"]]
FLS@discards.wt        <- fqs[["discards.wt"]]
FLS@stock.wt           <- fqs[["stock.wt"]] %>% FLCore::window(., end=FLS@range["maxyear"])
FLS@harvest.spwn       <- fqs[["harvest.spwn"]] %>% FLCore::window(., end=FLS@range["maxyear"]) 
FLS@m.spwn             <- fqs[["m.spwn"]] %>% FLCore::window(., end=FLS@range["maxyear"])
FLS@landings <- FLS@discards <- FLS@catch  <- FLS@stock <-
  FLQuant(NA, dimnames=list(age="all", year=FLS@range["minyear"]:FLS@range["maxyear"]))
FLS@landings          <- quantSums(FLS@catch.n*FLS@catch.wt)
FLS@discards          <- quantSums(FLS@discards.n * FLS@discards.wt)
FLS@catch             <- quantSums(FLS@catch.n * FLS@catch.wt)

# stock numbers
FLS@stock.n          <- FLQuant(NA, dimnames=list(age=FLS@range["min"]:FLS@range["max"], year=FLS@range["minyear"]:(FLS@range["maxyear"]+1)))
FLS@stock.n[,]       <- exp(fit$pl$logN) 
FLS@stock.n          <- FLCore::window(FLS@stock.n, end=FLS@range["maxyear"])

# harvest
n.ages               <- nrow(fit$pl$logF)
FLS@harvest          <- FLQuant(NA, dimnames=list(age=FLS@range["min"]:FLS@range["max"], year=FLS@range["minyear"]:(FLS@range["maxyear"]+1)))
FLS@harvest[FLS@range["min"]:(FLS@range["min"]+n.ages),] <- exp(fit$pl$logF)
FLS@harvest[(n.ages+1),]    <- FLS@harvest[n.ages,]
FLS@harvest          <- FLCore::window(FLS@harvest, end=FLS@range["maxyear"])
units(FLS@harvest)   <-  "f"

# ssb
FLS@stock            <- ssb(FLS)

# plot(FLS)
# plot(FLS@catch)

# set simulator properties
nsim     <- 1000
set.seed(123)

# Generate FLstock object with iterations
FLSs <- propagate(FLS, nsim)

# Simulate the observations
simdata <- simulate(fit, nsim=nsim,  full.data=TRUE)

# iterations. start with 2, 1 is the real observation.
i <- 2   

while (i <= nsim) {
  
  print(i)
  
  # sim_fit <-  invisible(capture.output(sam.fit(simdata[[i]], conf, par)))
 sim_fit <-  sam.fit(simdata[[i]], conf, par)
  
  if(exists("sim_fit")) {
    
    # stock numbers
    stock.n                 <- FLQuant(NA, dimnames=list(age=FLS@range["min"]:FLS@range["max"], year=FLS@range["minyear"]:(FLS@range["maxyear"]+1)))
    stock.n[,]              <- exp(sim_fit$pl$logN) 
    stock.n                 <- FLCore::window(stock.n, end=FLS@range["maxyear"])
    FLSs[,,,,,i]@stock.n    <- stock.n

    # harvest
    n.ages                  <- nrow(sim_fit$pl$logF)
    harvest                 <- FLQuant(NA, dimnames=list(age=FLS@range["min"]:FLS@range["max"], year=FLS@range["minyear"]:(FLS@range["maxyear"]+1)))
    harvest[FLS@range["min"]:(FLS@range["min"]+n.ages),] <- exp(sim_fit$pl$logF)
    harvest[(n.ages+1),]    <- harvest[n.ages,]
    FLSs[,,,,,i]@harvest    <- FLCore::window(harvest, end=FLS@range["maxyear"])
    units(FLSs[,,,,,i]@harvest)   <-  "f"
    
    # ssb
    FLSs[,,,,,i]@stock            <- ssb(FLSs[,,,,,i])
    
    rm(sim_fit)
    
    i = i + 1
  }
} # end of while statement


save(     FLSs,                   file="test.RData")


getwd()











# DISREGARD THE CODE BELOW. THIS IS OLD AND WILL BE REMOVED

for (i in 1:nsim) {
  
  sim_fit <-  sam.fit(simdata[[i]], conf, par)
  
  # stock numbers
  stock.n                 <- FLQuant(NA, dimnames=list(age=FLS@range["min"]:FLS@range["max"], year=FLS@range["minyear"]:(FLS@range["maxyear"]+1)))
  stock.n[,]              <- exp(sim_fit$pl$logN) 
  FLSs[,,,,,i+1]@stock.n  <- FLCore::window(stock.n, end=FLS@range["maxyear"])
  
  # harvest
  n.ages               <- nrow(sim_fit$pl$logF)
  FLS@harvest          <- FLQuant(NA, dimnames=list(age=FLS@range["min"]:FLS@range["max"], year=FLS@range["minyear"]:(FLS@range["maxyear"]+1)))
  FLS@harvest[FLS@range["min"]:(FLS@range["min"]+n.ages),] <- exp(fit$pl$logF)
  FLS@harvest[(n.ages+1),]    <- FLS@harvest[n.ages,]
  FLS@harvest          <- FLCore::window(FLS@harvest, end=FLS@range["maxyear"])
  units(FLS@harvest)   <-  "f"
  
  # ssb
  FLS@stock            <- ssb(FLS)
  # res                    <- new("FLSAM")
  # res@n.states           <- as.integer(sim_fit[[i]]$noYears)
  # res@name               <- sao.name
  # res@desc               <- paste("iter", i,  
  #                                 "FLSAM object generated:",
  #                                 date(), sep=" ")
  # # Extract ages and plusgroup
  # res@range["min"]       <- sim_fit[[i]]$minAge
  # res@range["max"]       <- sim_fit[[i]]$maxAge
  # res@range["plusgroup"] <- ifelse(sim_fit[[i]]$maxAgePlusGroup[1]==1,sim_fit[[i]]$maxAge,NA)
  # 
  # # Extract years
  # res@range["minyear"] <- min(sim_fit[[i]]$years)
  # res@range["maxyear"] <- max(sim_fit[[i]]$years)
  # 
  # # Extract the bindings
  # res@states             <- sim_fit[[i]]$keyLogFsta   
  # colnames(res@states)   <- res@range["min"]:res@range["max"]
  # rownames(res@states)   <- c("catch",seq(nrow(res@states))[-1])
  # 
  # # Extract the fbar ranges
  # res@range["minfbar"] <- sim_fit[[i]]$fbarRange[1]
  # res@range["maxfbar"] <- sim_fit[[i]]$fbarRange[2]
  # 
  # # stock numbers
  # res@stock.n          <- FLQuant(NA, dimnames=list(age=res@range["min"]:res@range["max"], 
  #                                                   year=res@range["minyear"]:res@range["maxyear"]))
  # res@stock.n[,]       <- exp(sim_fit[[i]]$logN)
  # 
  # # harvest
  # n.ages               <- nrow(sim_fit[[i]]$logF)
  # res@harvest          <- FLQuant(NA, dimnames=list(age=res@range["min"]:res@range["max"], 
  #                                                   year=res@range["minyear"]:res@range["maxyear"]))
  # 
  # res@harvest[res@range["min"]:(res@range["min"]+n.ages),] <- exp(sim_fit[[i]]$logF)
  # res@harvest[(n.ages+1),]    <- res@harvest[n.ages,]
  # units(res@harvest)                   <-  "f"
  # 
  # #Populate the info slot
  # info                 <- data.frame(FLSAM.version   = packageDescription("FLSAM")$Version,
  #                                    FLCore.version = packageDescription("FLCore")$Version,
  #                                    R.version      = R.version$version.string,
  #                                    platform       = R.version$platform,
  #                                    run.date       = Sys.time())
  # res@info <- t(info)
  # colnames(res@info) <- "_"
  # # summary(res)
  # 
  # # CREATE FLStock, drop landings.fraction
  # fls <- do.call("FLStock", fqs[-1])
  # 
  # # CALCULATE landings.n and discards.n
  # fls@landings.n <- fqs[["catch.n"]] * fqs[["landings.fraction"]]
  # fls@discards.n <- fqs[["catch.n"]] * (1-fqs[["landings.fraction"]])
  # 
  # # COMPUTE totals
  # landings(fls) <- computeLandings(fls)
  # discards(fls) <- computeDiscards(fls)
  # catch(fls) <- computeCatch(fls)
  # 
  # # add stock numbers and harvest from FLSAM
  # stock.n(fls) <- res@stock.n
  # harvest(fls) <- res@harvest
  # 
  # # SET to standard units
  # units(fls) <- FLCore::standardUnits(FLS)
  # 
  # # GET fbar range
  # fls@range["minfbar"] <- fit$conf$fbarRange[1]
  # fls@range["maxfbar"] <- fit$conf$fbarRange[2]
  # 
  # # SET name and desc
  # name(fls) <- paste(sao.name, i)
  # desc(fls) <- paste("Loaded from", url)
  # 
  # flstocks[[i]] <- fls
  # 
  # if (nrow(df) == 0) {
  #   df <- 
  #     as.data.frame(ssb(flstocks[[i]])) %>% 
  #     mutate(run=i)
  # } else {
  #   df <- 
  #     df %>% 
  #     bind_rows(., (as.data.frame(ssb(flstocks[[i]])) %>% mutate(run=i)))
  # }
}

df %>% 
  filter(run<=2) %>% 
  ggplot(aes(x=year,y=data, group=run)) +
  theme_bw() +
  geom_line(colour="gray")

# clean up data files; remove temp directory
# unlink(tempdir, recursive=TRUE)

plot(flstocks[[3]])
