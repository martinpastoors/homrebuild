# ============================================================================
# HOM SAM simulator to FLStock
# ============================================================================

rm(list=ls())
gc()

library(FLCore)
library(FLSAM)
library(stockassessment)
library(tidyverse)

source("../SAM2FLR/SAM2FLSTOCK.r")
source("EqSimWHM/R/get_dropbox.R")
source("EqSimWHM/R/utilities.R")

sao.name <- "WHOM_2019"
url      <- paste0("https://stockassessment.org/datadisk/stockassessment/userdirs/user3/",sao.name,"/")
tempdir  <- file.path("D:/temp",sao.name) 
fit      <- stockassessment::fitfromweb(sao.name, character.only=TRUE) 
fls0     <- SAM2FLSTOCK(sao.name, temp="D:\\temp")

# Get dropbox dir; for storing large RData files
dropbox.dir <- file.path(get_dropbox(), "HOM FG", "05. Data","RData")

dir.create(tempdir, showWarnings = FALSE)
write.data.files(fit, dir = tempdir)

inputs <- c(landings.fraction='lf.dat', catch.n='cn.dat', catch.wt='cw.dat',
            discards.wt='dw.dat', landings.wt='lw.dat', stock.wt='sw.dat',
            mat='mo.dat', m='nm.dat', harvest.spwn='pf.dat', m.spwn='pm.dat')

fqs <- as.list(inputs)

for (i in seq(inputs)) {
  file <- file.path("D:/temp",sao.name,inputs[i])
  fqs[[names(inputs[i])]] <- readVPAFile(file)
}
t <-  loadRData(file.path(dropbox.dir,"WGWIDE19_samset0001_0100.RData"))

runs <- 
  loadRData(file.path(dropbox.dir,"WGWIDE19_samset0001_0100.RData")) %>% 
  append(loadRData(file.path(dropbox.dir,"WGWIDE19_samset0101_0200.RData"))) %>% 
  append(loadRData(file.path(dropbox.dir,"WGWIDE19_samset0201_0400.RData"))) %>% 
  append(loadRData(file.path(dropbox.dir,"WGWIDE19_samset0401_0600.RData"))) %>% 
  append(loadRData(file.path(dropbox.dir,"WGWIDE19_samset0601_0800.RData"))) %>% 
  append(loadRData(file.path(dropbox.dir,"WGWIDE19_samset0801_0900.RData"))) %>% 
  append(loadRData(file.path(dropbox.dir,"WGWIDE19_samset0901_0990.RData"))) %>% 
  append(loadRData(file.path(dropbox.dir,"WGWIDE19_samset0990_1000.RData"))) 

attr(runs, "fit") <- fit
attr(runs, "class") <- "samset"
# plot(runs)

# plot(runs)

# get names
# t  <- runs[[1]]
# df <- data.frame(stringsAsFactors = FALSE)
# for (i in names(t)) {
#   for (j in names(t[[i]])) {
#     df <- bind_rows(df, data.frame(list=i, var=j, stringsAsFactors = FALSE))
#     cat(i,j,"\n")
#   }
# }
# df %>% filter(grepl("logn", tolower(var))) 

flstocks <- list()
flstocks[[1]] <- fls0
names(flstocks[1]) <- "ass"

df       <- data.frame(NULL)
i <- 1
for (i in 1:length(runs)) {
  
  cat(i, "\n")
  
  # Create an FLSAM object
  
  res                    <- new("FLSAM")
  res@n.states           <- as.integer(runs[[i]]$data$noYears)
  res@name               <- sao.name
  res@desc               <- paste("iter", i,  
                                  "FLSAM object generated:",
                                  date(), sep=" ")
  # Extract ages and plusgroup
  res@range["min"]       <- runs[[i]]$conf$minAge
  res@range["max"]       <- runs[[i]]$conf$maxAge
  res@range["plusgroup"] <- ifelse(runs[[i]]$conf$maxAgePlusGroup[1]==1,
                                   runs[[i]]$conf$maxAge,NA)
  
  # Extract years
  res@range["minyear"] <- min(runs[[i]]$data$years)
  res@range["maxyear"] <- max(runs[[i]]$data$years)
  
  # Extract the bindings
  res@states             <- runs[[i]]$conf$keyLogFsta   
  colnames(res@states)   <- res@range["min"]:res@range["max"]
  rownames(res@states)   <- c("catch",seq(nrow(res@states))[-1])
  
  # Extract the fbar ranges
  res@range["minfbar"] <- runs[[i]]$conf$fbarRange[1]
  res@range["maxfbar"] <- runs[[i]]$conf$fbarRange[2]
  
  # stock numbers
  res@stock.n          <- FLQuant(NA, dimnames=list(age=res@range["min"]:res@range["max"], 
                                                    year=res@range["minyear"]:res@range["maxyear"]))
  res@stock.n[,]       <- exp(runs[[i]]$pl$logN)
  
  # harvest
  n.ages               <- nrow(runs[[i]]$pl$logF)
  res@harvest          <- FLQuant(NA, dimnames=list(age=res@range["min"]:res@range["max"], 
                                                    year=res@range["minyear"]:res@range["maxyear"]))
  
  res@harvest[res@range["min"]:(res@range["min"]+n.ages),] <- exp(runs[[i]]$pl$logF)
  res@harvest[(n.ages+1),]    <- res@harvest[n.ages,]
  units(res@harvest)                   <-  "f"
  
  #Populate the info slot
  info                 <- data.frame(FLSAM.version  = packageDescription("FLSAM")$Version,
                                     FLCore.version = packageDescription("FLCore")$Version,
                                     R.version      = R.version$version.string,
                                     platform       = R.version$platform,
                                     run.date       = Sys.time())
  res@info <- t(info)
  colnames(res@info) <- "_"
  # summary(res)
  
  # CREATE FLStock, drop landings.fraction
  fls <- do.call("FLStock", fqs[-1])
  
  # CALCULATE landings.n and discards.n
  fls@landings.n <- fqs[["catch.n"]] * fqs[["landings.fraction"]]
  fls@discards.n <- fqs[["catch.n"]] * (1-fqs[["landings.fraction"]])
  
  # COMPUTE totals
  landings(fls) <- computeLandings(fls)
  discards(fls) <- computeDiscards(fls)
  catch(fls) <- computeCatch(fls)
  
  # add stock numbers and harvest from FLSAM
  stock.n(fls) <- res@stock.n
  harvest(fls) <- res@harvest
  
  # SET to standard units
  units(fls) <- standardUnits(fls)
  
  # GET fbar range
  fls@range["minfbar"] <- res@range["minfbar"]
  fls@range["maxfbar"] <- res@range["maxfbar"]
  
  # SET name and desc
  name(fls) <- paste(sao.name, i)
  desc(fls) <- paste("Simulated from", url)
  
  # add to flstocks
  flstocks[[i+1]] <- fls
  names(flstocks[i+1]) <- as.character(i)
  
  # generate data frame of ssb
  if (nrow(df) == 0) {
    df <- 
      as.data.frame(ssb(flstocks[[i]])) %>% 
      mutate(run=i)
  } else {
    df <- 
      df %>% 
      bind_rows(., (as.data.frame(ssb(flstocks[[i]])) %>% mutate(run=i)))
  }
  
  # clean up data files; remove temp directory
  unlink(tempdir, recursive=TRUE)
}

# add names to list
names(flstocks) <- c("ass",as.character(1:length(runs)))

# test plot of data frame
df %>% 
  # filter(run<=2) %>% 
  ggplot(aes(x=year,y=data, group=run)) +
  theme_bw() +
  geom_line(colour="gray")

# save flstocks 
save(fls0,     file=file.path(getwd(), "EqSimWHM","RData",
                              "WGWIDE19_SAM.RData"))
save(flstocks, file=file.path(getwd(), "EqSimWHM","RData",
                              "MSE_WGWIDE19_FLStocks_SAM1000.RData"))
