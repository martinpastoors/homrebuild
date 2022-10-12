# 00_SS simulator.R

# purpose: generate 1000 random population from variance-covariance matrix from SS results
# created: 19/04/2017, Piera Carpi
# updated: 26/06/2019, Gwladys Lambert 
# udpated: 11/10/2022, Martin Pastoors

#setup
rm(list=ls())
gc()

# Load libraries
library(MASS)
library(data.table) # I run it with r4ss version 1.35.1
library(Hmisc) # GL- required for checkBound
library(r4ss) # GL -but function could probably be altered not to require %nin%
library(matrixcalc)
library(dplyr)
library(FLCore)
library(ggplot2)
library(tidyr)

# SOURCE FUNCTIONS
source(file.path("R", 'FUN_Random_population.R'))
source(file.path("R", "SS3toFLStock_v3.30_AC.r"))
source(file.path("R", "utilities.r"))

#base assessment
Base <- "SS2022"

# Create simulation folders
dir.create(file.path("datasim", Base)) #GL - create that folder in here
dir.create(file.path("datasim", Base, "Set_newParameter"))

inpDir <- file.path("data",Base)
outDir <- file.path(inpDir, "random_pop")
MSEDir <- file.path(outDir,"MSEInputs")

# Get SS ADMB results
getFit <- read.admbFit(file.path(inpDir, "ss"))

# get variance-covariance matrix and estimates (will be my SD and mu for multivariate normal distribution)
covMat <- getFit$cov
mu     <- getFit$est

# then get the parameters with the bounds
mySS <- SS_output(dir=inpDir,covar=T, verbose=F, forecast=TRUE)

# convert to FLStock object
WHOM       <- SS3toFLStock(inpDir,stockname="WHOM",fbar_amin=1,fbar_amax=10)
WHOM       <- FLCore::setPlusGroup(WHOM,15)
WHOM@stock <- ssb(WHOM)
FLS        <- WHOM

# save(FLS, file = file.path(Base,"RData",paste0("WHOM_SS",substr(Base,7,8),"_FLSs_Clean.RData")))
# load(     file = file.path(getwd(),Base,"RData",paste0("WHOM_SS",substr(Base,7,8),"_FLSs_Clean.RData")))

plot(FLS)
ssb(FLS)

# stock numbers
bind_rows(
  mySS$natage_annual_2_with_fishery %>% 
    dplyr::select(-1, -2) %>% 
    pivot_longer(names_to = "age", values_to = "data", 2:22) %>% 
    rename(year=Yr) %>% 
    mutate(source="SS", age=as.numeric(age)) %>% 
    mutate(age = ifelse(age>15, 15, age)) %>% 
    group_by(year, age, source) %>% 
    summarise(data = sum(data, na.rm=TRUE)),
  FLCore::stock.n(FLS) %>% 
    as.data.frame() %>% 
    dplyr::select(year, age, data) %>% 
    mutate(source="FLS")
  ) %>% 
  pivot_wider(names_from = source, values_from = data) %>% 
  mutate(diff = abs(SS-FLS)) %>% 
  View()

mySS$wtatage %>% View()
FLCore::stock.wt(FLS)


# quantSums(stock.n(FLS) * stock.wt(FLS) * mat(FLS) *
#             (1 - harvest(FLS) * harvest.spwn(FLS)) *
#             exp(-m(FLS) * m.spwn(FLS)))

bind_rows(
  dplyr::select(mySS$timeseries, year=Yr, ssb=SpawnBio) %>% mutate(source="SS"),
  as.data.frame(FLS) %>% filter(slot=="stock") %>% dplyr::select(year, ssb=data) %>% mutate(source="FLS")
  ) %>% 
  ggplot(aes(x=as.numeric(year), y=ssb)) +
  theme_bw() +
  geom_line(aes(colour=source))


#SS_plots(replist=mySS)

myParBounds <- mySS$parameters

# get names from par file
myParNames <- read.csv(file.path(inpDir, "ss.par"), sep=" ")
parNames   <- clean_names(myParNames)

# and create new variable (to have those matching the names I get when I use read.admbFit)
parBound <- cbind(myParBounds[,c("Value", "Phase", "Min", "Max", "Init")], parNames[parNames!="checksum999"])
# =================================

###################################

# Draw my multivariate normal distribution (more that 1000 repetition because you might have parameters that are outside bounds, and if
# so I will remove the whole line)
# This takes a long time
set.seed(1)

#MVND <- data.table(mvrnorm(n = 100000, mu = mu, Sigma = covMat, empirical = TRUE))
MVND <- data.table(mvrnorm(n = 1000000, mu = mu, Sigma = covMat, empirical = TRUE))

# Assign names
names(MVND) <- getFit$names
# transpose it
newPar <- t(MVND)

# Create new dataframe with everything within bounds
newParSet <- checkBound(bounds=parBound, new_par = newPar)
newParSet <- newParSet[,-1]

# get only 1000
#newParSet <- newParSet[,1:1000]
newParSet <- newParSet[,1:10000]
newParSet <- cbind(newParSet, row.names(newPar)[row.names(newPar) %in%  parBound$parNames]) #GL there was a problem here - I replaced new_par with newPar
names(newParSet)[ncol(newParSet)] <- "par"

#write.csv(newParSet, file=paste(outDir,"Random_parameters_1000it.csv",sep="/"), row.names=F, quote = F)
#write.csv(newParSet, file=paste(outDir,"Random_parameters_1000it.csv",sep="/"), row.names=F, quote = F)
write.csv(newParSet, file=paste(outDir,"Random_parameters_10000it.csv",sep="/"), row.names=F, quote = F)

# Now run SS 
## GL Notes below
# This will take a long time to run and create all the model replicates, taking a lot of disc space

# 1) I'd suggest to hack into this function in the "FUN_Random_population.R" file to extract what is needed and delete, 
# or overwrite, the folders from one replicate to the next

# 2) Make sure to keep the files required to run "SSgetouput" below
# if getcovar is set to FALSE (as it is now) then no need for covar.sso (this will exist only if Hessian is run anyway - see nex comment)
# if getComp is set to FALSE (as it is now) then no need for CompReport.sso 
# I think the forecast file is also only needed if forecast = T
# so probably the report.sso file (and the warning file probably?) are needed. 
# This is to be checked before running all the sims to make sure nothing is missing and have to run everything again..

# 3) Also, as it is set up it will run the Hessian for each replicate, I don't think this is required, is it?
# It would be much faster to run without - to do so, go to "FUN_Random_population.R", go to FUN5 "SS_doPar" and 
# replace the argument extras = "-nox" by extras = "-nox -nohess" - that should do the trick?

# parNames=parNames; parBound=parBound; myParSet=newParSet;n_parSets=2001
# oldsubdir=inpDir; newsubdir = file.path("random_pop", "Set_newParameter"); subdirstart = "newParms"
# overwrite = TRUE; extras = "-nox"; 
# intern = FALSE; CallType = "system"; RemoveBlocks = FALSE

# NOtes by Martin Pastoors
# Since this takes a long time, integrate the fitting of the model with the generation of the FLStock objects and the selection
# of whether the run is to be included into the simulations

iters   <- seq(1,3200); niters <- length(iters)   #10k assessments from the mvrn draw
k.iters <- seq(1,1000); nits   <- length(k.iters)   #number of iters for the simulation

ages <- seq(0,15); nages<-length(ages)   #ages

#folders containing SS output files Report.sso, CompReport.sso and covar.sso
ss.repDirs <- c(file.path(getwd(),Base),
                file.path(getwd(),Base,"random_pop","Set_newParameter",paste0("newParms",iters)))
names(ss.repDirs) <- c("ass",iters)

#assessment years
ass.yrs <- seq(dims(FLS)$minyear,dims(FLS)$maxyear)
nass.yrs <- length(ass.yrs)
tyr <- dims(FLS)$maxyear

# get estiamtes of SSB from final year in the assessment
if (Base=="WGWIDE22") {
  #WGWIDE 2022 (Low-High is 95%)
  WGWIDE.tSSB    <- 693991
  WGWIDE.tSSB.Lo <- 436761
  WGWIDE.tSSB.Hi <- 951221
  WGWIDE.tSSB.SD <- 0.5*(WGWIDE.tSSB.Hi-WGWIDE.tSSB)
}
dfSSB <- data.frame(stringsAsFactors = FALSE)
# FLSs <- FLCore::propagate(FLS,niters)
# for (ii in 1:niters){
#   print(ii)
#   SS_doPar(parNames=parNames, parBound=parBound, myParSet=newParSet, n_parSets=ii)
#   WHOM          <- SS3toFLStock(ss.repDirs[ii],stockname="WHOM",fbar_amin=1,fbar_amax=10)
#   WHOM          <- FLCore::setPlusGroup(WHOM,15)
#   FLSs[,,,,,ii] <- WHOM
#   dfSSB         <- bind_rows(dfSSB, data.frame(iter=ii,SSB=as.numeric(ssb(WHOM))))
# }

save(FLSs, file = file.path(getwd(),Base,"RData",paste0("WHOM_SS",substr(Base,7,8),"_FLSs_3200.RData")))
load(      file = file.path(getwd(),Base,"RData",paste0("WHOM_SS",substr(Base,7,8),"_FLSs_3200.RData")))

dfSSB <- data.frame(iter=1:niters,SSB=as.numeric(ssb(FLSs[,as.character(tyr),,,,,])))

#select from 10k stocks a subset of 1000 with terminal year SSB distribution ensemble statistics matching the assessment
sel.iters <- c()
ntotobs   <- 0

#dist of SSBs
set.seed(1)
bks  <- c(0,seq(100000,1500000,by=100000))
bins <- hist(rnorm(nits,mean=WGWIDE.tSSB,sd=WGWIDE.tSSB.SD),breaks=bks,plot=FALSE)
# bins <- hist(rnorm(1000,mean=WGWIDE.tSSB,sd=WGWIDE.tSSB.SD),breaks=bks,plot=FALSE)

for(bin in seq(1,length(bks)-1)){
  if(bins$counts[bin]>0){
    nobs = length(dfSSB$iter[dfSSB$SSB>bks[bin] & dfSSB$SSB<=bks[bin+1]])
    ntotobs = ntotobs + nobs
    print(paste(bin, 
                nobs,
                ntotobs,
                bins$breaks[bin], 
                bins$counts[bin]))
    sel.iters <- c(sel.iters,sample(dfSSB$iter[dfSSB$SSB>bks[bin] & dfSSB$SSB<=bks[bin+1]],size=min(nobs, bins$counts[bin]),replace=FALSE))
    # sel.iters <- c(sel.iters,sample(dfSSB$iter[dfSSB$SSB>bks[bin] & dfSSB$SSB<=bks[bin+1]],size=bins$counts[bin],replace=FALSE))
  }
}

length(sel.iters)

#select the 1000 from the raw iterations & save
FLSs.1k <- FLSs[,,,,,sel.iters]

#replace first iteration with the assessment output
FLSs.1k[,,,,,1] <- FLS

#Save of 1000 iterations
save(FLSs.1k, file = file.path(getwd(),Base,"RData",paste0("WHOM_SS",substr(Base,7,8),"_FLSs_Clean.RData")))
load(         file = file.path(getwd(),Base,"RData",paste0("WHOM_SS",substr(Base,7,8),"_FLSs_Clean.RData")))

# plot
as.data.frame(ssb(FLSs.1k)) %>%
  ggplot(aes(x=as.numeric(year), y=data, group=iter)) +
  geom_line(data=as.data.frame(ssb(FLSs[,,,,,c(1:2000)[-sel.iters]])), colour="blue") +
  geom_line() +
  geom_line(data=as.data.frame(ssb(FLS)), colour="red")


#check first iter (should match assessment)
# ssb(FLSs.1k[,,,,,1])/ssb(FLS)
# fbar(FLSs.1k[,,,,,1])/fbar(FLS)
# rec(FLSs.1k[,,,,,1])/rec(FLS)




