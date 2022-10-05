# ==============================================================================
# WHM SAM EqSim simulation
# 
# 
# ==============================================================================

rm(list=ls())

gc()

# try(dev.off(),silent=TRUE)
# try(sink(),silent=TRUE)

library(FLCore)
library(Cairo)    #plotting
# library(devEMF)   
library(officer)  #for op to word doc
library(flextable)
library(gplots)   #rich.colors
library(dplyr)
library(tidyr)
library(ggplot2)

# lsdata <- function(fnam = '.Rdata') {
#   x <- load(fnam, envir = environment())
#   return(x)
# }
# lsdata(file.path(RData.dir,"MSE_WGWIDE19_FLStocks_SAM.RData"))

sessionInfo()
#R version 3.5.3 (2019-03-11)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 17763)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_Ireland.1252  LC_CTYPE=English_Ireland.1252    LC_MONETARY=English_Ireland.1252
#[4] LC_NUMERIC=C                     LC_TIME=English_Ireland.1252    

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#[1] ggplot2_3.3.1    tidyr_1.0.2      dplyr_0.8.5      gplots_3.0.3     Cairo_1.5-12     FLCore_2.6.14    iterators_1.0.12
#[8] lattice_0.20-38 

#loaded via a namespace (and not attached):
#[1] Rcpp_1.0.3         pillar_1.4.3       compiler_3.5.3     bitops_1.0-6       tools_3.5.3        packrat_0.5.0     
#[7] lifecycle_0.2.0    tibble_3.0.1       gtable_0.3.0       pkgconfig_2.0.3    rlang_0.4.5        Matrix_1.2-18     
#[13] rstudioapi_0.11    withr_2.1.2        vctrs_0.2.4        gtools_3.8.1       caTools_1.17.1.2   hms_0.5.3         
#[19] stats4_3.5.3       grid_3.5.3         tidyselect_1.0.0   glue_1.4.0         R6_2.4.1           gdata_2.18.0      
#[25] readr_1.3.1        purrr_0.3.3        magrittr_1.5       scales_1.1.0       ellipsis_0.3.0     MASS_7.3-51.5     
#[31] assertthat_0.2.1   colorspace_1.4-1   KernSmooth_2.23-16 munsell_0.5.0      crayon_1.3.4  


#locations
#Drive <- "C:"
Drive <- "D:"

#Base.dir <- file.path(Drive,"Stocks","hom_27_2a4a5b6a7a-ce-k8")
Base.dir <- file.path(Drive,"GIT")

#historic assessments here (not required)
#Assessment.Dir <- file.path(Base.dir,"Assessment")

#moved code to wk_WKREBUILD location 08/06/2020
#MSE.dir <- file.path(Base.dir,"MP_MSE","MSE 2019","whm.MSE2019.WKREBUILD")
#MSE.dir <- file.path(Base.dir,"MP_MSE","wk_WKREBUILD","EqSimWHM")
MSE.dir <- file.path(Base.dir,"wk_WKREBUILD","EqSimWHM")

#this is where the results of the 1000 assessment runs for initialisation of the MSE are saved
Data.dir <- file.path(MSE.dir,"Data")              
#any useful stuff contained in RData files
RData.dir <- file.path(MSE.dir,"RData")
#debug/verbose output
Log.dir <- file.path(MSE.dir,"Logs")              
#Simulation and statistical outputs
Res.dir <- file.path(MSE.dir, "Results")

Source.dir <- file.path(MSE.dir,"R")              #R functions
Scripts.dir <- file.path(MSE.dir, "Scripts")      #R scripts

#OMs, MPs
source(file = file.path(Scripts.dir,"OMs.R"))
source(file = file.path(Scripts.dir,"MPs.R"))

#source functions
sapply(list.files(path=file.path(Source.dir), pattern=".R", full.names=TRUE), source)

#basic simulation settings
#niters <- 10000
niters <- 100
#niters <- 100
#nyr <- 50
nyr <- 20

#base assessment
base <- "sam"

# simulation periods
per1 <- 5
per2 <- 5
# per3 is simply the remainder

#OM <- OM2; MP <- MP2.0_10000
#OM <- OM2; MP <- MP3.0

#OM <- OM2.1   #WGWIDE 2019, const weights, selection
#OM <- OM2.2   #WGWIDE 2019, stochastic weights, selection
OM <- OM2.2sam   #WGWIDE 2019 SAM + ref points, stochastic weights, selection
#MP <- MP1.0   #baseline, constant F harvest rule, no IAV control, no minimum TAC, no assessment/advice error
#MP <- MP1.1   #baseline, constant F harvest rule, no IAV control, 80kt minimum TAC, no assessment/advice error
#MP <- MP1.2   #baseline, constant F harvest rule, no IAV control, 150kt maximum TAC, no assessment/advice error
#MP <- MP1.3   #baseline, constant F harvest rule, no IAV control, 80kt min TAC, 150kt max TAC, no assessment/advice error
#MP <- MP1.4   #baseline, constant F harvest rule, no IAV control, no min/max TAC, includes assessment/advice error
#MP <- MP2.0   #ICES HCR, no IAV control, no minimum TAC, no assessment/advice error
MP <- MP2.1   #ICES HCR, no IAV control, no minimum TAC, with assessment/advice error

runName <- paste(OM$code,MP$code,sep="_")

#assessment FLStock from SS assessment
WHOM.WGWIDE2019 <- get(load(file = file.path(RData.dir,"WGWIDE19_SAM.RData")))

#reduce plus group to 15, to match available data for stock and catch weights
WHOM.WGWIDE2019 <- FLCore::setPlusGroup(WHOM.WGWIDE2019,15)

#Blim <- min(ssb(WHOM.WGWIDE2019))
#The IBP in 2019 selected SSB in 2003 as a proxy for Bpa and derived Blim from this (Bpa/1.4)
#given the lack of any clear SRR and sensitivity of the proportions of mixed models
#to individual data points, a segmented regression with breakpoint at Blim is the default SRR

# Blimloss <- min(OM$refPts$Blim,min(ssb(WHOM.WGWIDE2019)))
Blimloss <- 611814
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= Blimloss, ab$a * Blimloss, ab$a * ssb))

set.seed(1)
#segmented regression with breakpoint at Blim, from 1995 excluding terminal
SRR <- eqsr_fit(window(WHOM.WGWIDE2019,1995,2018), remove.years = c(2018), nsamp=niters, models = c("SegregBlim"))
# eqsr_plot(SRR)

#stock/catch weights (SS3 models catch/stock weights at age as time invariant)
dfSW <- readr::read_delim(file = file.path(Data.dir,"StockWeights.dat"),delim=",")
dfCW <- readr::read_delim(file = file.path(Data.dir,"CatchWeights.dat"),delim=",")

dfWeights <- dplyr::left_join(
  dfSW %>% pivot_longer(cols = paste0("Age",seq(0,15)), names_to = "Age", values_to = "SW", names_prefix = "Age"),
  dfCW %>% pivot_longer(cols = paste0("Age",seq(0,15)), names_to = "Age", values_to = "CW", names_prefix = "Age"),
  by=c("Year","Age"))

dfWeights <- within(dfWeights, Age <- factor(Age, levels = as.character(seq(0,15))))
dfSAWeights = data.frame(Age = seq(0,15), CW = rowMeans(catch.wt(WHOM.WGWIDE2019)), SW = rowMeans(stock.wt(WHOM.WGWIDE2019)), stringsAsFactors = FALSE)
dfSAWeights <- within(dfSAWeights, Age <- factor(Age, levels = as.character(seq(0,15))))

#quick look, comparing with the assessment ouput
gCW <- ggplot(data = dfWeights, mapping = aes(x=Year,y=CW)) +
  geom_line(aes(group=Age)) +
  geom_hline(data = dfSAWeights, mapping=aes(yintercept=CW), col="red") + 
  facet_wrap(~Age) + ylab("Catch Weight")

# emf(file = file.path(res.Dir,"CW.emf"), width = 7, height = 7)
# print(gCW)
# dev.off()

gSW <- ggplot(data = dfWeights, mapping = aes(x=Year,y=SW)) +
  geom_line(aes(group=Age)) +
  geom_hline(data = dfSAWeights, mapping=aes(yintercept=SW), col="red") + 
  facet_wrap(~Age) + ylab("Stock Weight")

# emf(file = file.path(res.Dir,"SW.emf"), width = 7, height = 7)
# print(gSW)
# dev.off()

# read_docx() %>%
#   body_add("Catch Weights") %>%
#   body_add_img(src = file.path(Res.dir,"CW.emf"), width = 7, height = 7) %>% 
#   body_add_break() %>%
#   body_add("Stock Weights") %>%
#   body_add_img(src = file.path(Res.dir,"SW.emf"), width = 7, height = 7) %>% 
#   print(target = file.path(Res.dir,"HistWeights.docx"))
#   
# file.remove(file.path(res.Dir,"CW.emf"))
# file.remove(file.path(res.Dir,"SW.emf"))

#assign the stock and catch weights into the assessment FLStock object
tSW <- FLQuant(t(dfSW[,-1]), dim=dim(stock.wt(WHOM.WGWIDE2019)), dimnames=dimnames(stock.wt(WHOM.WGWIDE2019)))
tCW <- FLQuant(t(dfCW[,-1]), dim=dim(catch.wt(WHOM.WGWIDE2019)), dimnames=dimnames(catch.wt(WHOM.WGWIDE2019)))
stock.wt(WHOM.WGWIDE2019) <- tSW
catch.wt(WHOM.WGWIDE2019) <- tCW

#reassign FLStock object with updated weights into stk slot of SRR 
SRR$stk <- WHOM.WGWIDE2019
rm(flstocks, lWHM)

#initial populations
lWHM <- get(load(file.path(RData.dir,"MSE_WGWIDE19_FLStocks_SAM.RData")))
#load(file=file.path(RData.dir,"MSE_WGWIDE19_FLStocks_15PG.RData"))
#load(file=file.path(RData.dir,"MSE_WGWIDE19_FLStocks_10k.RData"))

#add required number of stochastic FLStocks to FIT object
SRR$stks <- lWHM[as.character(seq(1,niters))]

#start,end,vectors of observation and simulation years
#simulation starts in assessment terminal year
minObsYear <- range(SRR$stk)["minyear"]
maxObsYear <- range(SRR$stk)["maxyear"]
obsYears <- ac(seq(minObsYear,maxObsYear))
yStart <- as.numeric(maxObsYear)
yEnd <- yStart + nyr - 1
simYears <- ac(seq(yStart,yEnd))

#exploration constraints
#2018 catch known, 2019 as assumed during WGWIDE 2019, 2020 as advised
#dfExplConstraints <- data.frame("Type" = c("Catch","Catch","Catch","MinTAC"), 
#                                "YearNum" = c("1","2","3","all"),
#                                "Val" = c(101682,110381,83954,50000), 
#                                stringsAsFactors = FALSE)

dfExplConstraints <- data.frame("Type" = c("Catch","Catch","Catch"), 
                                "YearNum" = c("1","2","3"),
                                "Val" = c(101682,110381,83954), 
                                stringsAsFactors = FALSE)

#min/max TAC
if (!is.na(MP$minTAC)) {
  dfExplConstraints <- dplyr::bind_rows(dfExplConstraints,
                                        data.frame("Type" = "MinTAC",
                                                   "YearNum" = "all",
                                                   "Val" = MP$minTAC,
                                                   stringsAsFactors = FALSE))
}

#min/max TAC
if (!is.na(MP$maxTAC)) {
  dfExplConstraints <- dplyr::bind_rows(dfExplConstraints,
                                        data.frame("Type" = "MaxTAC",
                                                   "YearNum" = "all",
                                                   "Val" = MP$maxTAC,
                                                   stringsAsFactors = FALSE))
}

#IAV
if (!is.na(MP$TAC_IAV)) {
  dfExplConstraints <- dplyr::bind_rows(dfExplConstraints,
                                        data.frame("Type" = "IAV",
                                                   "YearNum" = "all",
                                                   "Val" = MP$TAC_IAV,
                                                   stringsAsFactors = FALSE))
}

#statistical periods for reporting
lStatPer <- lStatPer2 <- list()
#create a list for the output statistical periods
#annual statistics, for each historic and simulated year
for (y in seq(minObsYear,yEnd)){lStatPer[[ac(y)]]<-c(y,y)}
for (y in seq(maxObsYear+1,yEnd)){lStatPer2[[ac(y)]]<-c(y,y)}

#Short (first 5 after constraints), Medium (next 5) and Long Term (next 20)
yConstraints <- 3
lStatPer[['ST']] <- c(yStart+yConstraints,yStart+yConstraints+(per1-1))
lStatPer2[['ST']] <- c(yStart+yConstraints+1,yStart+yConstraints+(per1-1))
lStatPer[['MT']] <- lStatPer2[['MT']] <- c(yStart+yConstraints+per1,yStart+yConstraints+(per1+per2-1))
lStatPer[['LT']] <- lStatPer2[['LT']] <- c(yStart+yConstraints+per1+per2,yStart+nyr-1)

sim <- eqsim_run(fit = SRR, 
                 bio.years = OM$BioYrs, bio.const = OM$BioConst,
                 sel.years = OM$SelYrs, sel.const = OM$SelConst,
                 Fscan = fGetValsScan(MP$F_target,OM$refPts), 
                 Fcv = MP$Obs$cvF, Fphi = MP$Obs$phiF,
                 SSBcv = MP$Obs$cvSSB, SSBphi = MP$Obs$phiSSB,
                 Nrun = nyr, calc.RPs = FALSE,
                 dfExplConstraints = dfExplConstraints, 
                 Btrigger = fGetValsScan(MP$B_trigger,OM$refPts),
                 HCRName = paste0("fHCR_",MP$HCRName))

SimRuns <- sim$simStks
  
#save ouptut
#create a folder for the output
dir.create(path = file.path(MSE.dir,"Results",runName), showWarnings = TRUE, recursive = TRUE)
#dir.create(path = file.path(getwd(),"Results",runName), showWarnings = TRUE, recursive = TRUE)
#and write the output (necessary to save entire image?)
save.image(file = file.path(MSE.dir,"Results",runName,paste0(runName,"_eqSim_Workspace.Rdata")))
#save.image(file = file.path(getwd(),"Results",runName,paste0(runName,"_eqSim_Workspace.Rdata")))
save(SimRuns,file = file.path(MSE.dir,"Results",runName,paste0(runName,"_SimRuns.RData")))
#save(SimRuns,file = file.path(getwd(),"Results",runName,paste0(runName,"_SimRuns.RData")))
  
#Percentiles to report, number of worm lines for plots
percentiles = c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975)
numWorm <- 20
  
#Create list objects to store stocks, stats
Stocks <- AllStats <- list()
#create a template by extending the time frame out to the final year of the simulation
stockTemplate <- window(SRR$stk, end=yEnd)
  
#set the maturity, natural mortality and f/m proportions
mat(stockTemplate)[,simYears] <- mat(stockTemplate)[,ac(yStart-1)]
m(stockTemplate)[,simYears] <- m(stockTemplate)[,ac(yStart-1)]
m.spwn(stockTemplate)[,simYears] <- m.spwn(stockTemplate)[,ac(yStart-1)]
harvest.spwn(stockTemplate)[,simYears] <- harvest.spwn(stockTemplate)[,ac(yStart-1)]
#quick look
plot(stockTemplate)
  
#extend object to store neessary number of iterations
stockTemplate <- FLCore::propagate(stockTemplate,niters)

#populate each stock object
#year dimension is trimmed to the actual simulation period (may be longer depending on HCR implemented)
#ii <- "0.074"
for (ii in names(SimRuns)) {
  
  cat("Calculating statistics for run with f =",ii,"\n")
  
  Stocks[[ii]] <- stockTemplate
  stock.n(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$N[,1:(yEnd-yStart+1),]
  harvest(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$F[,1:(yEnd-yStart+1),]
  catch.n(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$C[,1:(yEnd-yStart+1),]
  catch.wt(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$catW[,1:(yEnd-yStart+1),]
  #landings.n(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$L[,1:(yEnd-yStart+1),]
  landings.n(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$C[,1:(yEnd-yStart+1),]
  stock.wt(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$stkW[,1:(yEnd-yStart+1),]
  mat(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$Mat[,1:(yEnd-yStart+1),]
  discards.wt(Stocks[[ii]]) <- catch.wt(Stocks[[ii]])
  landings.wt(Stocks[[ii]]) <- catch.wt(Stocks[[ii]])
  discards.n(Stocks[[ii]])[,simYears,,,,] <- 0
  catch(Stocks[[ii]])[,simYears] <- apply(catch.n(Stocks[[ii]])[,simYears]*catch.wt(Stocks[[ii]])[,simYears],2:6,sum)
  landings(Stocks[[ii]])[,simYears] <- apply(landings.n(Stocks[[ii]])[,simYears]*landings.wt(Stocks[[ii]])[,simYears],2:6,sum)
  discards(Stocks[[ii]])[,simYears] <- apply(discards.n(Stocks[[ii]])[,simYears]*discards.wt(Stocks[[ii]])[,simYears],2:6,sum)
  
  #statistics
  Stats <- list()
    
  #store the operating model and harvest rules
  Stats[["OM"]] <- OM
  Stats[["MP"]] <- MP
  Stats[["simYears"]] <- as.integer(simYears)
  Stats[["obsYears"]] <- as.integer(obsYears)
  
  #SSB
  SSB.true <- ssb(Stocks[[ii]])
  Stats[["SSB"]][["val"]] <- fStatPercs(SSB.true, lStatPer=lStatPer)
  Stats[["SSB"]][["worm"]] <- FLCore::iter(SSB.true,1:numWorm)
  
  #SSB error
  tSSB <- FLQuant(SimRuns[[ii]]$SSBratio[1:(yEnd-yStart+1),],
                  dim = c(1,yEnd-yStart+1,1,1,1,niters),
                  dimnames = list(age="all",year=ac(seq(yStart,yEnd)),unit="unique",season="all",
                                  area="unique",iter=ac(seq(1,niters))))
    
  Stats[["SSBratio"]][["val"]] <- fStatPercs(tSSB, lStatPer = lStatPer)
  Stats[["SSBratio"]][["worm"]] <- FLCore::iter(tSSB,1:numWorm)
    
  SSB.dev <- SimRuns[[ii]][["SSBdev"]]
  Stats[["SSB.dev"]] <- SSB.dev
    
  #FBar - realised F
  FBar <- fbar(Stocks[[ii]])
  Stats[["FBar"]][["val"]] <- fStatPercs(FBar, lStatPer=lStatPer)
  Stats[["FBar"]][["worm"]] <- FLCore::iter(FBar,1:numWorm)
  
  #FBar error
  tFBar <- FLQuant(SimRuns[[ii]]$Fratio[1:(yEnd-yStart+1),],
                  dim = c(1,yEnd-yStart+1,1,1,1,niters),
                  dimnames = list(age="all",year=ac(seq(yStart,yEnd)),unit="unique",season="all",
                                  area="unique",iter=ac(seq(1,niters))))
  
  Stats[["Fratio"]][["val"]] <- fStatPercs(tFBar, lStatPer = lStatPer)
  Stats[["Fratio"]][["worm"]] <- FLCore::iter(tFBar,1:numWorm)
  
  Fdev <- SimRuns[[ii]][["Fdev"]]
  Stats[["Fdev"]] <- Fdev
  
  #yield
  Catch <- catch(Stocks[[ii]])
  Stats[["Catch"]][["val"]] <- fStatPercs(Catch, lStatPer=lStatPer)
  Stats[["Catch"]][["worm"]] <- FLCore::iter(Catch,1:numWorm)
  
  #TAC
  tTAC <- FLQuant(SimRuns[[ii]]$TAC[1:(yEnd-yStart+1),],
                  dim = c(1,yEnd-yStart+1,1,1,1,niters),
                  dimnames = list(age="all",year=ac(seq(yStart,yEnd)),unit="unique",season="all",
                                  area="unique",iter=ac(seq(1,niters))))
    
  Stats[["TAC"]][["val"]] <- fStatPercs(tTAC, lStatPer = lStatPer)
  Stats[["TAC"]][["worm"]] <- FLCore::iter(tTAC,1:numWorm)
    
  #IAV
  # IAV <- FLQuant(apply(SimRuns[[ii]]$TAC,MARGIN=2,FUN = function(x){(x-dplyr::lag(x))/x}),
  #                 dim = c(1,yEnd-yStart+1,1,1,1,niters),
  #                 dimnames = list(age="all",year=ac(seq(yStart,yEnd)),unit="unique",season="all",
  #                                 area="unique",iter=ac(seq(1,niters))))

  IAV <- FLQuant(apply(SimRuns[[ii]]$TAC,MARGIN=2,FUN = function(x){(x-dplyr::lag(x))}),
                 dim = c(1,yEnd-yStart+1,1,1,1,niters),
                 dimnames = list(age="all",year=ac(seq(yStart,yEnd)),unit="unique",season="all",
                                 area="unique",iter=ac(seq(1,niters))))
  
  Stats[["IAV"]][["val"]] <- fStatPercs(IAV, lStatPer = lStatPer2)
  Stats[["IAV"]][["worm"]] <- FLCore::iter(IAV,1:numWorm)
    
  #Recruitment
  Rec <- rec(Stocks[[ii]])
  Stats[["Rec"]][["val"]] <- fStatPercs(Rec, lStatPer=lStatPer)
  Stats[["Rec"]][["worm"]] <- FLCore::iter(Rec,1:numWorm)
  
  #probability that SSB is below RP (should this be true or observed SSB?)
  Stats[["pBlim"]][["val"]] <- fStatRisk(SSB = SSB.true, RP = OM$refPts$Blim, lStatPer = lStatPer)
  Stats[["pBpa"]][["val"]] <- fStatRisk(SSB = SSB.true, RP = OM$refPts$Bpa, lStatPer = lStatPer)
  Stats[["pExt"]][["val"]] <- fStatExtinct(SSB = SSB.true, depletion=0.01, firstYear = maxObsYear)
    
  AllStats[[ac(ii)]] <- Stats
    
}

## Save data
lOp <- list(stats = AllStats, runName = runName, lStatPer = lStatPer, OM = OM, MP = MP)
save(lOp,file = file.path(Res.dir,runName,paste0(runName,"_eqSim_Stats.Rdata")))
#save(lOp,file = file.path(getwd(),"Results",runName,paste0(runName,"_eqSim_Stats.Rdata")))

#generate the stock/stat trajectories
fPlotTraj(sim = lOp, plot.dir = Res.dir, lStatPer = lStatPer)
fPlotSummary(sim = lOp, plot.dir = Res.dir, lStatPer = lStatPer)
fTabulateStats(sim = lOp, plot.dir = Res.dir)

# #SSB vs Blim
# fAnnSSBvsBlimDist(OM = OM2, MP = MP2.0, res.dir = Res.dir, plot.dir = Res.dir)
# 

#comparison of stochastic/random weights & selection - min, max, min & max TAC
#runs2Compare <- c("OM2.1_MP1.0","OM2.2_MP1.0","OM2.2_MP1.1","OM2.2_MP1.2","OM2.2_MP1.3")
#for (stat in c("Catch","SSB","Risk3","Risk1")){
#fCompare_runs(runs2Compare = runs2Compare, Res.dir = Res.dir, Plot.dir = Res.dir,
#              PerfStat = stat, TargetFs = c(0,0.05,0.074,0.1,0.108,0.2),
#              simYears = simYears, lStatPer = lStatPer, Blim = OM$refPts$Blim)}

#inclusion of assessment and advice error (stochastoc weights/selection)
# runs2Compare <- c("OM2.2_MP1.0","OM2.2_MP1.4")
# for (stat in c("Catch","SSB","Risk3","Risk1")){
#   fCompare_runs(runs2Compare = runs2Compare, Res.dir = Res.dir, Plot.dir = Res.dir,
#                 PerfStat = stat, TargetFs = c(0,0.05,0.074,0.1,0.108,0.2),
#                 simYears = simYears, lStatPer = lStatPer, Blim = OM$refPts$Blim)}

#comparison of stochastic/random weights & selection - min, max, min & max TAC
#runs2Compare <- c("OM2.2_sam_MP2.1","OM2.2_MP2.1")
#debug("fCompare_runs")
#for (stat in c("Catch","SSB","Risk3","Risk1")){
# for (stat in c("SSB", "Risk3")){
#     fCompare_runs(runs2Compare = runs2Compare, 
#                   Res.dir = Res.dir, 
#                   Plot.dir = Res.dir,
#                   PerfStat = stat, 
#                   TargetFs = c(0, 0.05, 0.074, 0.1, 0.108, 0.2, 0.3),
#                   # simYears = simYears, 
#                   lStatPer = lStatPer, 
#                   Blim = OM$refPts$Blim)}
#undebug("fCompare_runs")

save.image(file = file.path(MSE.dir,"Results",runName,paste0(runName,"_eqSim_Workspace.Rdata")))

