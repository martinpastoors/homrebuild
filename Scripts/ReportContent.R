#Report plots


#code to generate plots for the evaluation report
rm(list=ls())
gc()

library(tidyverse)
library(FLCore)

source("EqSimWHM/Scripts/01_EqSim_setup.R")

#computer specific locations
#Drive    <- "D:"
#Base.dir <- file.path(Drive,"GIT")
# Drive    <- "C:"
# Base.dir <- file.path(Drive,"Stocks","hom_27_2a4a5b6a7a-ce-k8","MP_MSE")
# MSE.dir <- file.path(Base.dir,"wk_WKREBUILD","EqSimWHM")
# Data.dir <- file.path(MSE.dir,"Data")   
# RData.dir <- file.path(MSE.dir,"RData")
# Res.dir <- file.path(MSE.dir, "Results")
# stats.dir <- file.path(Res.dir, "Stats")
# Scripts.dir <- file.path(MSE.dir, "Scripts")
# Source.dir <- file.path(MSE.dir,"R")

# source(file.path(MSE.dir,"R","utilities.R"))
# source(file = file.path(Scripts.dir,"OMs.R"))
# sapply(list.files(path=file.path(Source.dir), pattern=".R", full.names=TRUE), source)

Rep.dir <- file.path(MSE.dir,"ReportGraphics")

ages <- seq(0,15)
nAges <- length(ages)
iters <- seq(1,1000)
nits <- 1000

##############################DATA################################################################

#historic data (assessments)

# WG17 <- loadRData(file.path(RData.dir,"WGWIDE17.RData")) %>% FLCore::setPlusGroup(., 15)
# name(WG17) <- "Western Horse Mackerel, WGWIDE17 SS Assessment"

# WG18 <- loadRData(file.path(RData.dir,"WGWIDE18.RData")) %>% FLCore::setPlusGroup(., 15)
# name(WG18) <- "Western Horse Mackerel, WGWIDE18 SS Assessment"

WG19 <- loadRData(file.path(RData.dir,"WGWIDE19.RData")) %>% FLCore::setPlusGroup(., 15)
name(WG19) <- "Western Horse Mackerel, WGWIDE19 SS Assessment"

WG20 <- loadRData(file.path(RData.dir,"WGWIDE20.RData")) %>% FLCore::setPlusGroup(., 15)
name(WG20) <- "Western Horse Mackerel, WGWIDE20 SS Assessment"

SAM19 <- loadRData(file.path(RData.dir,"WHOM_SAM19_FLS_WGWIDE.RData")) %>% FLCore::setPlusGroup(., 15)
name(WG19) <- "Western Horse Mackerel, WGWIDE19 SAM Assessment"

SAM20 <- loadRData(file.path(RData.dir,"WHOM_SAM20_FLS_WGWIDE.RData")) %>% FLCore::setPlusGroup(., 15)
name(WG20) <- "Western Horse Mackerel, WGWIDE20 SAM Assessment"

yrs_WG19  <- seq(dims(WG19)$minyear,dims(WG19)$maxyear)
nYrs_WG19 <- length(yrs_WG19)

yrs_WG20  <- seq(dims(WG20)$minyear,dims(WG20)$maxyear)
nYrs_WG20 <- length(yrs_WG20)

yrs_SAM19  <- seq(dims(SAM19)$minyear,dims(SAM19)$maxyear)
nYrs_SAM19 <- length(yrs_SAM19)

yrs_SAM20  <- seq(dims(SAM20)$minyear,dims(SAM20)$maxyear)
nYrs_SAM20 <- length(yrs_SAM20)

#1000 iterations
FLSs_WG19 <- loadRData(file=file.path(RData.dir,"WHOM_SS19_FLS_V2.RData"))
FLSs_WG20 <- loadRData(file=file.path(RData.dir,"WHOM_SS20_FLS_V2.RData"))

FLSs_SAM19 <- loadRData(file=file.path(RData.dir,"WHOM_SAM19_FLS_converged.RData"))
FLSs_SAM20 <- loadRData(file=file.path(RData.dir,"WHOM_SAM20_FLS_converged.RData"))

#alternative recruitment scenarios
FLSs_RRV5 <- loadRData(file=file.path(RData.dir,"WHOM_SS19_FLS_V5.RData"))  #GM 02-13
FLSs_RRV6 <- loadRData(file=file.path(RData.dir,"WHOM_SS19_FLS_V6.RData"))  #Baseline//2
FLSs_RRV7 <- loadRData(file=file.path(RData.dir,"WHOM_SS19_FLS_V7.RData"))  #Lowest 5 Mean

#starting SSBs
# dfInitSSB <- data.frame(Scenario=rep(c("Baseline","GM 02-14","Baseline/2","Mn 5Low"),each=nits),
#                         SSB=c(as.numeric(ssb(FLSs_WG19[,'2018'])),as.numeric(ssb(FLSs_RRV5[,'2018'])),
#                               as.numeric(ssb(FLSs_RRV6[,'2018'])),as.numeric(ssb(FLSs_RRV7[,'2018']))),
#                         iter=rep(seq(1,nits),4))

# gInitSSB <- ggplot(data = dfInitSSB, mapping = aes(x=Scenario,y=SSB/1e6)) + geom_boxplot() + 
#   geom_hline(yintercept=834480/1e6, col="red") + scale_x_discrete(limits=c("Baseline","Baseline/2","GM 02-14","Mn 5Low")) +
#   ylab("Initial SSB(Mt)")

# SSBs for SS and SAM; different years. 
dfInitSSB <- 
  data.frame(Scenario=rep(c("SS19","SS20","SAM19","SAM20"),each=nits),
             Assess  =rep(c("SS","SS","SAM","SAM"),each=nits),
             SSB=c(as.numeric(ssb(FLSs_WG19[,'2018'])),
                   as.numeric(ssb(FLSs_WG20[,'2018'])),
                   as.numeric(ssb(FLSs_SAM19[,'2018'])),
                   as.numeric(ssb(FLSs_SAM20[,'2018']))),
             iter=rep(seq(1,nits),4)) %>% 
  mutate(blim = ifelse(grepl("SS",Scenario), 834480/1e6, 611814/1e6))

# gInitSSB <- 
  ggplot(data = dfInitSSB, aes(x=Scenario,y=SSB/1e6)) + 
    geom_boxplot() + 
    geom_hline(aes(yintercept=blim), col="red") + 
    ylab("2018 SSB (Mt)") +
    expand_limits(y=0) +
    facet_wrap(~Assess, scales = "free_x")
  

png(filename = file.path(Rep.dir,"InitSSB_Comparisons.png"),width = 400, height = 600)
print(gInitSSB)
dev.off()

#maturity
dfMat <- data.frame(Age=ages,Mat=as.numeric(FLCore::mat(WG19)[,'2018']))
gMat <- ggplot(data = select(dfMat,Age,Maturity=Mat), mapping = aes(x=Age,y=Maturity)) + geom_line(lwd=1) + ylab("Proportion Mature")
png(filename = file.path(Rep.dir,"WGWIDE19_Maturity.png"),width = 600, height = 600)
print(gMat)
dev.off()


#weight at age data
dfWeights <- data.frame(WG=c(),Year=c(),Age=c(),Var=c(),Wgt=c())

for (ass in c("WGWIDE19","WGWIDE20")){
  dfWeights <- dplyr::bind_rows(dfWeights,
  readr::read_delim(file = file.path(getwd(),"..","MSEInitialisation",ass,"StockWeights.dat"),delim=",") %>%
    pivot_longer(cols = paste0("Age",seq(0,15)), names_to = "Age", values_to = "Wgt", names_prefix = "Age") %>%
    mutate(WG = ass, Var="SW") %>%
    select(WG,Year,Age,Var,Wgt))
  dfWeights <- dplyr::bind_rows(dfWeights,
    readr::read_delim(file = file.path(getwd(),"..","MSEInitialisation",ass,"CatchWeights.dat"),delim=",") %>%
    pivot_longer(cols = paste0("Age",seq(0,15)), names_to = "Age", values_to = "Wgt", names_prefix = "Age") %>%
    mutate(WG = ass, Var="CW") %>%
    select(WG,Year,Age,Var,Wgt))
}
dfWeights <- within(dfWeights, Age <- factor(Age, levels = ac(ages)))

#assessment weights at age (from the 1000 replicates)
dfSAWeights <- data.frame(WG=c(),Iter=c(),Age=c(),Var=c(),Wgt=c())

for (i in seq(1,nits)){
  SW <- as.numeric(FLCore::stock.wt(FLSs_WG19[,1,,,,i]))
  CW <- as.numeric(FLCore::catch.wt(FLSs_WG19[,1,,,,i]))
  dfSAWeights <- dplyr::bind_rows(dfSAWeights,data.frame(WG="WGWIDE19",Iter=i,Age=seq(0,15),Var="SW",Wgt=SW))
  dfSAWeights <- dplyr::bind_rows(dfSAWeights,data.frame(WG="WGWIDE19",Iter=i,Age=seq(0,15),Var="CW",Wgt=CW))
  SW <- as.numeric(FLCore::stock.wt(FLSs_WG20[,1,,,,i]))
  CW <- as.numeric(FLCore::catch.wt(FLSs_WG20[,1,,,,i]))
  dfSAWeights <- dplyr::bind_rows(dfSAWeights,data.frame(WG="WGWIDE20",Iter=i,Age=seq(0,15),Var="SW",Wgt=SW))
  dfSAWeights <- dplyr::bind_rows(dfSAWeights,data.frame(WG="WGWIDE20",Iter=i,Age=seq(0,15),Var="CW",Wgt=CW))
}
dfSAWeights <- within(dfSAWeights, Age <- factor(Age, levels = ac(ages)))

#assessment selection
dfSAOp  <- data.frame(WG=c(),Iter=c(),Age=c(),Year=c(),Var=c(),Val=c())
for (i in seq(1,nits)){
  dfSAOp <- dplyr::bind_rows(dfSAOp,
                               data.frame(WG="WGWIDE19",Iter=i,Age=ages,Year=rep(yrs_WG19,each=nAges),Var="Sel",
                                          Val=as.numeric(sweep(matrix(harvest(FLSs_WG19[,,,,,i]),ncol=nYrs_WG19),2,matrix(harvest(FLSs_WG19['15',,,,,i]),ncol=nYrs_WG19),"/"))))
  dfSAOp <- dplyr::bind_rows(dfSAOp,
                               data.frame(WG="WGWIDE20",Iter=i,Age=ages,Year=rep(yrs_WG20,each=nAges),Var="Sel",
                                          Val=as.numeric(sweep(matrix(harvest(FLSs_WG20[,,,,,i]),ncol=nYrs_WG20),2,matrix(harvest(FLSs_WG20['15',,,,,i]),ncol=nYrs_WG20),"/"))))
}
dfSAOp <- within(dfSAOp, fAge <- factor(Age, levels = ac(ages)))

#generate some future weights and selections (mimicking EqSim)
ass.yrs <- seq(dims(FLSs_WG19)$minyear,dims(FLSs_WG19)$maxyear)
nass.yrs <- length(ass.yrs)
tyr <- dims(FLSs_WG19)$maxyear
Nrun <- 23
Nmod <- nits
#historic years
histYears <- seq(tyr-10,tyr-1)
nhistYears <- length(histYears)
#future years
fYears <- seq(tyr,tyr+Nrun-1)
nfYears <- length(fYears)

#iteration stock/catch weights from last 10 years
iSW <- array(as.numeric(stock.wt(FLSs_WG19[,ac(histYears),,,,])),dim=c(nAges,nhistYears,Nmod), 
             dimnames=list(Age = ages,Year = histYears, Iter = seq(1,Nmod)))
iCW <- array(as.numeric(catch.wt(FLSs_WG19[,ac(histYears),,,,])),dim=c(nAges,nhistYears,Nmod), 
             dimnames=list(Age = ages,Year = histYears, Iter = seq(1,Nmod)))

#random sample from last 10 (same for each iteration)
rsam <- sample(histYears,Nrun,replace=TRUE)

#future weights/selections
ifSW <- ifCW <- ifSel <- array(NA,dim = c(nAges,nfYears,Nmod), dimnames = list(Age = ages,Year = fYears, Iter = seq(1,Nmod)))
for(iii in seq(1,Nmod)){
  ifSW[,,iii] <- iSW[,c(as.character(rsam)),iii]
  ifCW[,,iii] <- iCW[,c(as.character(rsam)),iii]
}

#random sample for selections
rsamsel = sample(seq(1,1000),Nrun*Nmod,replace=TRUE)

#really slow - recode this!
t <- filter(dfSAOp,WG=="WGWIDE19" & Var=="Sel" & Year==max(yrs_WG19)) %>% select(Iter,Age,Val)
c <- 0
for (iii in seq(1,nits)){
  for (yy in seq(1,nfYears)){
    c <- c + 1
    ifSel[,yy,iii] <- t$Val[t$Iter==rsamsel[c]]
  }
}


dfSimWeights <- dplyr::bind_rows(
  data.frame(Age=rep(ages,nfYears), Year=rep(fYears,each=nAges), Var="SW",
             Lo=rep(NA,nAges*nfYears),Md=rep(NA,nAges*nfYears),Hi=rep(NA,nAges*nfYears)),
  data.frame(Age=rep(ages,nfYears), Year=rep(fYears,each=nAges), Var="CW",
             Lo=rep(NA,nAges*nfYears),Md=rep(NA,nAges*nfYears),Hi=rep(NA,nAges*nfYears)),
  data.frame(Age=rep(ages,nfYears), Year=rep(fYears,each=nAges), Var="Sel",
             Lo=rep(NA,nAges*nfYears),Md=rep(NA,nAges*nfYears),Hi=rep(NA,nAges*nfYears)))

for(aa in ages){
  for (y in fYears){
    cond1 <- dfSimWeights$Age==aa & dfSimWeights$Year==y
    cond2 <- dfSimWeights$Var=="SW"
    dfSimWeights$Lo[cond1 & cond2] <- quantile(ifSW[as.character(aa),as.character(y),],0.05)
    dfSimWeights$Md[cond1 & cond2] <- quantile(ifSW[as.character(aa),as.character(y),],0.5)
    dfSimWeights$Hi[cond1 & cond2] <- quantile(ifSW[as.character(aa),as.character(y),],0.95)
    cond2 <- dfSimWeights$Var=="CW"
    dfSimWeights$Lo[cond1 & cond2] <- quantile(ifCW[as.character(aa),as.character(y),],0.05)
    dfSimWeights$Md[cond1 & cond2] <- quantile(ifCW[as.character(aa),as.character(y),],0.5)
    dfSimWeights$Hi[cond1 & cond2] <- quantile(ifCW[as.character(aa),as.character(y),],0.95)
    cond2 <- dfSimWeights$Var=="Sel"
    dfSimWeights$Lo[cond1 & cond2] <- quantile(ifSel[as.character(aa),as.character(y),],0.05)
    dfSimWeights$Md[cond1 & cond2] <- quantile(ifSel[as.character(aa),as.character(y),],0.5)
    dfSimWeights$Hi[cond1 & cond2] <- quantile(ifSel[as.character(aa),as.character(y),],0.95)
  }
}

dfSimWeights <- within(dfSimWeights, fAge <- factor(Age, levels = ac(ages)))

##############################PLOTS################################################################

#assessment summary plots
png(filename = file.path(Rep.dir,"WGWIDE19_Final_SS_Assessment_Summary.png"),
    width = 600, height = 600)
plot(WG19)
dev.off()
png(filename = file.path(Rep.dir,"WGWIDE20_Final_SS_Assessment_Summary.png"),
    width = 600, height = 600)
plot(WG20)
dev.off()


#construct a data frame with the historical assessment outputs
dfWHMAssess <- data.frame(WG = c(), Mod = c(), Yr = c(), SSB = c(), FBar = c(), Rec = c())
dfWHMAssess <- dplyr::bind_rows(dfWHMAssess,data.frame(WG = rep("WG17",dim(WG17)[2]),Mod = rep("SS3",dim(WG17)[2]),
                                                       Yr = seq(range(WG17)["minyear"],range(WG17)["maxyear"]), SSB = as.numeric(FLCore::ssb(WG17)), 
                                                       FBar = as.numeric(fbar(WG17)), Rec = as.numeric(rec(WG17))))
dfWHMAssess <- dplyr::bind_rows(dfWHMAssess,data.frame(WG = rep("WG18",dim(WG18)[2]),Mod = rep("SS3",dim(WG18)[2]),
                                                       Yr = seq(range(WG18)["minyear"],range(WG18)["maxyear"]), SSB = as.numeric(FLCore::ssb(WG18)), 
                                                       FBar = as.numeric(fbar(WG18)), Rec = as.numeric(rec(WG18))))
dfWHMAssess <- dplyr::bind_rows(dfWHMAssess,data.frame(WG = rep("WG19",dim(WG19)[2]),Mod = rep("SS3",dim(WG19)[2]),
                                                       Yr = seq(range(WG19)["minyear"],range(WG19)["maxyear"]), SSB = as.numeric(FLCore::ssb(WG19)), 
                                                       FBar = as.numeric(fbar(WG19)), Rec = as.numeric(rec(WG19))))
dfWHMAssess <- dplyr::bind_rows(dfWHMAssess,data.frame(WG = rep("WG20",dim(WG20)[2]),Mod = rep("SS3",dim(WG20)[2]),
                                                       Yr = seq(range(WG20)["minyear"],range(WG20)["maxyear"]), SSB = as.numeric(FLCore::ssb(WG20)), 
                                                       FBar = as.numeric(fbar(WG20)), Rec = as.numeric(rec(WG20))))
table(dfWHMAssess$WG)

#recruitment plot
png(filename = file.path(Rep.dir,"WGWIDE19_Recr_TimeSeries.png"),width = 600, height = 600)
plot(dfWHMAssess$Yr[dfWHMAssess$WG=="WG19"],dfWHMAssess$Rec[dfWHMAssess$WG=="WG19"]/1e6,type="l",
     xlab="Year",ylab="Recruits (billions)",axes=F,ylim=c(0,60),xlim=c(1980,2020))
points(dfWHMAssess$Yr[dfWHMAssess$WG=="WG19"],dfWHMAssess$Rec[dfWHMAssess$WG=="WG19"]/1e6,pch=19)
axis(1)
axis(2)
dev.off()

png(filename = file.path(Rep.dir,"WGWIDE19_SSBRecr_Scatter.png"),width = 600, height = 600)
plot(dfWHMAssess$SSB[dfWHMAssess$WG=="WG19"]/1e6,dfWHMAssess$Rec[dfWHMAssess$WG=="WG19"]/1e6,type="p",xlim=c(0,6),
     ylim=c(0,60),xlab="SSB (Mt)",ylab="Recruits (billions)",pch=19,axes=F)
axis(1)
axis(2)
dev.off()


#some plot limits
x.mult <- 1.1
y.mult <- 1.4
minSSB <- min(dfWHMAssess$SSB, max(dfWHMAssess$SSB) * 0.0125)
maxSSB <- max(dfWHMAssess$SSB) * x.mult
maxrec <- max(dfWHMAssess$SSB) * y.mult

ssb_eval <- seq(minSSB, maxSSB, length.out = 100)
n_mods <- 2000

sample_rec <- function(i) {
  FUN <-  match.fun(modset$model[i])
  exp(FUN(modset[i,], ssb_eval) + stats::rnorm(length(ssb_eval), sd = modset $ cv[i]) )
}


OM_WGWIDE19 <- OM2.2

#SRR fit for WGWIDE 2019
Blimloss <- OM_WGWIDE19$refPts$Blim
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= Blimloss, ab$a * Blimloss, ab$a * ssb))
set.seed(1)
SRR_WGWIDE19 <- eqsr_fit(window(WG19,1995,2018), remove.years = c(2018), nsamp=1000, models = c("SegregBlim"))

png(filename = file.path(Rep.dir,"EqSim_SRR_SS_WGWIDE19.png"), width = 600, height = 400)
eqsr_plot(SRR_WGWIDE19)
dev.off()

#ecdf - code to generate draws borrowed from eqsr
modset <- SRR_WGWIDE19$sr.sto
ids <- sample(1:nrow(modset), n_mods, replace = TRUE)
rec_sim <- sapply(ids, sample_rec)

out <- data.frame(grp = rep(1:length(ssb_eval), n_mods),
                  mid.grp = rep(ssb_eval, n_mods),
                  ssb = jitter(rep(ssb_eval, n_mods), 2), # jitter for nices plotting
                  rec = c(rec_sim),
                  model = rep(modset[ids,"model"], each = length(ssb_eval)))

#model vs obs
ggRec_WG19 <- ggplot(data = out, mapping = aes(x=rec)) +
  stat_ecdf(geom = "line", pad=FALSE) + xlim(0,1.5e7) +
  stat_ecdf(data = filter(dfWHMAssess,WG=="WG19" & Yr>=1995 & Yr<=2018), 
            mapping = aes(x=Rec), geom="point", pad=FALSE) +
  xlab("Recruits (000's)") + ylab("Cumulative Dist") + ggtitle("ECDF Recruitment, SS WGWIDE 19 (1995-2017)")

png(filename = file.path(Rep.dir,"ECDF_SS_WGWIDE19.png"), width = 600, height = 400)
print(ggRec_WG19)
dev.off()


#WGWIDE 2020
OM_WGWIDE20 <- OM2.3

Blimloss <- OM_WGWIDE20$refPts$Blim
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= Blimloss, ab$a * Blimloss, ab$a * ssb))
set.seed(1)
SRR_WGWIDE20 <- eqsr_fit(window(WG20,1995,2019), remove.years = c(2019), nsamp=1000, models = c("SegregBlim"))

png(filename = file.path(Rep.dir,"EqSim_SRR_SS_WGWIDE20.png"), width = 600, height = 400)
eqsr_plot(SRR_WGWIDE20)
dev.off()


#ecdf - code to generate draws borrowed from eqsr
modset <- SRR_WGWIDE20$sr.sto
ids <- sample(1:nrow(modset), n_mods, replace = TRUE)
rec_sim <- sapply(ids, sample_rec)

out <- data.frame(grp = rep(1:length(ssb_eval), n_mods),
                  mid.grp = rep(ssb_eval, n_mods),
                  ssb = jitter(rep(ssb_eval, n_mods), 2), # jitter for nices plotting
                  rec = c(rec_sim),
                  model = rep(modset[ids,"model"], each = length(ssb_eval)))

#model vs obs
ggRec_WG20 <- ggplot(data = out, mapping = aes(x=rec)) +
  stat_ecdf(geom = "line", pad=FALSE) + xlim(0,1.5e7) +
  stat_ecdf(data = filter(dfWHMAssess,WG=="WG20" & Yr>=1995 & Yr<=2019), 
            mapping = aes(x=Rec), geom="point", pad=FALSE) +
  xlab("Recruits") + ylab("Cumulative Dist") + ggtitle("ECDF Recruitment, SS WGWIDE20 (1995-2018)")

png(filename = file.path(Rep.dir,"ECDF_SS_WGWIDE20.png"), width = 600, height = 400)
print(ggRec_WG20)
dev.off()

ggHistRec <- ggplot(data = filter(dfWHMAssess, Yr>=1985) %>% select(Year=Yr,Assessment=WG,Rec), 
                    mapping = aes(x=Year, y=Rec/1e6, group = Assessment, col=Assessment)) + 
  geom_line(lwd=1) + ylab("Recruits (billions)") + xlab("Year") + theme(legend.position = "top") +
  ggtitle("WGWIDE 2017-2020::WHM Recruitment Estimates, 1995 onwards")

png(filename = file.path(Rep.dir,"WGRecruitments.png"), width = 600, height = 400)
print(ggHistRec)
dev.off()

ggHistSSB <- ggplot(data = filter(dfWHMAssess) %>% select(Year=Yr,Assessment=WG,SSB), 
                    mapping = aes(x=Year, y=SSB/1e6, group = Assessment, col=Assessment)) + 
  geom_line(lwd=1) + ylab("SSB (Mt)") + xlab("Year") + theme(legend.position = "top") +
  ggtitle("WGWIDE 2017-2020::WHM SSB Estimates")

png(filename = file.path(Rep.dir,"WGSSB.png"), width = 600, height = 400)
print(ggHistSSB)
dev.off()

####################Selections#######################################################################
#data, assessment point estimates and range of values from 1000 iters
gSelProfs_WG19 <- ggplot(data = filter(dfSAOp,WG=="WGWIDE19" & Var=="Sel" & Year==rev(yrs_WG19)[1])) + geom_line(aes(x=Age,y=Val,group=Iter)) +
  geom_line(filter(dfSAOp,WG=="WGWIDE19" & Var=="Sel" & Iter==1 & Year==rev(yrs_WG19)[1]),mapping=aes(x=Age,y=Val,group=1),col="red",lwd=1)
gSelProfs_WG20 <- ggplot(data = filter(dfSAOp,WG=="WGWIDE20" & Var=="Sel" & Year==rev(yrs_WG20)[1])) + geom_line(aes(x=Age,y=Val,group=Iter)) +
  geom_line(filter(dfSAOp,WG=="WGWIDE20" & Var=="Sel" & Iter==1 & Year==rev(yrs_WG20)[1]),mapping=aes(x=Age,y=Val,group=1),col="red",lwd=1)

#distributions of selection@age
#distribution of values from 1000 iterations, point estimates from assessment
#sort stupid long x axis labels
gSelHistatAge_WG19 <- ggplot(data = filter(dfSAOp,WG == "WGWIDE19" & Var=="Sel" & Year==max(yrs_WG19)), mapping = aes(Val)) + 
  geom_bar() + scale_x_binned(n.breaks=15, nice.breaks=FALSE) +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1)) +
  geom_vline(xintercept=c(0.25,0.5,0.75), col="grey", lwd=1, lty=2) +
  facet_wrap(~Age) + ylab("Count") +
  theme(axis.text.x=element_blank())

gSelHistatAge_WG20 <- ggplot(data = filter(dfSAOp,WG == "WGWIDE20" & Var=="Sel" & Year==max(yrs_WG20)), mapping = aes(Val)) + 
  geom_bar() + scale_x_binned(n.breaks=15, nice.breaks=FALSE) +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1)) +
  geom_vline(xintercept=c(0.25,0.5,0.75), col="grey", lwd=1, lty=2) +
  facet_wrap(~Age) + ylab("Count") +
  theme(axis.text.x=element_blank())

png(filename = file.path(Rep.dir,"Sel_WG19.png"), width = 600, height = 600)
ggplot(data = filter(dfSAOp,WG=="WGWIDE19" & Var=="Sel" & Iter==1 & Age<=5), mapping = aes(x=Year,y=Val)) +
  geom_segment(filter(dfSAOp,WG=="WGWIDE19" & Var=="Sel" & Age<=5 & Year==dims(WG19)$maxyear & Iter==600) %>%
                 mutate(x1=dims(WG19)$minyear,x2=dims(WG19)$maxyear), mapping = aes(x = x1, y = Val, xend = x2, yend = Val), col = "grey") +
  geom_segment(filter(dfSAOp,WG=="WGWIDE19" & Var=="Sel" & Age<=5 & Year==dims(WG19)$maxyear & Iter==650) %>%
                 mutate(x1=dims(WG19)$minyear,x2=dims(WG19)$maxyear), mapping = aes(x = x1, y = Val, xend = x2, yend = Val), col = "grey") +
  geom_segment(filter(dfSAOp,WG=="WGWIDE19" & Var=="Sel" & Age<=5 & Year==dims(WG19)$maxyear & Iter==710) %>%
                 mutate(x1=dims(WG19)$minyear,x2=dims(WG19)$maxyear), mapping = aes(x = x1, y = Val, xend = x2, yend = Val), col = "grey") +
  geom_line(aes(group=fAge)) + 
  geom_segment(filter(dfSAOp,WG=="WGWIDE19" & Var=="Sel" & Age<=5) %>%
                 group_by(Age) %>% summarise(y1 = quantile(Val,0.95),y2 = quantile(Val,0.95)) %>%
                 mutate(x1=dims(WG19)$minyear,x2=dims(WG19)$maxyear), mapping = aes(x = x1, y = y1, xend = x2, yend = y2), col = "red", lty = 2) +
  geom_segment(filter(dfSAOp,WG=="WGWIDE19" & Var=="Sel" & Age<=5) %>%
                 group_by(Age) %>% summarise(y1 = quantile(Val,0.05),y2 = quantile(Val,0.05)) %>%
                 mutate(x1=dims(WG19)$minyear,x2=dims(WG19)$maxyear), mapping = aes(x = x1, y = y1, xend = x2, yend = y2), col = "red", lty = 2) +
  geom_line(data=filter(dfSimWeights,Var=="Sel" & Age<=5), mapping = aes(x=Year,y=Hi), col = "blue", lty=2) +
  geom_line(data=filter(dfSimWeights,Var=="Sel" & Age<=5), mapping = aes(x=Year,y=Lo), col = "blue", lty=2) +
  geom_line(data=filter(dfSimWeights,Var=="Sel" & Age<=5), mapping = aes(x=Year,y=Md), col = "blue") +
  geom_line(data = filter(data.frame(Age = rep(ages,nfYears), fAge = factor(rep(ages,nfYears),levels=ac(ages)), Year = rep(fYears, each=nAges), W = as.numeric(ifSel[,,600])),Age<=5), mapping = aes(x=Year,y=W), col="grey") +
  geom_line(data = filter(data.frame(Age = rep(ages,nfYears), fAge = factor(rep(ages,nfYears),levels=ac(ages)), Year = rep(fYears, each=nAges), W = as.numeric(ifSel[,,750])),Age<=5), mapping = aes(x=Year,y=W), col="grey") +
  geom_line(data = filter(data.frame(Age = rep(ages,nfYears), fAge = factor(rep(ages,nfYears),levels=ac(ages)), Year = rep(fYears, each=nAges), W = as.numeric(ifSel[,,710])),Age<=5), mapping = aes(x=Year,y=W), col="grey") +
  facet_wrap(~Age, labeller = labeller(fAge = c("0" = "Age 0", "1" = "Age 1", "2" = "Age 2", "3" = "Age 3",
                                                                             "4" = "Age 4", "5" = "Age 5", "6" = "Age 6", "7" = "Age 7",
                                                                             "8" = "Age 8", "9" = "Age 9", "10" = "Age 10", "11" = "Age 11",
                                                                             "12" = "Age 12", "13" = "Age 13", "14" = "Age 14", "15" = "Age 15+"))) +
  ylab("Selection") + theme(axis.text.x = element_text(angle = 45, hjust=1))
dev.off()



################Stock and Catch Weights##################################################################

png(filename = file.path(Rep.dir,"SW_WG19.png"), width = 600, height = 600)
ggplot(data = filter(dfWeights,WG=="WGWIDE19" & Var=="SW"), mapping = aes(x=Year,y=Wgt)) +
  geom_line(aes(group=Age)) +
  geom_segment(data = filter(dfSAWeights,WG=="WGWIDE19" & Iter==1 & Var=="SW") %>% mutate(x1=dims(WG19)$minyear,x2=dims(WG19)$maxyear), 
               mapping = aes(x = x1, y = Wgt, xend = x2, yend = Wgt), col="red") +
  geom_segment(filter(dfSAWeights,WG=="WGWIDE19" & Var=="SW") %>%
                 group_by(Age) %>% summarise(y1 = quantile(Wgt,0.95),y2 = quantile(Wgt,0.95)) %>%
                 mutate(x1=dims(WG19)$minyear,x2=dims(WG19)$maxyear), mapping = aes(x = x1, y = y1, xend = x2, yend = y2), col = "red", lty = 2) +
  geom_segment(filter(dfSAWeights,WG=="WGWIDE19" & Var=="SW") %>%
                 group_by(Age) %>% summarise(y1 = quantile(Wgt,0.05),y2 = quantile(Wgt,0.05)) %>%
                 mutate(x1=dims(WG19)$minyear,x2=dims(WG19)$maxyear), mapping = aes(x = x1, y = y1, xend = x2, yend = y2), col = "red", lty = 2) +
  facet_wrap(~Age, labeller = labeller(Age = c("0" = "Age 0", "1" = "Age 1", "2" = "Age 2", "3" = "Age 3",
                                               "4" = "Age 4", "5" = "Age 5", "6" = "Age 6", "7" = "Age 7",
                                               "8" = "Age 8", "9" = "Age 9", "10" = "Age 10", "11" = "Age 11",
                                               "12" = "Age 12", "13" = "Age 13", "14" = "Age 14", "15" = "Age 15+"))) +
  geom_line(data=filter(dfSimWeights,Var=="SW"), mapping = aes(x=Year,y=Hi), col = "blue", lty=2) +
  geom_line(data=filter(dfSimWeights,Var=="SW"), mapping = aes(x=Year,y=Lo), col = "blue", lty=2) +
  geom_line(data=filter(dfSimWeights,Var=="SW"), mapping = aes(x=Year,y=Md), col = "blue") +
  geom_line(data = data.frame(Age = factor(rep(ages,nfYears),levels=ac(ages)), Year = rep(fYears, each=nAges), W = as.numeric(ifSW[,,601])), mapping = aes(x=Year,y=W), col="grey") +
  geom_line(data = data.frame(Age = factor(rep(ages,nfYears),levels=ac(ages)), Year = rep(fYears, each=nAges), W = as.numeric(ifSW[,,724])), mapping = aes(x=Year,y=W), col="grey") +
  ylab("Stock Weight (kg)") + theme(axis.text.x = element_text(angle = 45, hjust=1))
dev.off()

png(filename = file.path(Rep.dir,"CW_WG19.png"), width = 600, height = 600)
ggplot(data = filter(dfWeights,WG=="WGWIDE19" & Var=="CW"), mapping = aes(x=Year,y=Wgt)) +
  geom_line(aes(group=Age)) +
  geom_segment(data = filter(dfSAWeights,WG=="WGWIDE19" & Iter==1 & Var=="CW") %>% mutate(x1=dims(WG19)$minyear,x2=dims(WG19)$maxyear), 
               mapping = aes(x = x1, y = Wgt, xend = x2, yend = Wgt), col="red") +
  geom_segment(filter(dfSAWeights,WG=="WGWIDE19" & Var=="CW") %>%
                 group_by(Age) %>% summarise(y1 = quantile(Wgt,0.95),y2 = quantile(Wgt,0.95)) %>%
                 mutate(x1=dims(WG19)$minyear,x2=dims(WG19)$maxyear), mapping = aes(x = x1, y = y1, xend = x2, yend = y2), col = "red", lty = 2) +
  geom_segment(filter(dfSAWeights,WG=="WGWIDE19" & Var=="CW") %>%
                 group_by(Age) %>% summarise(y1 = quantile(Wgt,0.05),y2 = quantile(Wgt,0.05)) %>%
                 mutate(x1=dims(WG19)$minyear,x2=dims(WG19)$maxyear), mapping = aes(x = x1, y = y1, xend = x2, yend = y2), col = "red", lty = 2) +
  facet_wrap(~Age, labeller = labeller(Age = c("0" = "Age 0", "1" = "Age 1", "2" = "Age 2", "3" = "Age 3",
                                               "4" = "Age 4", "5" = "Age 5", "6" = "Age 6", "7" = "Age 7",
                                               "8" = "Age 8", "9" = "Age 9", "10" = "Age 10", "11" = "Age 11",
                                               "12" = "Age 12", "13" = "Age 13", "14" = "Age 14", "15" = "Age 15+"))) +
  geom_line(data=filter(dfSimWeights,Var=="CW"), mapping = aes(x=Year,y=Hi), col = "blue", lty=2) +
  geom_line(data=filter(dfSimWeights,Var=="CW"), mapping = aes(x=Year,y=Lo), col = "blue", lty=2) +
  geom_line(data=filter(dfSimWeights,Var=="CW"), mapping = aes(x=Year,y=Md), col = "blue") +
  geom_line(data = data.frame(Age = factor(rep(ages,nfYears),levels=ac(ages)), Year = rep(fYears, each=nAges), W = as.numeric(ifCW[,,601])), mapping = aes(x=Year,y=W), col="grey") +
  geom_line(data = data.frame(Age = factor(rep(ages,nfYears),levels=ac(ages)), Year = rep(fYears, each=nAges), W = as.numeric(ifCW[,,724])), mapping = aes(x=Year,y=W), col="grey") +
  ylab("Catch Weight (kg)") + theme(axis.text.x = element_text(angle = 45, hjust=1))
dev.off()














#catch opportunity histograms
dfCatchOps <- data.frame(Year=rep(seq(2016,2020),each=100),
                          catch = rnorm(500))

ggCatchOps <- ggplot(data = dfCatchOps, mapping = aes(catch, fill = cut(catch,10))) +
  geom_histogram(binwidth=0.25, show.legend = FALSE) + facet_wrap(Year,ncol=1,strip.position="right") +
  theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())

ggCatchOps

#use the selected HCR (5.23 - update this to some 'baseline' run?)
runRef <- "WHOM_SS3_OM2.2_MP5.23_1000_23"
Sim.dir <- file.path(Res.dir,runRef)

load(file=file.path(Sim.dir,paste0(runRef,"_SimRuns.Rdata")))

#target F = 0.075
sim <- SimRuns[["0.075"]]

simYears <- seq(2018,length=23)
nsimYears = length(simYears)
iters <- seq(1,1000)
niters <- 1000

cw <- array(sim$catW,
            dim = c(nages,nsimYears,niters),
            dimnames = list(age=ages, simYear=simYears, iter=iters))

sw <- array(sim$stkW,
            dim = c(nages,nsimYears,niters),
            dimnames = list(age=ages, simYear=simYears, iter=iters))

N <- array(sim$N,
            dim = c(nages,nsimYears,niters),
            dimnames = list(age=ages, simYear=simYears, iter=iters))

rec <- N[c("0"),,]

#get the historic data
dfSW <- readr::read_delim(file = file.path(Data.dir,"StockWeights.dat"),delim=",")
dfCW <- readr::read_delim(file = file.path(Data.dir,"CatchWeights.dat"),delim=",")
#single df
dfWeights <- dplyr::left_join(
  dfSW %>% pivot_longer(cols = paste0("Age",seq(0,15)), names_to = "Age", values_to = "SW", names_prefix = "Age"),
  dfCW %>% pivot_longer(cols = paste0("Age",seq(0,15)), names_to = "Age", values_to = "CW", names_prefix = "Age"),
  by=c("Year","Age"))

#dfWeights <- within(dfWeights, Age <- factor(Age, levels = ac(ages)))

histYears <- seq(range(dfWeights$Year)[1],dfWeights$Year[2])
allYears <- seq(min(histYears),max(simYears))

#Stock objects - point estimates and the 1000 iterations
FLStockfile    <- "WGWIDE19.RData"
FLS <- loadRData(file.path(RData.dir,FLStockfile)) %>% FLCore::setPlusGroup(., 15)
dfSAWeights = data.frame(Age = ages, iter=0, CW = rowMeans(catch.wt(FLS)), SW = rowMeans(stock.wt(FLS)), stringsAsFactors = FALSE)
dfSAWeights <- within(dfSAWeights, Age <- factor(Age, levels = ac(ages)))

FLStockSimfile <- "MSE_WGWIDE19_FLStocks_1k15PG.RData"
FLSs <- loadRData(file.path(RData.dir,FLStockSimfile))

#extract catch, stock weight info from each iteration
FLSs.CW <- lapply(lapply(FLSs,FLCore::catch.wt),rowMeans)
FLSs.SW <- lapply(lapply(FLSs,FLCore::stock.wt),rowMeans)
iCW <- matrix(unlist(FLSs.CW),nrow=nages,ncol=length(FLSs),dimnames=list(age=ac(ages),iter=seq(1,length(FLSs))))
iSW <- matrix(unlist(FLSs.SW),nrow=nages,ncol=length(FLSs),dimnames=list(age=ac(ages),iter=seq(1,length(FLSs))))
dfSAWeights <- dplyr::bind_rows(dfSAWeights,data.frame(Age = ac(ages), iter=rep(seq(1,length(FLSs)),each=nages), CW = c(iCW), SW = c(iSW)))
#make Age a factor so plot order appropriate
dfSAWeights <- within(dfSAWeights, Age <- factor(Age, levels = ac(ages)))


#expand df to store simulated values
dfWeights$Upper <- NA
dfWeights$Med <- NA
dfWeights$Lower <- NA

for (a in ages){
  lower <- apply(sw[ac(a),,],MARGIN=1,FUN=stats::quantile,probs=c(0.05))
  med <- apply(sw[ac(a),,],MARGIN=1,FUN=stats::quantile,probs=c(0.5))
  upper <- apply(sw[ac(a),,],MARGIN=1,FUN=stats::quantile,probs=c(0.95))
  
  dfWeights <- dplyr::bind_rows(dfWeights,
                                 data.frame(Year=simYears,Age=ac(rep(a,length(simYears))),Lower=lower,Med=med,Upper=upper))
}

dfWeights <- within(dfWeights, Age <- factor(Age, levels = ac(ages)))

#swap geom_segment for geom_line to clip horizontal extents
#eg geom_segment(aes(x=2,xend=4,y=20,yend=20))

gSW <- ggplot(data = dfWeights, mapping = aes(x=Year,y=SW)) +
  geom_line(aes(group=Age)) +
  geom_hline(data = filter(dfSAWeights,iter==0), mapping=aes(yintercept=SW), col="red") +
  geom_hline(data = dfSAWeights %>% group_by(Age) %>% summarise(mn = min(SW), mx = max(SW)), mapping=aes(yintercept=mx), col="red", lty=2) +
  geom_hline(data = dfSAWeights %>% group_by(Age) %>% summarise(mn = min(SW), mx = max(SW)), mapping=aes(yintercept=mn), col="red", lty=2) +
  facet_wrap(~Age) + ylab("Stock Weight")


simYears

#age plots
a<-7
lower <- apply(sw[ac(a),,],MARGIN=1,FUN=stats::quantile,probs=c(0.05))
med <- apply(sw[ac(a),,],MARGIN=1,FUN=stats::quantile,probs=c(0.5))
upper <- apply(sw[ac(a),,],MARGIN=1,FUN=stats::quantile,probs=c(0.95))

plot(simYears,lower,type="l",ylim=c(0,0.5),xlim=c(min(allYears),max(allYears)))
lines(simYears,upper,type="l")
lines(simYears,med)
for (ii in seq(99,length=10,by=100)){lines(simYears,sw[ac(a),,ii],col="grey")}
lines(dfWeights$Year[dfWeights$Age==a],dfWeights$SW[dfWeights$Age==a],col="red")

sw %>% 
#percentiles of sw
quantile(sw[6,'2018',],probs=c(0.05,0.5,0.95))
