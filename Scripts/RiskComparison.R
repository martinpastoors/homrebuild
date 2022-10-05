
#comparison of risk calculations for varying numbers of iterations
#based on a run with 10k iterations
#randomly select 500, 1000, 1500, ... , 10000 for comparison
#risk 1 - average probability that SSB is below Blim where the average (of the annual probabilities) is taken across the reporting period
#risk 3 - maximum probability that SSB is below Blim, where the maximum (of the annual probabilities) is taken over the reporting period

rm(list=ls())
gc()
try(dev.off(),silent=TRUE)
try(sink(),silent=TRUE)

library(FLCore)
library(Cairo)
library(gplots)   #rich.colors
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

#locations
Drive <- "C:"
Base.dir <- file.path(Drive,"Stocks","hom_27_2a4a5b6a7a-ce-k8")
Assessment.Dir <- file.path(Base.dir,"Assessment")
#MSE.dir <- file.path(Base.dir,"MP_MSE","MSE 2019","whm.MSE2019.WKREBUILD")
MSE.dir <- file.path(Base.dir,"MP_MSE","MSE 2020")
Data.dir <- file.path(MSE.dir,"Data")              #this is where the results of the 1000 assessment runs for initialisation of the MSE are saved
RData.dir <- file.path(MSE.dir,"RData")            #historic assessment outputs, stock-recruit fits
Log.dir <- file.path(MSE.dir,"Logs")              #debug/verbose output
Res.dir <- file.path(MSE.dir, "Results")


Source.dir <- file.path(getwd(),"R")              #R functions
Scripts.dir <- file.path(getwd(), "Scripts")      #R scripts

#OMs, MPs
source(file = file.path(Scripts.dir,"OMs.R"))
source(file = file.path(Scripts.dir,"MPs.R"))

OM <- OM2; MP <- MP2.0_10000
runName <- paste(OM$code,MP$code,sep="_")

load(file = file.path(Res.dir, runName, "OM2_MP2.0_10000_eqSim_Workspace.Rdata"))
#select f=0.1
t <- SimRuns[['0.1']]
#abundance
Abd <- t[["N"]]
#stock weights
SW <- t[["stkW"]]
#maturity
Mat <- t[["Mat"]]

#SSB (Mt)
SSB <- apply(Abd*SW*Mat,2:3,sum)/1e6
dimnames(SSB)$year <- simYears

blnBelowSSB <- SSB<(Blim/1e6)

dfAllRisks <- data.frame(Reps = c(), Per = c(), R1 = c(), R2 = c(), stringsAsFactors = FALSE)

#1000 risk calculations, each based on a sample of r replicates, resampled with replacement from the 10000
for (r in seq(500,10000,by=500)){

  cat("\n",r,"replicates\n")
  
  for (i in 1:1000){
    
    sample.iters <- sample(seq(1,10000),size=r,replace=TRUE)
    
    for (sp in c('ST','MT','LT')){
        
      bln.sample <- blnBelowSSB[ac(seq(lStatPer[[sp]][1],lStatPer[[sp]][2])),sample.iters]

      dfAllRisks <- dplyr::bind_rows(dfAllRisks,
                                     data.frame(Reps = r,
                                                Per = sp,
                                                R1 = mean(as.numeric(apply(bln.sample, MARGIN=1, FUN=sum))/r),
                                                R3 = max(as.numeric(apply(bln.sample, MARGIN=1, FUN=sum))/r),
                                                stringsAsFactors = FALSE))
    }
  }
}

#plots

for (sp in c('ST','MT','LT')){
  p.R3 <- ggplot(data = dplyr::filter(dfAllRisks,Per==sp), aes(x = as.factor(Reps), y = 100*R3)) + 
    geom_boxplot(outlier.shape = 1) + 
    geom_hline(yintercept=100*median(dfAllRisks$R3[dfAllRisks$Reps==10000 & dfAllRisks$Per==sp]),color="red",lwd=1) + 
    theme_bw() +
    ylim(0,5) +
    labs(x = "Number of Replicates", y = "p(SSB<Blim) (%)") + 
    ggtitle(paste0("Risk Type 3, ",sp,", ICES HCR, F=0.1")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  p.R1 <- ggplot(data = dplyr::filter(dfAllRisks,Per==sp), aes(x = as.factor(Reps), y = 100*R1)) + 
    geom_boxplot(outlier.shape = 1) + 
    geom_hline(yintercept=100*median(dfAllRisks$R1[dfAllRisks$Reps==10000 & dfAllRisks$Per==sp]),color="red",lwd=1) + 
    theme_bw() +
    ylim(0,5) +
    labs(x = "Number of Replicates", y = "p(SSB<Blim) (%)") + 
    ggtitle(paste0("Risk Type 1, ",sp,", ICES HCR, F=0.1")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  p <- cowplot::plot_grid(p.R3, p.R1)

  Cairo(width=780, height=480, file=file.path(Res.dir,paste0("RiskComparison_",sp,".png")))
  print(p)
  dev.off()
  
}
