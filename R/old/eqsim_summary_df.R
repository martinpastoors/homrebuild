#eqsim_summary_df
# Explort a single run into a df

# runs <- c("OM2.2_MP2.1_1000_20")
# lStatPer = lStatPer
# Blim <- OM$refPts$Blim
# PerfStat = c("IAVUpDown")
# Fbarrange=c(1,10)

fsummary_df <- function(run, simRuns, 
                        Res.dir, Plot.dir, 
                        simYears, xlab, 
                        lStatPer, Blim, 
                        Fbarrange=c(1,10)){

  #produces a dataframe
  #comparing the supplied performance statistic for ST,MT and LT for each runName
  #grouped by runName
  
  require(tidyverse)
  
  dfAll <- data.frame(RunRef = c(), Label=c(), Ftgt = c(), PerfStat = c(), Period = c(), Val = c(), stringsAsFactors = FALSE)

  #get the data
  # run <- "OM2.2_MP2.1_1000_20"
  # cat(run,"\n")
  
  #load the output of the simulation and the summary statistics
  # load(file = file.path(Res.dir,run,paste0(run,"_SimRuns.RData")))
  # load(file = file.path(Res.dir,run,paste0(run,"_eqSim_Stats.RData")))
  
  
  
  # ftgt <- 0.1
  for (ftgt in an(names(SimRuns))){
    
#    cat("F ", ftgt,"\n")
    
    #simulation op
    t <- SimRuns[[ac(ftgt)]]
    
    #simulation stats
    # t2 <- lOp$stats[[ac(ftgt)]]
    # t2 <- lStats$stats[[ac(ftgt)]]

    # p <- "SSB"
    
    for (p in c("Catch","IAV","IAVUpDown", "SSB","Risk3","Risk1", "F")){
      
      # cat("PerfStat: ",p, "\n")
      
      if (p %in% c("Catch","IAV","IAVUpDown")) {
      
        #catch numbers
        Cnum <- t[["C"]]
        #catch weights
        Cwgt <- t[["catW"]]
        #catch weight (tons)
        CW <- apply(Cnum*Cwgt,2:3,sum)/1e3
        dimnames(CW)$year <- simYears
        
        if (p =="Catch"){
          
          # cat("Catch ","\n")
          
          CU = as.numeric(apply(CW[ac(seq(lStatPer$CU[1],lStatPer$CU[2])),],2,mean))
          ST = as.numeric(apply(CW[ac(seq(lStatPer$ST[1],lStatPer$ST[2])),],2,mean))
          MT = as.numeric(apply(CW[ac(seq(lStatPer$MT[1],lStatPer$MT[2])),],2,mean))
          LT = as.numeric(apply(CW[ac(seq(lStatPer$LT[1],lStatPer$LT[2])),],2,mean))
          
          StatName = "Yield"
          StatUnit = "(kt)"
        }
        
        if (p=="IAV"){

          # cat("IAV ","\n")
          
          IAV <- abs(1-CW[-1,]/CW[-nrow(CW),])
          #replace Inf with NA (NA results from comparing with zero catch)
          IAV <- ifelse(is.finite(IAV),IAV,NA)
          
          # CU = apply(IAV[as.character(seq(lStatPer$CU[1],lStatPer$CU[2])),],2,mean)
          ST = apply(IAV[as.character(seq(lStatPer$ST[1],lStatPer$ST[2])),],2,mean)
          MT = apply(IAV[as.character(seq(lStatPer$MT[1],lStatPer$MT[2])),],2,mean)
          LT = apply(IAV[as.character(seq(lStatPer$LT[1],lStatPer$LT[2])),],2,mean)
          
          StatName = "IAV"
          StatUnit = ""
          
        }
        
        if (p=="IAVUpDown"){

          # cat("IAVUpDown ","\n")
          
          IAVUp <- IAVDown <- IAVUpDown <- (CW[-1,]/CW[-nrow(CW),]) - 1
          
          IAVUp[!is.finite(IAVUp)]<-NA
          IAVDown[!is.finite(IAVDown)]<-NA
          
          IAVUp[IAVUp<0] <- NA
          IAVDown[IAVDown>0] <- NA
          
          # CU = c(apply(IAVUp[as.character(seq(lStatPer$CU[1],lStatPer$CU[2])),],2,mean,na.rm=TRUE),
          #        apply(IAVDown[as.character(seq(lStatPer$CU[1],lStatPer$CU[2])),],2,mean,na.rm=TRUE))
          ST = c(apply(IAVUp[as.character(seq(lStatPer$ST[1],lStatPer$ST[2])),],2,mean,na.rm=TRUE),
                 apply(IAVDown[as.character(seq(lStatPer$ST[1],lStatPer$ST[2])),],2,mean,na.rm=TRUE))
          MT = c(apply(IAVUp[as.character(seq(lStatPer$MT[1],lStatPer$MT[2])),],2,mean,na.rm=TRUE),
                 apply(IAVDown[as.character(seq(lStatPer$MT[1],lStatPer$MT[2])),],2,mean,na.rm=TRUE))
          LT = c(apply(IAVUp[as.character(seq(lStatPer$LT[1],lStatPer$LT[2])),],2,mean,na.rm=TRUE),
                 apply(IAVDown[as.character(seq(lStatPer$LT[1],lStatPer$LT[2])),],2,mean,na.rm=TRUE))
          
          # CU[!is.finite(CU)]<-NA
          ST[!is.finite(ST)]<-NA
          MT[!is.finite(MT)]<-NA
          LT[!is.finite(LT)]<-NA
          
          StatName = c("IAVUp","IAVDown")
          StatUnit = c("")
          
        }
          
      } else if (p %in% c("SSB","Risk3","Risk1", "F")){

        #harvest
        Harvest <- t[["F"]]
        dimnames(Harvest)$year <- simYears
        Harvest <- apply(Harvest[ac(Fbarrange[1]:Fbarrange[2]),,],2:3, mean)

        #abundance
        Abd <- t[["N"]]
        #stock weights
        SW <- t[["stkW"]]
        #maturity
        Mat <- t[["Mat"]]

        #SSB (Mt)
        SSB <- apply(Abd*SW*Mat,2:3,sum)/1e6
        dimnames(SSB)$year <- simYears
        
        # Harvest[ac(1:10),ac(seq(lStatPer$CU[1],lStatPer$CU[2])),]
        # apply(Harvest[ac(seq(lStatPer$CU[1],lStatPer$CU[2])), ],2,mean)
        
        if (p == "F") {
          
          StatName = "F"
          StatUnit = "1/y"
          CU = as.numeric(apply(Harvest[ac(seq(lStatPer$CU[1],lStatPer$CU[2])), ],2,mean))
          ST = as.numeric(apply(Harvest[ac(seq(lStatPer$ST[1],lStatPer$ST[2])),],2,mean))
          MT = as.numeric(apply(Harvest[ac(seq(lStatPer$MT[1],lStatPer$MT[2])),],2,mean))
          LT = as.numeric(apply(Harvest[ac(seq(lStatPer$LT[1],lStatPer$LT[2])),],2,mean))
          
        } else if (p == "SSB") {
          
          # cat("SSB ","\n")
          
          StatName = "SSB"
          StatUnit = "(Mt)"
          CU = as.numeric(apply(SSB[ac(seq(lStatPer$CU[1],lStatPer$CU[2])),],2,mean))
          ST = as.numeric(apply(SSB[ac(seq(lStatPer$ST[1],lStatPer$ST[2])),],2,mean))
          MT = as.numeric(apply(SSB[ac(seq(lStatPer$MT[1],lStatPer$MT[2])),],2,mean))
          LT = as.numeric(apply(SSB[ac(seq(lStatPer$LT[1],lStatPer$LT[2])),],2,mean))
        
        } else if (p == "Risk3"){
          
          # cat("Risk3 ","\n")
          
          StatName = "Risk3"
          StatUnit = "p"
          #maximum probability that SSB is below Blim, where the maximum (of the annual probabilities) is taken over the statistical period 
          CU = max(apply(SSB[ac(seq(lStatPer$CU[1],lStatPer$CU[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
          ST = max(apply(SSB[ac(seq(lStatPer$ST[1],lStatPer$ST[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
          MT = max(apply(SSB[ac(seq(lStatPer$MT[1],lStatPer$MT[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
          LT = max(apply(SSB[ac(seq(lStatPer$LT[1],lStatPer$LT[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
        
        } else if (p == "Risk1"){
          
          # cat("Risk1 ","\n")
          
          StatName = "Risk1"
          StatUnit = "p"
          #average probability that SSB is below Blim over the statistical period 
          CU = mean(apply(SSB[ac(seq(lStatPer$CU[1],lStatPer$CU[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
          ST = mean(apply(SSB[ac(seq(lStatPer$ST[1],lStatPer$ST[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
          MT = mean(apply(SSB[ac(seq(lStatPer$MT[1],lStatPer$MT[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
          LT = mean(apply(SSB[ac(seq(lStatPer$LT[1],lStatPer$LT[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
        }
        
      } # end of if statement for PerfStat p
      
      #CU
      if(exists("CU")) {
        dfAll <- dplyr::bind_rows(dfAll,
                                  data.frame(RunRef = rep(run,length(CU)),
                                             Label = rep(xlab,length(CU)),
                                             Ftgt = rep(ftgt,length(CU)),
                                             PerfStat = rep(StatName,each=length(CU)/length(StatName)),
                                             Period = rep("CU",each=length(CU)),
                                             Period2 = paste(lStatPer$CU, collapse="-"),
                                             Iter = seq(1:length(CU)),
                                             Val = CU,
                                             stringsAsFactors = FALSE))
        rm(CU)  
      }
      
      #ST
      dfAll <- dplyr::bind_rows(dfAll,
                                data.frame(RunRef = rep(run,length(ST)),
                                           Label = rep(xlab,length(ST)),
                                           Ftgt = rep(ftgt,length(ST)),
                                           PerfStat = rep(StatName,each=length(ST)/length(StatName)),
                                           Period = rep("ST",each=length(ST)),
                                           Period2 = paste(lStatPer$ST, collapse="-"),
                                           Iter = seq(1:length(ST)),
                                           Val = ST,
                                           stringsAsFactors = FALSE))
      #MT
      dfAll <- dplyr::bind_rows(dfAll,
                                data.frame(RunRef = rep(run,length(MT)),
                                           Label = rep(xlab,length(MT)),
                                           Ftgt = rep(ftgt,length(MT)),
                                           PerfStat = rep(StatName,each=length(MT)/length(StatName)),
                                           Period = rep("MT",each=length(MT)),
                                           Period2 = paste(lStatPer$MT, collapse="-"),
                                           Iter = seq(1:length(ST)),
                                           Val = MT,
                                           stringsAsFactors = FALSE))
      #LT
      dfAll <- dplyr::bind_rows(dfAll,
                                data.frame(RunRef = rep(run,length(LT)),
                                           Label = rep(xlab,length(LT)),
                                           Ftgt = rep(ftgt,length(LT)),
                                           PerfStat = rep(StatName,each=length(LT)/length(StatName)),
                                           Period = rep("LT",each=length(LT)),
                                           Period2 = paste(lStatPer$LT, collapse="-"),
                                           Iter = seq(1:length(ST)),
                                           Val = LT,
                                           stringsAsFactors = FALSE))
      
    } # end of p loop

  } # end of for loop: ftgt
    

  dfAll
  
} # end of function


# dfAll %>% 
#   filter(PerfStat == "Yield") %>% 
#   mutate(Ftgt = as.numeric(Ftgt)) %>% 
#   mutate(Period = factor(Period, levels=c("ST","MT","LT"))) %>% 
#   ggplot(aes(x=Ftgt, y=Val, group=Period)) +
#   theme_bw() +
#   geom_bar(stat="identity") +
#   facet_grid(RunRef ~ Period)

