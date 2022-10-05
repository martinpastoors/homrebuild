#eqsim_compare_runs

fCompare_runs <- function(runs2Compare, Res.dir, Plot.dir, PerfStat, TargetFs, lStatPer, Blim) {

  #produces a grid (max 6)
  #comparing the supplied performance statistic for ST,MT and LT for each runName
  #grouped by runName
  
  require(ggplot2)
  require(dplyr)
  
  dfAll <- data.frame(RunRef = c(), Label=c(), Ftgt = c(), PerfStat = c(), Period = c(), Val = c(), stringsAsFactors = FALSE)

  #get the data
  for (r in runs2Compare){
    
    cat(r,"\n")
    
    #load the output of the simulation and the summary statistics
    load(file = file.path(Res.dir,r,paste0(r,"_SimRuns.RData")))
    load(file = file.path(Res.dir,r,paste0(r,"_eqSim_Stats.RData")))

    for (ftgt in TargetFs){
      
      #simulation op
      t <- SimRuns[[ac(ftgt)]]
      #simulation stats
      #t2 <- lOp$stats[[ac(ftgt)]]
      t2 <- lStats$stats[[ac(ftgt)]]
      
      if (PerfStat %in% c("Catch","IAV","IAVUpDown")) {
        
        #catch numbers
        Cnum <- t[["C"]]
        #catch weights
        Cwgt <- t[["catW"]]
        #catch weight (tons)
        CW <- apply(Cnum*Cwgt,2:3,sum)/1e3
        dimnames(CW)$year <- t2$simYears
        
        if (PerfStat=="Catch"){
          
          ST = as.numeric(apply(CW[ac(seq(lStatPer$ST[1],lStatPer$ST[2])),],2,mean))
          MT = as.numeric(apply(CW[ac(seq(lStatPer$MT[1],lStatPer$MT[2])),],2,mean))
          LT = as.numeric(apply(CW[ac(seq(lStatPer$LT[1],lStatPer$LT[2])),],2,mean))
          
          StatName = "Yield"
          StatUnit = "(kt)"
        }
        
        if (PerfStat=="IAV"){

          IAV <- abs(1-CW[-1,]/CW[-nrow(CW),])
          #replace Inf with NA (NA results from comparing with zero catch)
          IAV <- ifelse(is.finite(IAV),IAV,NA)
          
          ST = apply(IAV[as.character(seq(lStatPer$ST[1],lStatPer$ST[2])),],2,mean)
          MT = apply(IAV[as.character(seq(lStatPer$MT[1],lStatPer$MT[2])),],2,mean)
          LT = apply(IAV[as.character(seq(lStatPer$LT[1],lStatPer$LT[2])),],2,mean)
          
          StatName = "IAV"
          StatUnit = ""
          
        }
        
        if (PerfStat=="IAVUpDown"){

          IAVUp <- IAVDown <- IAVUpDown <- (CW[-1,]/CW[-nrow(CW),]) - 1
          
          IAVUp[!is.finite(IAVUp)]<-NA
          IAVDown[!is.finite(IAVDown)]<-NA
          
          IAVUp[IAVUp<0] <- NA
          IAVDown[IAVDown>0] <- NA
          
          ST = c(apply(IAVUp[as.character(seq(lStatPer$ST[1],lStatPer$ST[2])),],2,mean,na.rm=TRUE),
                 apply(IAVDown[as.character(seq(lStatPer$ST[1],lStatPer$ST[2])),],2,mean,na.rm=TRUE))
          MT = c(apply(IAVUp[as.character(seq(lStatPer$MT[1],lStatPer$MT[2])),],2,mean,na.rm=TRUE),
                 apply(IAVDown[as.character(seq(lStatPer$MT[1],lStatPer$MT[2])),],2,mean,na.rm=TRUE))
          LT = c(apply(IAVUp[as.character(seq(lStatPer$LT[1],lStatPer$LT[2])),],2,mean,na.rm=TRUE),
                 apply(IAVDown[as.character(seq(lStatPer$LT[1],lStatPer$LT[2])),],2,mean,na.rm=TRUE))
          
          ST[!is.finite(ST)]<-NA
          MT[!is.finite(MT)]<-NA
          LT[!is.finite(LT)]<-NA
          
          StatName = c("IAVUp","IAVDown")
          StatUnit = c("")
          
        }
          
      } else if (PerfStat %in% c("SSB","Risk3","Risk1")){

        #abundance
        Abd <- t[["N"]]
        #stock weights
        SW <- t[["stkW"]]
        #maturity
        Mat <- t[["Mat"]]

        #SSB (Mt)
        SSB <- apply(Abd*SW*Mat,2:3,sum)/1e6
        dimnames(SSB)$year <- t2$simYears
        
        if (PerfStat == "SSB") {
          StatName = "SSB"
          StatUnit = "(Mt)"
          ST = as.numeric(apply(SSB[ac(seq(lStatPer$ST[1],lStatPer$ST[2])),],2,mean))
          MT = as.numeric(apply(SSB[ac(seq(lStatPer$MT[1],lStatPer$MT[2])),],2,mean))
          LT = as.numeric(apply(SSB[ac(seq(lStatPer$LT[1],lStatPer$LT[2])),],2,mean))
        } else if (PerfStat == "Risk3"){
          StatName = "Risk3"
          StatUnit = "p"
          #maximum probability that SSB is below Blim, where the maximum (of the annual probabilities) is taken over the statistical period 
          ST = max(apply(SSB[ac(seq(lStatPer$ST[1],lStatPer$ST[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
          MT = max(apply(SSB[ac(seq(lStatPer$MT[1],lStatPer$MT[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
          LT = max(apply(SSB[ac(seq(lStatPer$LT[1],lStatPer$LT[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
        } else if (PerfStat == "Risk1"){
          StatName = "Risk1"
          StatUnit = "p"
          #average probability that SSB is below Blim over the statistical period 
          ST = mean(apply(SSB[ac(seq(lStatPer$ST[1],lStatPer$ST[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
          MT = mean(apply(SSB[ac(seq(lStatPer$MT[1],lStatPer$MT[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
          LT = mean(apply(SSB[ac(seq(lStatPer$LT[1],lStatPer$LT[2])),]<Blim/1e6,1,sum)/dim(SSB)[2])
        }
        
      }

      dfAll <- dplyr::bind_rows(dfAll,
                                data.frame(RunRef = rep(r,length(ST)),
                                           Label = rep(t2$MP$xlab,length(ST)),
                                           Ftgt = rep(ftgt,length(ST)),
                                           PerfStat = rep(StatName,each=length(ST)/length(StatName)),
                                           Period = rep("ST",each=length(ST)),
                                           Val = ST,
                                           stringsAsFactors = FALSE))
      dfAll <- dplyr::bind_rows(dfAll,
                                data.frame(RunRef = rep(r,length(MT)),
                                           Label = rep(t2$MP$xlab,length(MT)),
                                           Ftgt = rep(ftgt,length(MT)),
                                           PerfStat = rep(StatName,each=length(MT)/length(StatName)),
                                           Period = rep("MT",each=length(MT)),
                                           Val = MT,
                                           stringsAsFactors = FALSE))
      dfAll <- dplyr::bind_rows(dfAll,
                                data.frame(RunRef = rep(r,length(LT)),
                                           Label = rep(t2$MP$xlab,length(LT)),
                                           Ftgt = rep(ftgt,length(LT)),
                                           PerfStat = rep(StatName,each=length(LT)/length(StatName)),
                                           Period = rep("LT",each=length(LT)),
                                           Val = LT,
                                           stringsAsFactors = FALSE))
      
    }
    
  }

  if (PerfStat == "IAVUpDown"){
    for (s in c("IAVUp","IAVDown")) {
      p <- ggplot(data = filter(dfAll,PerfStat==s), aes(x=factor(Label), y=Val, fill=factor(Period, levels = c("ST","MT","LT")))) +
        geom_boxplot(outlier.size = 0.2) +
        facet_wrap(vars(paste("Ftgt = ",Ftgt))) +
        ggtitle(paste(s,StatUnit)) + 
        theme(legend.position="none",
              axis.title.y = element_blank(),
              axis.title.x = element_blank(),
              strip.text.x = element_text(size=12, face="bold"),
              axis.text.x = element_text(size=8, face="bold"),
              strip.background = element_rect(colour="red", fill="#CCCCFF"))
      if (s=="IAVUp") p <- p + ylim(0,1)
      if (s=="IAVDown") p <- p + ylim(-1,0)
      
      png(filename = file.path(Plot.dir,paste0(s,"Comparison.png")),
          type = 'cairo', units = 'in', width = 10,
          height = 8, pointsize = 12, res = 96)
      print(p)
      dev.off()
    }
  } else {
  
    #geom_col for risks, boxplots otherwise
    p <- ggplot(data = dfAll, aes(x=factor(Label), y=Val, fill=factor(Period, levels = c("ST","MT","LT"))))
    
    if (PerfStat %in% c("Risk1","Risk3")){
      p <- p + geom_col(position="dodge") + geom_hline(yintercept = 0.05, col="black", linetype=2)
    } else {
      p <- p + geom_boxplot(outlier.size = 0.2)
    }
    
    p <- p +  
      facet_wrap(vars(paste("Ftgt = ",Ftgt))) +
      ggtitle(paste(StatName,StatUnit)) + 
      theme(legend.position="none",
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            strip.text.x = element_text(size=12, face="bold"),
            axis.text.x = element_text(size=8, face="bold"),
            strip.background = element_rect(colour="red", fill="#CCCCFF"))
      
    if (PerfStat=="IAV"){p <- p + ylim(0,1)}
    
    png(filename = file.path(Plot.dir,paste0(StatName,"Comparison.png")),
        type = 'cairo', units = 'in', width = 10,
        height = 8, pointsize = 12, res = 96)
    print(p)
    dev.off()
  }
  
}