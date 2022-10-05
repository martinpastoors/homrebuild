#stats

#statPercs
#Computes the mean, min, max, percentiles, standard deviation and variance across iterations 
#for each of the years and ranges supplied
fStatPercs <- function(stat, lStatPer, percs=c(0.01,0.025,0.05,0.10,0.25,0.5,0.75,0.90,0.95,0.975,0.99)) {
  
  #lPeriods = list of statistical reporting periods
  #percs = percentiles to calculate
  
  #create an FLQuant object to store the results
  #stat goes in age dimension
  #for age based (selection, catch weight, stock weight) age is used and mean is calculated
  
  #x <- FLQuant(NA, dimnames=list(age=c("min",paste(100*percs,"%",sep=""),"max","mean","sd","var"), year=YRS))
  
  #browser()
  
  #all iterations
  ITS <- 1:length(stat[1,1])
  
  if (dim(stat)[1]>1){
    
    ages <- dimnames(stat)$age
    #cat(ages,"\n")
    #cat(names(lStatPer),"\n")
    #age based
    #x <- FLQuant(NA, dimnames=list(age=seq(1,dim(stat)[1]), year=names(lStatPer)))
    x <- FLQuant(NA, dimnames=list(age=ages, year=names(lStatPer)))
    
    for (p in 1:length(lStatPer)) {
      
      y1 <- ac(lStatPer[[p]][1])
      y2 <- ac(lStatPer[[p]][2])
      y12 <- ac(seq(lStatPer[[p]][1],lStatPer[[p]][2]))
      
      if (y1==y2) {
        if (y1 %in% dimnames(stat)$year){
          for(a in ages){
            x[a,names(lStatPer)[p]]  <- mean(stat[a,y1,,,,ITS]@.Data, na.rm=T)
          }
        }
      } else {
        for(a in ages){
          x[a,names(lStatPer)[p]]  <- mean(stat[a,y12,,,,ITS]@.Data, na.rm=T)  
        }
      }
    }
    
    
    
  } else {
    
    x <- FLQuant(NA, dimnames=list(age=c("min",paste(100*percs,"%",sep=""),"max","mean","sd","var","CV"), year=names(lStatPer)))
    
    for (p in 1:length(lStatPer)) {
      
      y1 <- ac(lStatPer[[p]][1])
      y2 <- ac(lStatPer[[p]][2])
      y12 <- ac(seq(lStatPer[[p]][1],lStatPer[[p]][2]))
      
      if ((y1 %in% dimnames(stat)$year) & (y1 %in% dimnames(stat)$year)) {
        if (y1==y2) {
          x["var",names(lStatPer)[p]]  <- var(stat[,y1,,,,ITS]@.Data, na.rm=T)
          x["sd",names(lStatPer)[p]]   <- sd(stat[,y1,,,,ITS]@.Data, na.rm=T)
          x["mean",names(lStatPer)[p]] <- mean(stat[,y1,,,,ITS]@.Data, na.rm=T)
          x["CV",names(lStatPer)[p]] <- sd(stat[,y1,,,,ITS]@.Data, na.rm=T)/mean(stat[,y1,,,,ITS]@.Data, na.rm=T)
          #x["min",names(lStatPer)[p]]  <- min(stat[,y1,,,,ITS]@.Data, na.rm=T)
          if (any(!is.na(stat[,y1,,,,ITS]@.Data))){x["min",names(lStatPer)[p]] <- min(stat[,y1,,,,ITS]@.Data, na.rm=T)}else{x["min",names(lStatPer)[p]]<-NA}
          #x["max",names(lStatPer)[p]]  <- max(stat[,y1,,,,ITS]@.Data, na.rm=T)
          if (any(!is.na(stat[,y1,,,,ITS]@.Data))){x["max",names(lStatPer)[p]] <- max(stat[,y1,,,,ITS]@.Data, na.rm=T)}else{x["max",names(lStatPer)[p]]<-NA}
          x[2:(length(percs)+1),names(lStatPer)[p]]   <- quantile(stat[,y1,,,,ITS]@.Data,probs=percs,na.rm=T)
        } else {
          #browser()
          #x["var",names(lStatPer)[p]]  <- var(stat[,y12,,,,ITS]@.Data, na.rm=T)
          #x["sd",names(lStatPer)[p]]   <- sd(stat[,y12,,,,ITS]@.Data, na.rm=T)
          #x["mean",names(lStatPer)[p]] <- mean(stat[,y12,,,,ITS]@.Data, na.rm=T)
          #x["CV",names(lStatPer)[p]] <- sd(stat[,y12,,,,ITS]@.Data, na.rm=T)/mean(stat[,y12,,,,ITS]@.Data, na.rm=T)
          #x["min",names(lStatPer)[p]]  <- min(stat[,y12,,,,ITS]@.Data, na.rm=T)
          #x["max",names(lStatPer)[p]]  <- max(stat[,y12,,,,ITS]@.Data, na.rm=T)
          #x[2:(length(percs)+1),names(lStatPer)[p]]   <- quantile(stat[,y12,,,,ITS]@.Data,probs=percs,na.rm=T)
          x["var",names(lStatPer)[p]] <- var(apply(stat[,y12,,,,ITS]@.Data,FUN='mean',6,na.rm=TRUE), na.rm=T)
          x["sd",names(lStatPer)[p]] <- sd(apply(stat[,y12,,,,ITS]@.Data,FUN='mean',6,na.rm=TRUE), na.rm=T)
          x["mean",names(lStatPer)[p]] <- mean(apply(stat[,y12,,,,ITS]@.Data,FUN='mean',6,na.rm=TRUE), na.rm=T)
          x["CV",names(lStatPer)[p]] <- sd(apply(stat[,y12,,,,ITS]@.Data,FUN='mean',6,na.rm=TRUE), na.rm=T)/mean(apply(stat[,y12,,,,ITS]@.Data,FUN='mean',6,na.rm=TRUE), na.rm=T)
          if (any(!is.na(stat[,y12,,,,ITS]@.Data))){x["min",names(lStatPer)[p]] <- min(apply(stat[,y12,,,,ITS]@.Data,FUN='mean',6,na.rm=TRUE), na.rm=T)}else{x["min",names(lStatPer)[p]]<-NA}
          #x["max",names(lStatPer)[p]] <- max(apply(stat[,y12,,,,ITS]@.Data,FUN='mean',6,na.rm=TRUE), na.rm=T)
          if (any(!is.na(stat[,y12,,,,ITS]@.Data))){x["max",names(lStatPer)[p]] <- max(apply(stat[,y12,,,,ITS]@.Data,FUN='mean',6,na.rm=TRUE), na.rm=T)}else{x["max",names(lStatPer)[p]]<-NA}
          x[2:(length(percs)+1),names(lStatPer)[p]] <- quantile(apply(stat[,y12,,,,ITS]@.Data,FUN='mean',6,na.rm=TRUE),probs=percs,na.rm=TRUE)
        }
      }
      
    }
    
  }
  
  return(x)
  
}

fStatRisk <- function(SSB, RP, lStatPer) {
  
  #compute the risk of the supplied SSB falling below the supplied RP for each of the years/periods
  #given in the lStatPer list. For periods the maximum of the annual risks is returned
  
  #FLQuant for results. If a single year then min,max,mean and median will all have the same value
  x <- FLQuant(NA, dimnames=list(age=c("min","max","mean","median"), year=names(lStatPer)))
  
  for (p in 1:length(lStatPer)){

    nits <- length(SSB[1,1])
    y1 <- ac(lStatPer[[p]][1])
    y2 <- ac(lStatPer[[p]][2])
    y12 <- ac(seq(lStatPer[[p]][1],lStatPer[[p]][2]))

    if (y1==y2){
      if(y1 %in% dimnames(SSB)$year){
        x["min",names(lStatPer)[p]] <- x["max",names(lStatPer)[p]] <- x["mean",names(lStatPer)[p]] <- x["median",names(lStatPer)[p]] <- sum(as.numeric(SSB[,y1])<RP)/nits  
      }
    } else {
      x["min",names(lStatPer)[p]] <- min(apply(FLCore::window(SSB,y1,y2),2,function(x){return(sum(x<RP)/nits)}))
      x["max",names(lStatPer)[p]] <- max(apply(FLCore::window(SSB,y1,y2),2,function(x){return(sum(x<RP)/nits)}))
      x["mean",names(lStatPer)[p]] <- mean(apply(FLCore::window(SSB,y1,y2),2,function(x){return(sum(x<RP)/nits)}))
      x["median",names(lStatPer)[p]] <- quantile(as.numeric(apply(FLCore::window(SSB,y1,y2),2,function(x){return(sum(x<RP)/nits)})),probs=0.5,na.rm=T)
    }
    
  }
  
  return(x)
  
}

#extinction rate
fExtYear <- function(x,yrs) {
  return(rev(as.numeric(yrs))[min(which(rev(as.vector(x))==TRUE))] + 1)
}

fStatExtinct <- function(SSB, depletion=0.01, firstYear) {
  
  #compute the proportion of interations extinct by year
  #an extinct stock is one where the SSB has fallen below the limit proportion (depletion param - default 1%)
  #of the minimum observed SSB and remains so until the final year
  
  #browser()
  
  #all.years <- dimnames(SSB)$year
  x <- FLQuant(NA, dimnames=list(age=c("min","max","mean","median"), year=names(lStatPer)))
  #ret <- rep(0,length(all.years))
  #names(ret) <- all.years

  #depletion <- 1.5
  #minimum observed SSB & depletion gives the level below which extinction is assumed
  ext.SSB <- SSB<FLQuant(rep(as.vector(depletion * apply(SSB[,ac(seq(dims(SSB)$minyear,firstYear)),,,,],3:6,min,na.rm=TRUE)),each=dim(SSB)[2]), dimnames=dimnames(SSB))

  for (p in 1:length(lStatPer)){
    
    nits <- length(SSB[1,1])
    y1 <- ac(lStatPer[[p]][1])
    y2 <- ac(lStatPer[[p]][2])
    y12 <- ac(seq(lStatPer[[p]][1],lStatPer[[p]][2]))
    
    if (y1==y2){
      if(y1 %in% dimnames(SSB)$year){
        x["min",names(lStatPer)[p]] <- x["max",names(lStatPer)[p]] <- x["mean",names(lStatPer)[p]] <- x["median",names(lStatPer)[p]] <- sum(as.numeric(ext.SSB[,y1,,,,]))/nits  
      }
    } else {
      numy <- an(y2)-an(y1)+1
      t <- ext.SSB[,ac(seq(y1,y2)),,,,]
      x["min",names(lStatPer)[p]] <- min(apply(t,2,function(x){return(sum(t)/(numy*nits))}))
      x["max",names(lStatPer)[p]] <- max(apply(t,2,function(x){return(sum(t)/(numy*nits))}))
      x["mean",names(lStatPer)[p]] <- mean(apply(t,2,function(x){return(sum(t)/(numy*nits))}))
      x["median",names(lStatPer)[p]] <- quantile(apply(t,2,function(x){return(sum(t)/(numy*nits))}),probs=0.5,na.rm=T)
    }
    
  }
  
  
  #prob.ext <- apply(ext.SSB,2:5,sum,na.rm=TRUE)/1000
  
  
  # #not interested in the years before the simulation start
  # stat2 <- window(SSB,as.character(firstYear),as.character(dims(SSB)$maxyear))
  # #just the iterations that are extinct in the final year
  # stat3 <- stat2[,,,,,as.vector(stat2[,as.character(dims(stat2)$maxyear),,,,]<depletion*stat2[,as.character(dims(stat2)$minyear),,,,])]
  # 
  # #identify the years above/below extinction level
  # if(dims(stat3)$iter > 0){
  #   stat4 <- stat3 > depletion*as.vector(stat3[,as.character(dims(stat3)$minyear),,,,])
  #   #cumulative sum divided by total number of iterations give proportion of iterations extinct by year
  #   stat5 <- table(as.vector(apply(stat4,6,fExtYear,yrs=dimnames(stat4)$year)))/dims(stat2)$iter
  #   ret[names(stat5)] <- stat5
  # }
  # ret[] <- cumsum(ret)

  return(x)
  
}

# fTabulateStats <- function(sims, op.dir){
#   
#   require(knitr)
#   require(kableExtra)
#   
# }

fStatRecovery <- function(SSB, RP, percs = c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975)) {
  
  #time taken for (first) recovery above the biomass given by RP
  
  #exclude any iterations not initially below Blim 
  t <- SSB[,,,,,SSB[,1]<Blim]
  
  t <- as.numeric(FLCore::yearSums(SSB.true<RP))
  
  t <- t[t>0]   #some iterations never fall below Blim
  
  return(1)
}



