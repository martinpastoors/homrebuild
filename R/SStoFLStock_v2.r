
`SStoFLStock` <-
  function (dirSSoutput, stockname, fbar_amin, fbar_amax, nforeYear=0) 
  {
    
    output <- SS_output(dir = dirSSoutput, 
                        #model = "ss",
                        repfile = "Report.sso", 
                        compfile = "CompReport.sso", 
                        covarfile = "covar.sso", 
                        ncols = 200, 
                        forecast = TRUE, 
                        warn = TRUE,
                        covar = FALSE, 
                        checkcor = TRUE, 
                        cormax = 0.95, 
                        cormin = 0.01,
                        printhighcor = 10, 
                        printlowcor = 10, 
                        verbose = FALSE,
                        printstats = FALSE, 
                        hidewarn = TRUE, 
                        NoCompOK = FALSE,
                        aalmaxbinrange=0)
    
    
    ###############################################################
    ####  CREATING FLSTOCK FROM EXTERNAL FILES   ##################
    #### This is a MOCK FLSTOCK object, most of the slots have 1111 and the contents of other slots have been suitably modified!!! 
    #### The intention here is to have an FLStock object which contains the required information to fit the Stock-Recruitment relationship in EqSim -- this FLStock object should not be used for any other purpose!!!
    
    data <- FLStock(object=FLQuant(quant='age'))
    
    # summary(data)
    
    yearfirst <- output$startyr 
    yearlast <- output$endyr + nforeYear 
    nyears <- yearlast - yearfirst + 1
    
    agefirst <- 0
    agelast <- output$accuage
    nages <- agelast - agefirst + 1 
    
    # Name:
    data@name <- stockname
    
    # Age minimum:
    data@range[[1]] <- agefirst
    # Age maximum:
    data@range[[2]] <- agelast
    # Plus group:
    data@range[[3]] <- agelast
    # First year:
    data@range[[4]] <- yearfirst
    # Last year:
    data@range[[5]] <- yearlast
    
    # Min age for Fbar:
    data@range[[6]] <- fbar_amin
    # Max age for Fbar:
    data@range[[7]] <- fbar_amax
    
    
    # summary(data)
    
    
    # PC: changed "Gender" to "Sex" as per new definition in report file.
    ## STOCK NUMBERS AT AGE:
    auxi <-  output$natage[(output$natage$"Beg/Mid" == 'B')&(output$natage$"Sex" == 1),]
    if(nforeYear==0){
      natage <- as.vector(auxi[auxi$Era=="TIME",names(auxi)==as.character(0)]); for(i in 1:agelast){natage <- c(natage, as.vector(auxi[auxi$Era=="TIME",names(auxi)==as.character(i)]))}
    } else {
      natage <- as.vector(auxi[auxi$Era=="TIME" | auxi$Era=="FORE", names(auxi)==as.character(0)]); for(i in 1:agelast){natage <- c(natage, as.vector(auxi[auxi$Era=="TIME" | auxi$Era=="FORE",names(auxi)==as.character(i)]))}}
    
    
    if(output$nsexes==2){
      auxi <-  output$natage[(output$natage$"Beg/Mid" == 'B')&(output$natage$"Sex" == 2),]
      if(nforeYear==0){
        auxi2 <- as.vector(auxi[auxi$Era=="TIME",names(auxi)==as.character(0)]); for(i in 1:(nages-1)){auxi2 <- c(auxi2, as.vector(auxi[auxi$Era=="TIME",names(auxi)==as.character(i)]))}
      } else {
        auxi2 <- as.vector(auxi[auxi$Era=="TIME" | auxi$Era=="FORE",names(auxi)==as.character(0)]); for(i in 1:(nages-1)){auxi2 <- c(auxi2, as.vector(auxi[auxi$Era=="TIME" | auxi$Era=="FORE",names(auxi)==as.character(i)]))}
      }
      natage <- natage + auxi2
    }
    
    natage <- t(matrix(natage, ncol=nages))
    
    data@stock.n <-FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", natage, units = '10^3')
    
    # STOCK WEIGHT-AT-AGE: 
    data@stock.wt <-FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", rep(as.numeric(unlist(t(output$wtatage[(output$wtatage$Fleet==0)&(output$wtatage$Sex==1),-c(1:6)]))), nyears), units = 'kg')
    
    # NATURAL MORTALITY: 
    data@m <- FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", rep(as.numeric(output$MGparmAdj$NatM_uniform_Fem_GP_1), nyears), units = 'm')
    
    # PROPORTION OF NATURAL MORTALITY BEFORE FISHING: 
    # Since in SS3 we're assuming spawning is on January 1st, I set this value to 0
    data@m.spwn <- FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", rep(0,nages*nyears), units = 'prop')
    
    # MATURITY-AT-AGE: 
    data@mat <-FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", rep(output$endgrowth$Age_Mat[1:nages],nyears), units = 'prop')
    #data@mat <-FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", rep(output$endgrowth[,19][1:nages]/output$endgrowth$Wt_Beg[1:nages],nyears), units = 'prop')
    
    # F-AT-AGE: 
    # This is the matrix of F-at-age, for each year and age (complicated calculation, see below!!!)
    
    # PC: Changed "selex$year" to "selex$Yr" and "selex$gender" to "selex$sex" as per changes in report
    ## First calculate selectivity-at-length, for each fishing fleet (allowing for possible changes in selectivity over time):
    
    selex <- output$sizeselex
    selex <- selex[(selex$Fleet<= output$nfishfleets)&(selex$Factor=="Lsel")&(selex$Yr >= yearfirst)&(selex$Sex==1), ]
    # dim(selex)
    # names(selex)
    
    # Calculating selectivity-at-length for each fishing fleet:
    selatlen <- array(1, dim=c(nyears,output$nfishfleets,ncol(selex)-5)); dim(selatlen)
    for (fl in 1:output$nfishfleets){
      ## Selectivity for years from first assessment year:
      selc <- selex[selex$Fleet==fl ,]
      
      for(j in 1:nrow(selc)){   
        if(j==1){
          indexyear <- 1
          selatlen[indexyear,fl,] <- as.numeric(selc[1,-(1:5)])
        }else{
          if(selc$Yr[j] <= yearlast){
            diff <- selc$Yr[j] - selc$Yr[j-1]
            # Fill in years in between with same selectivity-at-length:
            if(diff>1){ for(y in indexyear+c(1:(diff-1))){selatlen[y,fl,]<-selatlen[indexyear,fl,]} }
            # Year with new selectivity-at-length:
            indexyear <- indexyear + diff
            selatlen[indexyear,fl,] <- as.numeric(selc[j,-(1:5)])
          }
        }
      } 
      if(indexyear < dim(selatlen)[1]){ for(y in (indexyear+1):(dim(selatlen)[1])){selatlen[y,fl,]<-selatlen[indexyear,fl,]} }   
    }   
    
    # dim(selatlen) # Year, Fleet, Length class
    
    
    ## Now derive selectity-at-age from selectivity-at-length and p(length|age) in the middle of the season:
    
    #dim(output$ALK) = Length, Age, for each season (in sequence over seasons): Start of season (Female, then Male) / Middle of season (Female, then Male)
    plenatage <-  output$ALK[dim(output$ALK)[1]:1,,1+output$nsexes]  
    # dim(plenatage)  # Length, Age
    # Check that, for each age, the sum over the lengths is equal to 1:
    # apply(plenatage, 2, sum)
    
    # Selectivity-at-age:
    selatage <- array( apply(plenatage, 2, function(x){apply(selatlen,c(1,2),function(y){sum(y*x)}) } ), dim=c(nyears, output$nfishfleets, nages) )    
    # dim(selatage) # Year, Fleet, Age
    
    # Now calculate F-at-age, multiplying selectivity-at-age by apical F 
    #(all calculations are done by fishing fleet, and, at the end, we sum over all fishing fleets to get an F-at-age for the entire stock):
    if(nforeYear==0){
      apicalf <- as.matrix(output$timeseries[output$timeseries$Era == "TIME",substr(names(output$timeseries),1,3)=="F:_"])
    } else {apicalf <- as.matrix(output$timeseries[output$timeseries$Era == "TIME" | output$timeseries$Era == "FORE",substr(names(output$timeseries),1,3)=="F:_"])
    }
    
    # dim(apicalf) # Year, Fleet
    
    fatage <- t( apply(selatage*as.vector(apicalf), c(1,3), sum) )
    # dim(fatage) # Age, Year
    
    data@harvest <- FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", fatage, units = 'f')
    # data@harvest
    
    # Harvest rate before spawning: 
    # Since SS3 computes SSB at start of season (January 1st), I assume this is 0
    data@harvest.spwn <- FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", rep(0,nages*nyears), units = 'prop' )
    
  
    ## CATCH NUMBERS-AT-AGE (I CALCULATE FITTED VALUES):
    
    matage <- array(data@m,dim=dim(fatage)); dim(matage)
    
    # Catch number at age: 
    data@catch.n <- FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'),quant = "age", natage*(1-exp(-fatage-matage))*fatage/(fatage+matage), units = '10^3')
    
    # TOTAL OBSERVED LANDINGS (input data to stock assessment):
    if(nforeYear==0){
      obslandings <- apply( array(output$timeseries[output$timeseries$Era == "TIME",substring(names(output$timeseries),1,7)=="obs_cat"], dim=c(output$nfishfleets,nyears)), 2, sum)
    }else{
      obslandings <- apply( array(output$timeseries[output$timeseries$Era == "TIME" | output$timeseries$Era == "FORE",substring(names(output$timeseries),1,7)=="obs_cat"], dim=c(output$nfishfleets,nyears)), 2, sum)
    }
    
    
    data@landings <- FLQuant(dimnames = list(age = 'all', year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", obslandings , units = 't')
    
    ## Are there any discards???
    if(nforeYear==0){
      fitlandings <- matrix(output$timeseries[output$timeseries$Era=="TIME",substr(names(output$timeseries),1,9)=="retain(B)"],ncol=output$nfishfleets)
      fitcatch <- output$timeseries[output$timeseries$Era=="TIME",substr(names(output$timeseries),1,7)=="dead(B)"]
      fitdiscards <- apply( fitcatch - fitlandings, 1, sum )
    }else{  
      fitlandings <- matrix(output$timeseries[output$timeseries$Era=="TIME" | output$timeseries$Era=="FORE", substr(names(output$timeseries),1,9)=="retain(B)"],ncol=output$nfishfleets)
      fitcatch <- output$timeseries[output$timeseries$Era=="TIME" | output$timeseries$Era=="FORE", substr(names(output$timeseries),1,7)=="dead(B)"]
      fitdiscards <- apply( fitcatch - fitlandings, 1, sum )
    }
    
    
    ## If there are no discards, catch=landings:
    if( max(fitdiscards) == 0){ 
      
      data@discards <- FLQuant(dimnames = list(age = 'all', year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", rep(0,nages*nyears) , units = 't')
      
      data@discards.n <- FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'),quant = "age", rep(0, nages*nyears), units = '10^3')
      
    }
    
    data@catch <- data@landings + data@discards
    
    # Landings number at age (assuming no discards; a discard rate at age could be assumed at this point): 
    data@landings.n <- data@catch.n - data@discards.n
    
    
    # PC: Added "(output$wtatage$Yr<=yearlast)" and changed fleet to Fleet, Gender to Sex.
    # CATCH WEIGHT AT AGE: 
    if( output$nfishfleets == 1){
      wtatage_commercial <- t(output$wtatage[(output$wtatage$Fleet==1)&(output$wtatage$Sex==1)&(output$wtatage$Yr<=yearlast),-c(1:6)])
    }
    # If there was more than 1 fishing fleet in the model, wtatage_commercial would need to be a weighted average of the weights in each fleet taking into account the proportion of the stock catch-at-age that the model estimates that comes from each of the fleets (this proportion would be annually varying) 
    
    data@catch.wt <- FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'),quant = "age", wtatage_commercial, units = 'kg')
    
    # Landings weight at age: 
    data@landings.wt <- data@catch.wt
    
    # Discards weight at age:
    data@discards.wt <- data@catch.wt
    
    # Total stock: just 1111s as DUMMY VALUES (EqSim ignores this slot)
    data@stock <- FLQuant(dimnames = list(age = 'all', year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", rep(1111,nyears), units = 't') 
    
    # summary(data)
    
    # save(data,file=paste(dirSSoutput,"/",stockname,"_SSresults.rdata",sep=""))
    
    
    #################  PLOTS  ##############################
    
    # pdf(file=paste(dirSSoutput,"/",stockname,"_SSresults.pdf",sep=""),height=7,width=10.5)
    
    
    # dim(fatage)
    #Selection-at-age for the stock as a whole:
    # selatage <-  apply(fatage, 2, function(x){x/max(x)}); dim(selatage) 
    
    # years <- yearfirst:yearlast
    # ages <- agefirst:agelast
    
    # par(mfrow=c(1,2))
    # plot(years, selatage[1,],type="l",ylim=c(0,1),xlab="",ylab="",lwd=2, main="Selection at age")
    # for(j in 2:nrow(selatage)){lines(years, selatage[j,], col=min(j,8), lwd=2)}
    # for(j in 2:nrow(selatage)){lines(years, selatage[j,], col=min(j,8), lwd=2)}
    # for(j in 1:nrow(selatage)){points(years, selatage[j,], col=min(j,8), pch=20)}
    
    # plot(ages, selatage[,1],type="l",ylim=c(0,1),xlab="",ylab="",lwd=2, main="Selection at age")
    # for(j in 2:ncol(selatage)){lines(ages, selatage[,j], col=j, lwd=2)}
    # for(j in 1:ncol(selatage)){points(ages, selatage[,j], col=j, pch=20)}
    
    # natage <- t( matrix(as.vector(data@stock.n),nrow=nages) )
    # dim(natage)
    
    # swatage <- t( matrix(as.vector(data@stock.wt),nrow=nages) ) 
    # dim(swatage)
    
    # cwatage <- t( matrix(as.vector(data@catch.wt),nrow=nages) ) 
    # dim(cwatage)
    
    # lwatage <- t( matrix(as.vector(data@landings.wt),nrow=nages) ) 
    # dim(lwatage)
    
    # dwatage <- t( matrix(as.vector(data@discards.wt),nrow=nages) ) 
    # dim(dwatage)
    
    # matage <- t( matrix(as.vector(data@mat),nrow=nages) ) 
    # dim(matage)
    
    # M <- t( matrix(as.vector(data@m),nrow=nages) ) 
    # dim(M)
    
    # ssb <- apply(natage*swatage*matage, 1, sum)
    
    # minssb <- min(ssb)
    # minssb_minus <- min(ssb)/1.4
    # minssb_plus <- min(ssb)*1.4
    
    # dim(fatage)
    # fbar <- apply(fatage[1+(fbar_amin:fbar_amax),], 2, mean)
    
    # recage <- data@range[names(data@range)=="min"]; recage
    
    # par(mfrow=c(2,3))
    
    # plot(ages, M[1,],type="l",ylim=c(0,max(M)),xlab="",ylab="",lwd=2, main="M-at-age in all years",col=1)
    # for(j in 2:nyears){lines(ages, M[j,], col=j, lwd=2)}
    # for(j in 1:nyears){points(ages, M[j,], col=j, pch=20)}
    
    # plot(ages, matage[1,],type="l",ylim=c(0,max(matage)),xlab="",ylab="",lwd=2, main="Maturity-at-age in all years",col=1)
    # for(j in 2:nyears){lines(ages, matage[j,], col=j, lwd=2)}
    # for(j in 1:nyears){points(ages, matage[j,], col=j, pch=20)}
    
    # plot(ages, swatage[1,],type="l",ylim=c(0,max(swatage)),xlab="",ylab="",lwd=2, main="Stock WAA in all years",col=1)
    # for(j in 2:nyears){lines(ages, swatage[j,], col=j, lwd=2)}
    # for(j in 1:nyears){points(ages, swatage[j,], col=j, pch=20)}
    
    # plot(ages, cwatage[1,],type="l",ylim=c(0,max(cwatage)),xlab="",ylab="",lwd=2, main="Catch WAA in all years",col=1)
    # for(j in 2:nyears){lines(ages, cwatage[j,], col=j, lwd=2)}
    # for(j in 1:nyears){points(ages, cwatage[j,], col=j, pch=20)}
    
    # plot(ages, lwatage[1,],type="l",ylim=c(0,max(lwatage)),xlab="",ylab="",lwd=2, main="Landings WAA in all years",col=1)
    # for(j in 2:nyears){lines(ages, lwatage[j,], col=j, lwd=2)}
    # for(j in 1:nyears){points(ages, lwatage[j,], col=j, pch=20)}
    
    # plot(ages, dwatage[1,],type="l",ylim=c(0,max(dwatage)),xlab="",ylab="",lwd=2, main="Discards WAA in all years",col=1)
    # for(j in 2:nyears){lines(ages, dwatage[j,], col=j, lwd=2)}
    # for(j in 1:nyears){points(ages, dwatage[j,], col=j, pch=20)}
    
    
    # par(mfrow=c(2,2))
    # plot(years,ssb,type="l",lwd=2,main="SSB",ylab="",xlab="",ylim=c(0,max(ssb)))
    # points(years,ssb,pch=20)
    # abline(h=minssb, col=2)
    # abline(h=minssb_plus, col=3)
    # abline(h=minssb_minus, col=4)
    
    # plot(years,natage[,1],type="l",lwd=2,main="Rec",ylab="",xlab="",ylim=c(0,max(natage[,1])))
    # points(years,natage[,1],pch=20)
    
    # plot(years, fbar, type="l", main=paste("F(",fbar_amin,"-",fbar_amax,")",sep=""),ylab="",xlab="",ylim=c(0,max(fbar)))
    # points(years,fbar,pch=20)
    
    # if(recage>0){
    #   plot(ssb[-(length(ssb)+1-c(1:recage))],natage[-c(1:recage),1],pch=20,main=paste("(SSB_y,Rec_y+",recage,")",sep=""),xlab="",ylab="",xlim=c(0,max(ssb)))
    # }else{
    #   plot(ssb,natage[,1],pch=20,main="(SSB_y,Rec_y)",xlab="",ylab="",xlim=c(0,max(ssb)))
    # }
    
    # ssbplot <- ssb; recplot <- natage[,1]
    # if(recage>0){ssbplot <- ssbplot[-(length(ssb)+1-c(1:recage))]; recplot <- recplot[-c(1:recage)]  }
    
    # par(mfrow=c(1,1))                                                                
    # plot(ssbplot,recplot,pch=20,cex=1.5,xlab="",ylab="",main=c(paste("(SSB_y,Rec_y+",recage,")",sep=""),"red start-90, green 91-00", "dark blue 01-10, clear blue 11-15"), xlim=c(0,max(ssbplot)), ylim=c(0,max(recplot)) )
    # colour=2; start <- 1991
    # points(ssbplot[years<start],recplot[years<start],col=colour,pch=20,cex=1.5)
    # colour <- colour+1
    # points(ssbplot[(years>=start)&(years<(start+10))],recplot[(years>=start)&(years<(start+10))],col=colour,pch=20,cex=1.5)
    # colour <- colour+1; start=start+10
    # points(ssbplot[(years>=start)&(years<(start+10))],recplot[(years>=start)&(years<(start+10))],col=colour,pch=20,cex=1.5)
    # colour <- colour+1; start=start+10
    # points(ssbplot[(years>=start)&(years<(start+10))],recplot[(years>=start)&(years<(start+10))],col=colour,pch=20,cex=1.5)
    # abline(v=minssb, col=2)
    # abline(v=minssb_plus, col=3)
    # abline(v=minssb_minus, col=4)
    
    
    # par(mfrow=c(1,1))
    # ssbplot <- ssbplot[-1]; recplot <- recplot[-1]; years <- years[-1]                                                                
    # plot(ssbplot,recplot,pch=20,cex=1.5,xlab="",ylab="",main=c(paste("(SSB_y,Rec_y+",recage,"); excluding 1982",sep=""),"red start-90, green 91-00", "dark blue 01-10, clear blue 11-15"), xlim=c(0,max(ssbplot)), ylim=c(0,max(recplot)) )
    # colour=2; start <- 1991
    # points(ssbplot[years<start],recplot[years<start],col=colour,pch=20,cex=1.5)
    # colour <- colour+1
    # points(ssbplot[(years>=start)&(years<(start+10))],recplot[(years>=start)&(years<(start+10))],col=colour,pch=20,cex=1.5)
    # colour <- colour+1; start=start+10
    # points(ssbplot[(years>=start)&(years<(start+10))],recplot[(years>=start)&(years<(start+10))],col=colour,pch=20,cex=1.5)
    # colour <- colour+1; start=start+10
    # points(ssbplot[(years>=start)&(years<(start+10))],recplot[(years>=start)&(years<(start+10))],col=colour,pch=20,cex=1.5)
    # abline(v=minssb, col=2)
    # abline(v=minssb_plus, col=3)
    # abline(v=minssb_minus, col=4)
    
    
    # dev.off()
    
    return(data)
  }




# `SS3toFLIndices` <-
#   function (dirSS3output, stockname, fbar_amin, fbar_amax) 
#   {
#     
#     
#     output <- SS_output(dir = dirSS3output, model = "ss3",repfile = "Report.sso", compfile = "CompReport.sso", covarfile = "covar.sso", ncols = 200, forecast = TRUE, warn = TRUE,covar = FALSE, checkcor = TRUE, cormax = 0.95, cormin = 0.01,printhighcor = 10, printlowcor = 10, verbose = TRUE,printstats = TRUE, hidewarn = FALSE, NoCompOK = FALSE,aalmaxbinrange=0)
#     
#     
#     ###############################################################
#     ####  CREATING FLSTOCK FROM EXTERNAL FILES   ##################
#     #### This is a MOCK FLSTOCK object, most of the slots have 1111 and the contents of other slots have been suitably modified!!! 
#     #### The intention here is to have an FLStock object which contains the required information to fit the Stock-Recruitment relationship in EqSim -- this FLStock object should not be used for any other purpose!!!
#     
#     cpue              <- output$cpue
#     ncpue             <- unique(output$cpue$Fleet)
# 
#       if(is.null(dim(cpue))){
#       cat("skipping index plots: no CPUE data in this model\n")
#       return()
#     }
#     
#     for(i in ncpue) {
#       
#       data <- FLIndex(object=FLQuant(quant='age'))
# 
#       summary(data)
# 
#       yearfirst <- min(output$cpue[which(output$cpue$Fleet==i), "Yr"])
#       yearlast <- max(output$cpue[which(output$cpue$Fleet==i), "Yr"])
#       nyears <- yearlast - yearfirst + 1
# 
#       # Name:
#       data@name <- unique(output$cpue[which(output$cpue$Fleet==i), "FleetName"])
# 
#       # Age minimum:
#       data@range[[1]] <- agefirst
#       # Age maximum:
#       data@range[[2]] <- agelast
#       # Plus group:
#       data@range[[3]] <- agelast
#       # First year:
#       data@range[[4]] <- yearfirst
#       # Last year:
#       data@range[[5]] <- yearlast
# 
#       # Min age for Fbar:
#       data@range[[6]] <- fbar_amin
#       # Max age for Fbar:
#       data@range[[7]] <- fbar_amax
# 
# 
#       summary(data)
# 
# 
# ## TOTAL BIO/ABUNDANCE:
# auxi <-  output$natage[(output$natage$"Beg/Mid" == 'B')&(output$natage$"Gender" == 1),]
# natage <- as.vector(auxi[auxi$Era=="TIME",names(auxi)==as.character(0)]); for(i in 1:agelast){natage <- c(natage, as.vector(auxi[auxi$Era=="TIME",names(auxi)==as.character(i)]))}
# if(output$nsexes==2){
#   auxi <-  output$natage[(output$natage$"Beg/Mid" == 'B')&(output$natage$"Gender" == 2),]
#   auxi2 <- as.vector(auxi[auxi$Era=="TIME",names(auxi)==as.character(0)]); for(i in 1:(nages-1)){auxi2 <- c(auxi2, as.vector(auxi[auxi$Era=="TIME",names(auxi)==as.character(i)]))}
#   natage <- natage + auxi2
# }
# natage <- t(matrix(natage, ncol=nages))
#  
# data@stock.n <-FLQuant(dimnames = list(age = agefirst:agelast, year = yearfirst:yearlast, unit = 'unique', season = 'all', area = 'unique'), quant = "age", natage, units = '10^3')
# datas <- FLIndices()
# 
# 
# }
