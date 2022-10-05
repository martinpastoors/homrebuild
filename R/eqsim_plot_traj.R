#fPlotTraj <- function(sim, plot.dir, fileFormat="png", wth=7, hght=7, lStatPer){
fPlotTraj <- function(sim, plot.dir, lStatPer){
 
  require(gplots)
  require(scales)
  require(officer)
  require(devEMF)
  require(dplyr)
  
  cols <- rich.colors(10)
  
  #plot the stock trajectories
  
  #sim - list of simulation statistics
  #plot.dir - location for output
  
  allstats <- sim$stats
  cat(names(allstats),"\n")
  RP <- sim$OM$refPts
  runName <- sim$runName
  
  #plots for each f in FScan
  for (f in names(allstats)) {
    
    dat2Plot <- allstats[[f]]
    
    #year limits
    obsYears <- dat2Plot[["obsYears"]]
    simYears <- dat2Plot[["simYears"]]
    #simulation period up to end of long term period
    simYear2LT <- simYears[simYears<=lStatPer$LT[2]]
    allYears <- seq(min(c(obsYears,simYears)),max(c(obsYears,simYears)))
    
    #replace dec point with underscore for filename
    fv.rep <- stringr::str_replace(f,"\\.","_")

    for (what in c("SSB","SSBratio","Catch","FBar","Fratio","Rec","pBlim","pBpa","pExt","TAC","IAV")) {

      #if (what %in% c("SSB","pExt","IAV")) {browser()}
      
      #worms and statistics
      stats <- dat2Plot[[what]][["val"]]
      if (!is.null(dat2Plot[[what]][["worm"]])) {
        worms <- dat2Plot[[what]][["worm"]]
        nworms <- dim(worms)[6]
      }

      #set up plot limits
      xlims <- range(allYears)
      if (what %in% c("SSB")){
        ylims <- c(0,1e6*ceiling(max(max(worms,na.rm=TRUE)/1e6,max(stats["95%",],na.rm=TRUE)/1e6)))
      } else if (what %in% c("Catch")) {
        ylims <- c(0,ceiling(max(max(1e5*worms,na.rm=TRUE),max(1e5*stats["95%",],na.rm=TRUE)))/1e5)
      } else if (what %in% c("FBar")) {
        ylims <- c(0,ceiling(max(max(10*worms,na.rm=TRUE),max(10*stats["95%",],na.rm=TRUE)))/10)
      } else if (what %in% c("IAV")) {
        xlims <- range(simYear2LT)
        ylims <- c(0,0.3)
      } else if (what %in% c("SSBratio","Fratio")) {
        xlims <- range(simYear2LT)
        ylims <- c(0.5,2)
      } else if (what %in% c("pBlim","pBpa","pExt")) {
        xlims <- range(simYears)
        ylims <- c(0,0.4)
        #if (max(stats,na.rm=TRUE)>0) ylims[2] <- ceiling(max(10*stats,na.rm=TRUE))/10
      } else {
        ylims <- c(0,1.1*ceiling(max(max(worms,na.rm=TRUE),max(stats["95%",],na.rm=TRUE))))
      }
    
      #cat(file.path(plot.dir,runName,paste0(what,"_",runName,"_",fv.rep,".",fileFormat)),"\n")
      #cat(what,runName,fv.rep,"\n")
      
      emf(file = file.path(plot.dir,paste0(what,".emf")), width = 7, height = 7)
      #Cairo(file = file.path(plot.dir,runName,paste0(what,"_",runName,"_",fv.rep,".",fileFormat)),
      #      type=fileFormat, units="in", height=hght, width=2*wth, bg="white", dpi=96, pointsize=12)
    
      #setup the plot area
      plot(allYears, rep(0,length(allYears)), 
           main = paste0(runName,", F=",f),
           type="n", lwd=2, col=cols[1], ylab=what, xlab="Year", xlim = xlims, ylim = ylims, 
           cex.lab=1.5, cex.axis=1.5, xaxs="i", yaxs="i")

      #worm lines
      if (!is.null(dat2Plot[[what]][["worm"]])){
        #2 worms for SSBratio
        if (what %in% c("SSBratio","Fratio")) {
          lines(simYear2LT,worms[,ac(simYear2LT),,,,1], lwd=1, col="black")
          points(simYear2LT,worms[,ac(simYear2LT),,,,1], pch=19, col="black")
          lines(simYear2LT,worms[,ac(simYear2LT),,,,2], lwd=1, col="black")
          points(simYear2LT,worms[,ac(simYear2LT),,,,2], pch=17, col="black")
        } else {
          for (w in seq(1,nworms)) lines(dplyr::intersect(simYears,as.integer(dimnames(worms)$year)),
                                         worms[,ac(dplyr::intersect(simYears,as.integer(dimnames(worms)$year))),,,,w], lwd=1, col="grey")  
        }
        
      }
      
      statYrs<-as.integer(intersect(dimnames(stats)$year,ac(allYears)))
      
      #5th-95th percentile limits
      if (("5%" %in% dimnames(stats)$age) & ("95%" %in% dimnames(stats)$age)) {
        polygon(c(statYrs,rev(statYrs)),
                c(stats["5%",ac(statYrs)],rev(as.numeric(stats["95%",ac(statYrs)]))), 
                col=scales::alpha(cols[length(cols)],0.5), border=NA)
        
        #outlines
        lines(statYrs,stats["5%",ac(statYrs)], lwd=2, col=cols[length(cols)], lty=3)
        lines(statYrs,stats["95%",ac(statYrs)], lwd=2, col=cols[length(cols)], lty=3)
      }
      
      #median
      if ("50%" %in% dimnames(stats)$age) {
        lines(statYrs,stats["50%",ac(statYrs)], lwd=2, col=cols[length(cols)], lty=1)
      } else if ("median" %in% dimnames(stats)$age) {
        lines(statYrs,stats["median",ac(statYrs)], lwd=2, col=cols[length(cols)], lty=1)
      } else {
        lines(statYrs[statYrs %in% names(stats)],stats,lty=1)
      }
      
      #start of projection period
      abline(v=min(simYears), lty=2)
      
      if (what=="SSB"){
        #biomass reference points
        abline(h=c(RP[["Blim"]],RP[["Bpa"]]), lty=2)
      } else if (what %in% c("pBlim","pBpa")){
        #precautionary risk limit
        abline(h=RP[["pBlim"]], lty=2)
      }

      if (what %in% c("SSBratio","Fratio")){
        abline(h=c(0.5,0.75,1,1/0.75,1/0.5),lty=2)
      }
      
      if (what=="IAV"){
        abline(h=c(-0.2,-0.1,0,0.1,0.2),lty=2)
      }
      
      if (what=="pBlim"){abline(h=0.05,lty=2,col="grey")}
      
      #shade the statistical periods
      #statistical periods
      polygon(x = c(lStatPer$ST[1],lStatPer$ST[2]+0.5,lStatPer$ST[2]+0.5,lStatPer$ST[1]), 
              y = c(ylims[1],ylims[1],ylims[2],ylims[2]), col = alpha("red",0.1), border=NA)
      polygon(x = c(lStatPer$MT[1]-0.5,lStatPer$MT[2]+0.5,lStatPer$MT[2]+0.5,lStatPer$MT[1]-0.5), 
              y = c(ylims[1],ylims[1],ylims[2],ylims[2]), col = alpha("blue",0.1), border=NA)
      polygon(x = c(lStatPer$LT[1]-0.5,lStatPer$LT[2],lStatPer$LT[2],lStatPer$LT[1]-0.5), 
              y = c(ylims[1],ylims[1],ylims[2],ylims[2]), col = alpha("green",0.1), border=NA)
 
      dev.off()

    }
    
    fGetLag1 <- function(ts){
      t <- acf(ts, plot=FALSE)
      return(t$acf[[2]])
    }
    
    for (what in c("Fdev","SSB.dev")){
    
      #error autocorrelation
      phi <- apply(dat2Plot[[what]], MARGIN=2, FUN = fGetLag1)
    
      if (sum(!is.nan(phi))>0) {
        
        
        emf(file = file.path(plot.dir,paste0(what,".emf")), width = 7, height = 7)
        #Cairo(file = file.path(plot.dir,runName,paste0(what,"_",runName,"_",fv.rep,".",fileFormat)),
        #      type=fileFormat, units="in", height=hght, width=2*wth, bg="white", dpi=96, pointsize=12)
    
        hist(phi)
        dev.off()
      }
      
    }
    
    #create word doc
    
    #graphics files
    gFiles <- list.files(path=file.path(plot.dir), pattern=".emf", full.names=TRUE)
    
    if (length(gFiles)>0){
      op <- read_docx() 
      for (fl in gFiles) {
        op <- op %>%
          body_add_img(src = fl, width = 7, height = 7) %>%
          body_add_break()
      }
      
      op <- op %>%
      print(target = file.path(plot.dir,paste0(runName,"_Ftgt_",f,".docx")))
      
      cat(paste0(runName,"_Ftgt_",f,".docx"),"\n")
      
      sapply(gFiles,file.remove)
      
    }

  } #end f loop
  
  return(1)
  
}