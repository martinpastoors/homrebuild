#annual SSB/Blim distribution
#annual whisker plots for SSB/Blim 

fAnnSSBvsBlimDist <- function(OM,MP,res.dir,plot.dir){
  
  require(ggplot2)
  require(Cairo)
  
  runName <- paste(OM$code,MP$code,sep="_")
  
  load(file = file.path(Res.dir, runName, paste0(runName,"_eqsim_Stats.Rdata")))
  
  AllStats <- lStats$stats
  
  for (ftgt in names(AllStats)){

    t <- AllStats[[ftgt]]

    dfPlot = data.frame(x1 = t$simYears, 
                        x2 = t$simYears, 
                        y1 = as.numeric(t$SSB$val['5%',ac(simYears),,,,]/OM$refPts$Blim), 
                        y2 = as.numeric(t$SSB$val['95%',ac(simYears),,,,]/OM$refPts$Blim),
                        y12 = as.numeric(t$SSB$val['50%',ac(simYears),,,,]/OM$refPts$Blim),
                        stringsAsFactors = FALSE)
    
    dfLowerWhiskers <- data.frame(x1 = t$simYears - 0.25,
                                  x2 = t$simYears + 0.25,
                                  y1 = as.numeric(t$SSB$val['5%',ac(simYears),,,,]/OM$refPts$Blim),
                                  y2 = as.numeric(t$SSB$val['5%',ac(simYears),,,,]/OM$refPts$Blim),
                                  stringsAsFactors = FALSE)

    dfUpperWhiskers <- data.frame(x1 = t$simYears - 0.25,
                                  x2 = t$simYears + 0.25,
                                  y1 = as.numeric(t$SSB$val['95%',ac(simYears),,,,]/OM$refPts$Blim),
                                  y2 = as.numeric(t$SSB$val['95%',ac(simYears),,,,]/OM$refPts$Blim),
                                  stringsAsFactors = FALSE)
    
    p <- ggplot(data = dfPlot) + 
      geom_segment(mapping = aes(x = x1, y = y1, xend = x2, yend = y2)) +
      geom_segment(data = dfLowerWhiskers, mapping = aes(x = x1, y = y1, xend = x2, yend = y2)) +
      geom_segment(data = dfUpperWhiskers, mapping = aes(x = x1, y = y1, xend = x2, yend = y2)) +
      geom_hline(yintercept = 1, linetype = 2, col = "red") +
      geom_point(mapping = aes(x=x1, y=y12)) + 
      labs(x = "Year", y = "SSB/Blim") + 
      ggtitle(paste0(runName," F = ",ftgt)) +
      theme_bw()
    
    Cairo(width=780, height=480, file=file.path(res.dir,runName,paste0("SSBBlimRatio_",runName,"_",ftgt,".png")))
    print(p)
    dev.off()
                   
                   
  }
  
  
}