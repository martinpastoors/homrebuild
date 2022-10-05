fPlotSummary <- function(sim, plot.dir, fileFormat="png", wth=7, hght=7, lStatPer, FtoPlot=c(0,0.05,0.074,0.1)) {
  
  require(ggplot2)
  
  AllStats <- sim$stats
  runName <- sim$runName
  
  dfSummary <- data.frame("Ftgt"=c(),"OP"=c(),"Year"=c(),"p1"=c(),"p5"=c(),"p25"=c(),
                          "p50"=c(),"p75"=c(),"p95"=c(),"p99"=c(),"ref"=c(),
                          stringsAsFactors = FALSE)
  for (r in names(AllStats)){
    t <- AllStats[[r]]
    yrs <- t$simYears
    nyrs <- length(yrs)
    #SSB
    dfSummary <- dplyr::bind_rows(dfSummary,
                                  data.frame("Ftgt" = rep(r,nyrs),
                                             "OP" = rep("SSB",nyrs),
                                             "Year" = yrs,
                                             "p1" = as.numeric(t$SSB$val["1%",ac(yrs)])/1e6,
                                             "p2.5" = as.numeric(t$SSB$val["2.5%",ac(yrs)])/1e6,
                                             "p5" = as.numeric(t$SSB$val["5%",ac(yrs)])/1e6,
                                             "p25" = as.numeric(t$SSB$val["25%",ac(yrs)])/1e6,
                                             "p50" = as.numeric(t$SSB$val["50%",ac(yrs)])/1e6,
                                             "p75" = as.numeric(t$SSB$val["75%",ac(yrs)])/1e6,
                                             "p95" = as.numeric(t$SSB$val["95%",ac(yrs)])/1e6,
                                             "p97.5" = as.numeric(t$SSB$val["97.5%",ac(yrs)])/1e6,
                                             "p99" = as.numeric(t$SSB$val["99%",ac(yrs)])/1e6,
                                             "ref" = 0,
                                             stringsAsFactors = FALSE))
    #Catch
    dfSummary <- dplyr::bind_rows(dfSummary,
                                  data.frame("Ftgt" = rep(r,nyrs),
                                             "OP" = rep("Catch",nyrs),
                                             "Year" = yrs,
                                             "p1" = as.numeric(t$Catch$val["1%",ac(yrs)])/1e3,
                                             "p2.5" = as.numeric(t$Catch$val["2.5%",ac(yrs)])/1e3,
                                             "p5" = as.numeric(t$Catch$val["5%",ac(yrs)])/1e3,
                                             "p25" = as.numeric(t$Catch$val["25%",ac(yrs)])/1e3,
                                             "p50" = as.numeric(t$Catch$val["50%",ac(yrs)])/1e3,
                                             "p75" = as.numeric(t$Catch$val["75%",ac(yrs)])/1e3,
                                             "p95" = as.numeric(t$Catch$val["95%",ac(yrs)])/1e3,
                                             "p97.5" = as.numeric(t$Catch$val["97.5%",ac(yrs)])/1e3,
                                             "p99" = as.numeric(t$Catch$val["99%",ac(yrs)])/1e3,
                                             "ref" = NA,
                                             stringsAsFactors = FALSE))
    #FBar
    dfSummary <- dplyr::bind_rows(dfSummary,
                                  data.frame("Ftgt" = rep(r,nyrs),
                                             "OP" = rep("FBar",nyrs),
                                             "Year" = yrs,
                                             "p1" = as.numeric(t$FBar$val["1%",ac(yrs)]),
                                             "p2.5" = as.numeric(t$FBar$val["2.5%",ac(yrs)]),
                                             "p5" = as.numeric(t$FBar$val["5%",ac(yrs)]),
                                             "p50" = as.numeric(t$FBar$val["50%",ac(yrs)]),
                                             "p75" = as.numeric(t$FBar$val["75%",ac(yrs)]),
                                             "p95" = as.numeric(t$FBar$val["95%",ac(yrs)]),
                                             "p97.5" = as.numeric(t$FBar$val["97.5%",ac(yrs)]),
                                             "p99" = as.numeric(t$FBar$val["99%",ac(yrs)]),
                                             "ref" = NA,
                                             stringsAsFactors = FALSE))
    
    #IAV
    dfSummary <- dplyr::bind_rows(dfSummary,
                                  data.frame("Ftgt" = rep(r,length(intersect(dimnames(t$IAV$val["50%"])$year,ac(yrs)))),
                                             "OP" = rep("IAV",length(intersect(dimnames(t$IAV$val["50%"])$year,ac(yrs)))),
                                             "Year" = as.numeric(intersect(dimnames(t$IAV$val["50%"])$year,ac(yrs))),
                                             "p1" = as.numeric(t$IAV$val["1%",intersect(dimnames(t$IAV$val["1%"])$year,ac(yrs))]),
                                             "p2.5" = as.numeric(t$IAV$val["2.5%",intersect(dimnames(t$IAV$val["2.5%"])$year,ac(yrs))]),
                                             "p5" = as.numeric(t$IAV$val["5%",intersect(dimnames(t$IAV$val["5%"])$year,ac(yrs))]),
                                             "p25" = as.numeric(t$IAV$val["25%",intersect(dimnames(t$IAV$val["25%"])$year,ac(yrs))]),
                                             "p50" = as.numeric(t$IAV$val["50%",intersect(dimnames(t$IAV$val["50%"])$year,ac(yrs))]),
                                             "p75" = as.numeric(t$IAV$val["75%",intersect(dimnames(t$IAV$val["75%"])$year,ac(yrs))]),
                                             "p95" = as.numeric(t$IAV$val["95%",intersect(dimnames(t$IAV$val["95%"])$year,ac(yrs))]),
                                             "p97.5" = as.numeric(t$IAV$val["97.5%",intersect(dimnames(t$IAV$val["97.5%"])$year,ac(yrs))]),
                                             "p99" = as.numeric(t$IAV$val["99%",intersect(dimnames(t$IAV$val["99%"])$year,ac(yrs))]),
                                             "ref" = 0,
                                             stringsAsFactors = FALSE))
    
    #Risk type 3
    dfSummary <- dplyr::bind_rows(dfSummary,
                                  data.frame("Ftgt" = rep(r,nyrs),
                                             "OP" = rep("pBlim",nyrs),
                                             "Year" = yrs,
                                             "p1" = 100*as.numeric(t$pBlim$val["median",ac(yrs)]),
                                             "p2.5" = 100*as.numeric(t$pBlim$val["median",ac(yrs)]),
                                             "p5" = 100*as.numeric(t$pBlim$val["median",ac(yrs)]),
                                             "p25" = 100*as.numeric(t$pBlim$val["median",ac(yrs)]),
                                             "p50" = 100*as.numeric(t$pBlim$val["median",ac(yrs)]),
                                             "p75" = 100*as.numeric(t$pBlim$val["median",ac(yrs)]),
                                             "p95" = 100*as.numeric(t$pBlim$val["median",ac(yrs)]),
                                             "p97.5" = 100*as.numeric(t$pBlim$val["median",ac(yrs)]),
                                             "p99" = 100*as.numeric(t$pBlim$val["median",ac(yrs)]),
                                             "ref" = 5,
                                             stringsAsFactors = FALSE))
    
  }
  
  #create a reordered factor to get the plots in the desired layout
  dfSummary$OP2 <- factor(dfSummary$OP, levels=c("SSB","Catch","FBar","IAV","pBlim"))
  dfSummary$Ftgt2 <- factor(paste0("Ftgt = ",dfSummary$Ftgt))
  
  #label lookup
  Ylabs = c(
    SSB = "SSB (Mt)",
    Catch = "Yield (kt)",
    FBar = "FBar",
    IAV = "IAV",
    pBlim = "Blim Risk (%)"
  )
  
  p <- ggplot(subset(dfSummary,Ftgt %in% FtoPlot), aes(Year,p50))
  p <- p + geom_ribbon(aes(ymin = p1, ymax = p99), fill = "grey90")
  p <- p + geom_ribbon(aes(ymin = p5, ymax = p95), fill = "grey80")
  p <- p + geom_ribbon(aes(ymin = p25, ymax = p75), fill = "grey70")
  p <- p + geom_line(aes(Year, p99), color = "grey70", size = 0.1)
  p <- p + geom_line(aes(Year, p1), color = "grey70", size = 0.1)
  p <- p + geom_line(aes(Year, ref), color = "grey", linetype=2, size = 0.1)
  p <- p + geom_vline(xintercept = c(lStatPer$ST[1],0.5*(lStatPer$ST[2]+lStatPer$MT[1]),0.5*(lStatPer$MT[2]+lStatPer$LT[1]),lStatPer$LT[2]), color="grey", linetype=2, size=0.1)
  p <- p + geom_line(color = "firebrick", size = 1)
  p <- p + facet_grid(rows = vars(OP2), cols = vars(Ftgt2), scales="free_y", labeller = labeller(OP2 = Ylabs))
  p <- p + ggtitle(MP$desc)
  p <- p + theme()
  p <- p + theme(panel.background = element_rect(fill = "white", colour = "grey50"),
                  axis.title.y = element_blank(),
                  strip.text.x = element_text(size=12, face="bold"),
                  strip.text.y = element_text(size=12, face="bold"),
                  strip.background = element_rect(colour="red", fill="#CCCCFF"),
                  plot.title = element_text(hjust = 0.5))
   
  ggsave(file.path(plot.dir,runName,"summary.png"))

}