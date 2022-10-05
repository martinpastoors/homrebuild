fGetStatAsDF <- function(all,stat){
  #return the requested statistic as a dataframe
  #all as a list with names corresponding to the target fishing mortality of the simulation
  #stat is one of SSB, Catch, FBar, TAC, IAV, Rec, pBlim, pBpa, pExt
  #all stats (in the age dimension of the FLQuant object are included)
  
  #empty df
  dfStats <- data.frame("Ftgt"=c(),"OM"=c(),"MP"=c(),"Per"=c(),"Qty"=c(),"Stat"=c(),"Val"=c(),stringsAsFactors = FALSE)
  
  for (f in names(all)){
    
    #t <- AllStats[[f]]
    t <- all[[f]]
    Ftgt <- f
    OM <- t$OM$code
    MP <- t$MP$code
    yrs <- ac(c(t$simYears,'ST','MT','LT'))

    if (stat=="SSB"){q <- t$SSB$val}
    else if (stat=="Catch") {q <- t$Catch$val}
    else if (stat=="FBar") {q <- t$FBar$val}
    else if (stat=="pBlim") {q <- t$pBlim$val}
    else if (stat=="pBpa") {q <- t$pBpa$val}
    else if (stat=="pExt") {q <- t$pExt$val}
    else if (stat=="IAV") {q <- t$IAV$val}
    else if (stat=="precBpa") {q <- t$precBpa$val}
    else if (stat=="precBlim") {q <- t$precBlim$val}
    else {stop("Invalid statistic")}
    

    if (stat %in% c("precBpa","precBlim")) {
      
      q.yrs <- names(q)
      
      dfStats <- dplyr::bind_rows(dfStats,
                                  data.frame("Ftgt" = rep(Ftgt,length(q.yrs)),
                                             "OM" = rep(OM,length(q.yrs)),
                                             "MP" = rep(MP,length(q.yrs)),
                                             "Per" = q.yrs,
                                             "Qty" = rep(stat,length(q.yrs)),
                                             "Stat" = rep("Val",length(q.yrs)),
                                             "Val" = q,
                                             stringsAsFactors = FALSE))
    } else {
    
      st <- dimnames(q)$age
      q.yrs <- intersect(yrs,dimnames(q)$year)
      
      for (s in st){
        
        st1 <- as.numeric(q[s,q.yrs,,,,])
        
        dfStats <- dplyr::bind_rows(dfStats,
                                    data.frame("Ftgt" = rep(Ftgt,length(q.yrs)),
                                               "OM" = rep(OM,length(q.yrs)),
                                               "MP" = rep(MP,length(q.yrs)),
                                               "Per" = q.yrs,
                                               "Qty" = rep(stat,length(q.yrs)),
                                               "Stat" = rep(s,length(q.yrs)),
                                               "Val" = st1,
                                               stringsAsFactors = FALSE))
      }
    }
  }
  
  return(dfStats)
  
}


fMakeTable <- function(dfStats,q,s,om,mp,Title="",LowLimit=NA,HighLimit=NA,divFactor=1,dp=0,pers=c("ST","MT","LT")){

  if (any(is.na(pers))) {
    tData <- dfStats %>%
      dplyr::filter(Qty==q & Stat==s & OM==om & MP==mp) %>%
      tidyr::pivot_wider(names_from="Ftgt",values_from = "Val",values_fn=list("Val"=function(x){round(x/divFactor,dp)}))
  } else {
    tData <- dfStats %>%
      dplyr::filter(Per %in% pers & Qty==q & Stat==s & OM==om & MP==mp) %>%
      tidyr::pivot_wider(names_from="Ftgt",values_from = "Val",values_fn=list("Val"=function(x){round(x/divFactor,dp)}))
  }
  
  myft <- flextable::flextable(
    data = tData,
    col_keys = c("Per",names(table(dfStats$Ftgt)))
  )
  
  myft <- flextable::theme_vanilla(myft) %>%
    add_header_lines(Title) %>%
    set_header_labels(Per = "Period/Ftgt") %>%
    bold(part = "header") %>%
    autofit()
  
  #highlight cells
  if (!is.na(LowLimit)) for (c in names(table(dfStats$Ftgt))){myft <- color(myft,i = tData[,c]<LowLimit, j = c, color="red")}
  if (!is.na(HighLimit)) for (c in names(table(dfStats$Ftgt))){myft <- color(myft,i = tData[,c]>HighLimit, j = c, color="red")}
  
  return(myft)
  
}

fTabulateStats <- function(sim, setting, plot.dir){
  
  require(dplyr)
  require(flextable)
  require(officer)
  require(magrittr)
  
  allstats <- sim$stats
  cat(names(allstats),"\n")
  RP <- sim$OM$refPts
  runName <- sim$runName
  
  #reformat list of stats to a dataframe
  #1 row per statistic
  dfStats <- data.frame("Ftgt"=c(),"OM"=c(),"MP"=c(),"Per"=c(),"Qty"=c(),"Stat"=c(),"Val"=c(),stringsAsFactors = FALSE)
  dfStats <- dplyr::bind_rows(dfStats,fGetStatAsDF(all=AllStats,stat="SSB"))
  dfStats <- dplyr::bind_rows(dfStats,fGetStatAsDF(all=AllStats,stat="Catch"))
  dfStats <- dplyr::bind_rows(dfStats,fGetStatAsDF(all=AllStats,stat="IAV"))
  dfStats <- dplyr::bind_rows(dfStats,fGetStatAsDF(all=AllStats,stat="FBar"))
  dfStats <- dplyr::bind_rows(dfStats,fGetStatAsDF(all=AllStats,stat="pBlim"))
  dfStats <- dplyr::bind_rows(dfStats,fGetStatAsDF(all=AllStats,stat="pExt"))
  dfStats <- dplyr::bind_rows(dfStats,fGetStatAsDF(all=AllStats,stat="pBpa"))
  dfStats <- dplyr::bind_rows(dfStats,fGetStatAsDF(all=AllStats,stat="precBpa"))
  dfStats <- dplyr::bind_rows(dfStats,fGetStatAsDF(all=AllStats,stat="precBlim"))
  
  #create a word doc
  my_doc <- officer::read_docx()

  #add the SSB table
  my_doc <- my_doc %>%
    
    body_add_par(runName, style = "table title") %>%
    body_add_par("", style = "Normal") %>% 
    
    body_add_par(paste0("Date: ", format(Sys.time(), "%d-%b-%Y %H.%M")), style = "Normal") %>%
    body_add_par("", style = "Normal") %>% 
    
    body_add_par("SSB (Mt)", style = "Normal") %>%
    body_add_par("", style = "Normal") %>% 
    body_add_flextable(value = fMakeTable(dfStats,"SSB","25%",sim$OM$code,sim$MP$code,"25%",LowLimit=1,divFactor=1e6,dp=2)) %>%
    body_add_flextable(value = fMakeTable(dfStats,"SSB","50%",sim$OM$code,sim$MP$code,"50%",LowLimit=1,divFactor=1e6,dp=2)) %>%
    body_add_flextable(value = fMakeTable(dfStats,"SSB","75%",sim$OM$code,sim$MP$code,"75%",LowLimit=1,divFactor=1e6,dp=2)) %>%
    body_add_par("", style = "Normal") %>% 
    body_add_par("Yield (kt)", style = "Normal") %>%
    body_add_par("", style = "Normal") %>% 
    body_add_flextable(fMakeTable(dfStats,"Catch","25%",sim$OM$code,sim$MP$code,"25%",LowLimit=100,divFactor=1e3,dp=0)) %>%
    body_add_flextable(fMakeTable(dfStats,"Catch","50%",sim$OM$code,sim$MP$code,"Median",LowLimit=100,divFactor=1e3,dp=0)) %>%
    body_add_flextable(fMakeTable(dfStats,"Catch","75%",sim$OM$code,sim$MP$code,"75%",LowLimit=100,divFactor=1e3,dp=0)) %>%
    
    body_add_break() %>%
    body_add_par("IAV", style = "Normal") %>%
    body_add_par("", style = "Normal") %>% 
    body_add_flextable(fMakeTable(dfStats,"IAV","25%",sim$OM$code,sim$MP$code,"25%",HighLimit=50,divFactor=1,dp=2)) %>%
    body_add_flextable(fMakeTable(dfStats,"IAV","50%",sim$OM$code,sim$MP$code,"Median",HighLimit=50,divFactor=1,dp=2)) %>%
    body_add_flextable(fMakeTable(dfStats,"IAV","75%",sim$OM$code,sim$MP$code,"75%",HighLimit=50,divFactor=1,dp=2)) %>%
    body_add_par("", style = "Normal") %>% 
    body_add_par("FBar", style = "Normal") %>%
    body_add_par("", style = "Normal") %>% 
    body_add_flextable(fMakeTable(dfStats,"FBar","25%",sim$OM$code,sim$MP$code,"25%",HighLimit=0.5,divFactor=1,dp=3)) %>%
    body_add_flextable(fMakeTable(dfStats,"FBar","50%",sim$OM$code,sim$MP$code,"Median",HighLimit=0.5,divFactor=1,dp=3)) %>%
    body_add_flextable(fMakeTable(dfStats,"FBar","75%",sim$OM$code,sim$MP$code,"75%",HighLimit=0.5,divFactor=1,dp=3)) %>%
    
    body_add_break() %>%
    body_add_par("Risk (Type3) to Blim (%)", style = "Normal") %>%
    body_add_par("", style = "Normal") %>% 
    body_add_flextable(fMakeTable(dfStats,"pBlim","mean",sim$OM$code,sim$MP$code,"mean",HighLimit=5,divFactor=0.01,dp=1)) %>%
    body_add_par("", style = "Normal") %>% 
    body_add_par("Risk (Type3) to Bpa (%)", style = "Normal") %>%
    body_add_par("", style = "Normal") %>% 
    body_add_flextable(fMakeTable(dfStats,"pBpa","mean",sim$OM$code,sim$MP$code,"mean",HighLimit=5,divFactor=0.01,dp=1)) %>%  
    body_add_par("", style = "Normal") %>% 
    body_add_par("Extinction Risk (%)", style = "Normal") %>%
    body_add_par("", style = "Normal") %>% 
    body_add_flextable(fMakeTable(dfStats,"pExt","mean",sim$OM$code,sim$MP$code,"mean",HighLimit=5,divFactor=0.01,dp=1)) %>%
    body_add_break() %>%
    body_add_par("Proportion Recovered Above Blim", style = "Normal") %>%
    body_add_par("", style = "Normal") %>% 
    body_add_flextable(fMakeTable(dfStats,"precBlim","Val",sim$OM$code,sim$MP$code,"Val",LowLimit=50,divFactor=0.01,dp=1,pers=NA)) %>%
    body_add_break() %>%
    body_add_par("Proportion Recovered Above Bpa", style = "Normal") %>%
    body_add_par("", style = "Normal") %>% 
    body_add_flextable(fMakeTable(dfStats,"precBpa","Val",sim$OM$code,sim$MP$code,"Val",LowLimit=50,divFactor=0.01,dp=1,pers=NA)) %>%
    body_add_break() %>%
    body_add_par("Settings used", style = "Normal") %>%
    body_add_flextable(fMakeSettingsTable(setting))

  #write the file
  print(my_doc, target = file.path(plot.dir,runName,paste0(runName,"_Stats_Tables.docx")))
  
}