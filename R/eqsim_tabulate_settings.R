fGetSettings <- function(stats, 
                         lStatPer, SimRuns, FLStockfile, FLStockSimfile,
                         OM, MP, niters, nyr){
  
  # dfSimRuns <- SimRuns
  # dfStats   <- lStats
  # flstockfile <- FLStockfile
  # flstocksimfile <- FLStockSimfile
  
  tData <- 
    # OM
    data.frame(unlist(OM, use.names=TRUE), stringsAsFactors = FALSE) %>% 
    tibble::rownames_to_column() %>% 
    setNames(c("desc","value")) %>%
    mutate(class="OM") %>% 
    
    # MP
    bind_rows(
      data.frame(unlist(MP, use.names=TRUE), stringsAsFactors = FALSE) %>% 
        tibble::rownames_to_column() %>% 
        setNames(c("desc","value")) %>% 
        mutate(class="MP")
    ) %>% 
    
    # Other
    bind_rows(
      data.frame(desc=c("niters","nyr","CU","ST","MT","LT","flstock","flstock_sim"),
                 value=c(ac(niters), 
                         ac(nyr),
                         paste(ac(lStatPer[["CU"]]), collapse="-"),
                         paste(ac(lStatPer[["ST"]]), collapse="-"),
                         paste(ac(lStatPer[["MT"]]), collapse="-"),
                         paste(ac(lStatPer[["LT"]]), collapse="-"),
                         FLStockfile,
                         FLStockSimfile),
                 class="OTHER",
                 stringsAsFactors = FALSE)
    ) %>% 
    
    dplyr::select(class, desc, value)
  
  return(tData)
}

fMakeSettingsTable <- function(dfSettings){
  
  myft <- 
    flextable::flextable(data = dfSettings) %>% 
    flextable::theme_vanilla() %>%
    bold(part = "header") %>%
    autofit()
  
  return(myft)
}

