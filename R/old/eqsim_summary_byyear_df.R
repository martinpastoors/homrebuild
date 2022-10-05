#eqsim_summary_df
# Explort a single run into a df

# run <- c("WHOM_SS_OM2.2_MP2.1_1000_20")
# lStatPer = lStatPer
# Blim <- OM$refPts$Blim
# Fbarrange=c(1,10)

fsummary_byyear_df <- function(run, ftgt, simRuns,
                        Res.dir, Plot.dir, 
                        lStatPer, Blim, 
                        simYears, xlab, 
                        Fbarrange=c(1,10)){

  #produces a dataframe
  #comparing the supplied performance statistic for ST,MT and LT for each runName
  #grouped by runName
  
  require(tidyverse)
  
  dfAll <- data.frame(stringsAsFactors = FALSE)

  #get the data
  # run <- "OM2.2_MP2.1_1000_20"
  # cat(run,"\n")
  
  #load the output of the simulation and the summary statistics
  # load(file = file.path(Res.dir,run,paste0(run,"_SimRuns.RData")))
  # load(file = file.path(Res.dir,run,paste0(run,"_eqSim_Stats.RData")))
  
  years <-
    data.frame(period="CU", year=seq(an(lStatPer$CU[1]),
                                     an(lStatPer$CU[2])),
               stringsAsFactors = FALSE) %>% 
    bind_rows(data.frame(period="ST", year=seq(an(lStatPer$ST[1]),
                                               an(lStatPer$ST[2])),
                         stringsAsFactors = FALSE)) %>% 
    bind_rows(data.frame(period="MT", year=seq(an(lStatPer$MT[1]),
                                               an(lStatPer$MT[2])),
                         stringsAsFactors = FALSE)) %>% 
    bind_rows(data.frame(period="LT", year=seq(an(lStatPer$LT[1]),
                                               an(lStatPer$LT[2])),
                         stringsAsFactors = FALSE)) 
  
  #units
  units <- data.frame(
    PerfStat = c("TAC","SSB","CW","Harvest","Rec"),
    unit = c("tonnes", "tonnes","tonnes","1/year","Millions"),
    stringsAsFactors = FALSE
  )
  
  # ftgt <- 0.1
  # for (ftgt in an(names(SimRuns))){
    
    # cat("Ftgt: ", ftgt, "\n")
    
    #simulation op
    t <- SimRuns[[ac(ftgt)]]

    #simulation stats
    # t2 <- lOp$stats[[ac(ftgt)]]
    # t2 <- lStats$stats[[ac(ftgt)]]
    
    # TAC
    dimnames(t$TAC)$year <- simYears
    
    #catch numbers
    Cnum <- t[["C"]]
    #catch weights
    Cwgt <- t[["catW"]]
    #catch weight (tons)
    CW <- apply(Cnum*Cwgt,2:3,sum)/1e3
    dimnames(CW)$year <- simYears
    t$CW <- CW

    #F management
    dimnames(t$Fmgmt)$year <- simYears
    
    #harvest
    Harvest <- t[["F"]]
    dimnames(Harvest)$year <- simYears
    Harvest <- apply(Harvest[ac(Fbarrange[1]:Fbarrange[2]),,],2:3, mean)
    t$Harvest <- Harvest
    # dimnames(Harvest)
    
    #abundance
    Abd <- t[["N"]]
    #stock weights
    SW <- t[["stkW"]]
    #maturity
    Mat <- t[["Mat"]]
    
    #recruitment
    Rec <- t[["N"]][1,,]
    dimnames(Rec)$year <- simYears
    t$Rec <- Rec
    
    #SSB (Mt)
    SSB <- apply(Abd*SW*Mat,2:3,sum)/1e3
    dimnames(SSB)$year <- simYears
    t$SSB <- SSB
    

    # item <- "TAC"
    for (item in c("TAC","SSB","CW", "Harvest", "Fmgmt","Rec")) {
      
      # cat(item, "\n")
      
      x <- 
        as.data.frame(t(t[[(item)]])) %>% 
        pivot_longer(cols=1:nrow(t[[(item)]]), 
                     names_to = "year", 
                     values_to = "value") %>% 
        rownames_to_column(var="iter") %>% 
        mutate(
          PerfStat = item,
          year     = an(year),
          RunRef = run,
          Label = xlab,
          Ftgt = ftgt) %>% 
        left_join(years, by="year") %>% 
        left_join(units, by="PerfStat") 
        

      dfAll <- dplyr::bind_rows(dfAll,x)
      
    } # end of for loop (item)

  # } # end of for loop: ftgt
    
  dfAll <-
    dfAll %>% 
    tidyr::separate(RunRef, 
                    into=c("stock","assess", "OM","MP","niters","nyrs"), 
                    sep="_",
                    remove = FALSE) 
    
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

