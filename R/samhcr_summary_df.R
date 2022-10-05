#samhcr_summary_df

# FC = FC
# ftgt = ftgt
# Settings = Settings
# FLSs = FLSs

samsummary_df <- function(FC, ftgt, Settings, FLSs){

  #produces a dataframe
  
  invisible(gc())
  
  HI <- Settings[["HI"]]          
  CU <- Settings[["CU"]]          
  ST <- Settings[["ST"]]          
  MT <- Settings[["MT"]]          
  LT <- Settings[["LT"]]          
  
  years <-
    data.frame(period="CU", year=seq(an(CU[1]),an(CU[2])),stringsAsFactors = FALSE) %>% 
    bind_rows(
      data.frame(period="ST", year=seq(an(ST[1]),an(ST[2])),stringsAsFactors = FALSE)) %>% 
    bind_rows(
      data.frame(period="MT", year=seq(an(MT[1]),an(MT[2])),stringsAsFactors = FALSE)) %>% 
    bind_rows(
      data.frame(period="LT", year=seq(an(LT[1]),an(LT[2])),stringsAsFactors = FALSE)) %>% 
    mutate(year = an(year))
  
  # set small ftgt to zero
  ftgt = ifelse(ftgt <= 0.01, 0.0, ftgt)
  

  # make data frame out of the historical iterations
  dfsim <- 
    as.data.frame(catch(FLSs))  %>%     
    mutate_if(is.factor, as.character) %>% 
    mutate(perfstat = "catch") %>% 
    mutate(period="HI", iter=as.integer(iter)) %>% 
    
    # add recruitment
    bind_rows(
      as.data.frame(rec(FLSs)) %>% 
        mutate_if(is.factor, as.character) %>% 
        mutate(perfstat = "rec") %>% 
        mutate(age = ac(age)) %>% 
        mutate(period="HI", iter=as.integer(iter))
    ) %>% 
    
    # add fbar
    bind_rows(
      as.data.frame(fbar(FLSs)) %>% 
        mutate_if(is.factor, as.character) %>% 
        mutate(perfstat = "fbar") %>% 
        mutate(period="HI", iter=as.integer(iter))
    ) %>% 

    # add ssb
    bind_rows(
      as.data.frame(ssb(FLSs)) %>% 
        mutate_if(is.factor, as.character) %>% 
        mutate(perfstat = "stock") %>% 
        mutate(period="HI", iter=as.integer(iter))
    ) 
    
  # add the simulated iterations
  # NOTE: THE VALUES IN THE ITERATIONS ARE CONSTANT OVER TIME !! ERROR ??
  # v <- "ssb"
  for (v in c("fbar","catch","ssb","rec")) {
    # y=20
    for (y in 1:length(years$year)) {

      # all iterations to dataframe
      dfsim <-
        bind_rows(
          dfsim,
          data.frame(
            data     = FC[[y]][[v]],
            iter     = 1:niters,
            perfstat = tolower(v),
            year     = as.numeric(FC[[y]]$year),
            age      = "all",
            metric   = "worm",
            stringsAsFactors = FALSE
          )  %>%
          mutate(perfstat=ifelse(perfstat=="ssb","stock",perfstat)) %>% 
          left_join(years, by="year")
        ) 
    }  # end of y loop
  } # end of v loop

  # Add historical summary to data frame
  df <- 
    data.frame(stringsAsFactors = FALSE) %>% 
    
    bind_rows(
      as.data.frame(summary(fit)) %>% 
        setNames(c("rec_median","rec_lower","rec_upper",
                   "stock_median","stock_lower","stock_upper",
                   "fbar_median","fbar_lower","fbar_upper")) %>% 
        rownames_to_column(var="year") %>% 
        mutate(year = an(year)) %>% 
        pivot_longer(names_to="var", values_to="data", rec_median:fbar_upper) %>% 
        separate(var, into=c("perfstat","metric"), sep="_") %>% 
        pivot_wider(names_from=metric, values_from=data) %>% 
        mutate(period  ="HI") 
    ) %>% 
    
    # adding historical catch data
    bind_rows(
      as.data.frame(catchtable(fit)) %>% 
        setNames(c("median","lower","upper")) %>% 
        rownames_to_column(var="year") %>% 
        mutate(year = an(year)) %>% 
        mutate(perfstat="catch") %>% 
        mutate(period  ="HI") 
    ) %>% 
    
    # adding simulated data
    bind_rows(
      as.data.frame(attr(FC,"tab")) %>% 
        rownames_to_column(var="year") %>% 
        mutate(year = an(year)) %>% 
        pivot_longer(names_to="var", values_to="data", "fbar:median":"catch:high") %>% 
        separate(var, into=c("perfstat","metric"), sep=":") %>%
        mutate(metric = case_when(
          metric == "low" ~ "lower",
          metric == "high" ~ "upper",
          TRUE             ~ metric
        )) %>% 
        mutate(perfstat = case_when(
          perfstat == "ssb"  ~ "stock",
          TRUE               ~ perfstat
        )) %>% 
        pivot_wider(names_from=metric, values_from=data) %>% 
        left_join(years, by="year")
    ) %>% 
    
    # add calculated probability of being below blim
    bind_rows(
      dfsim %>%
        filter(perfstat=="stock") %>% 
        group_by(year, period) %>% 
        summarise(nbelowblim = sum(stock<Settings[["Blim"]])) %>% 
        mutate(mean = nbelowblim / Settings[["niters"]]) %>% 
        mutate(perfstat = "pblim") %>% 
        dplyr::select(-nbelowblim)
    ) %>% 
    
    # add calculated probability of recovery above threshold
    bind_rows(
      dfsim %>%
        filter(perfstat == "stock") %>% 
        filter(year >= an(CU[1])) %>% 
        group_by(iter) %>% 
        arrange(iter, year) %>% 
        mutate(v1 = lag(data, n=1),
               v2 = lag(data, n=2)) %>% 
        mutate(precblim = ifelse(v2 >= Settings[["Blim"]] & 
                                 v1 >= Settings[["Blim"]] & 
                                 data >= Settings[["Blim"]], TRUE, FALSE),
               precbpa  = ifelse(v2 >= Settings[["Bpa"]] & 
                                 v1 >= Settings[["Bpa"]] & 
                                 data >= Settings[["Bpa"]], TRUE, FALSE)) %>% 
        filter(!is.na(v1), !is.na(v2)) %>% 
        group_by(year) %>% 
        summarize(
          precblim = sum(precblim, na.rm=TRUE)/n(),
          precbpa  = sum(precbpa, na.rm=TRUE)/n()
        ) %>% 
        pivot_longer(names_to="perfstat", values_to="mean",precblim:precbpa) 
    ) %>% 
    
    # add first year rebuilt to reference point
    bind_rows(
      dfsim %>%
        filter(perfstat == "stock") %>% 
        filter(year >= an(CU[1])) %>% 
        group_by(iter) %>% 
        arrange(iter, year) %>% 
        mutate(v1 = lag(data, n=1),
               v2 = lag(data, n=2)) %>% 
        mutate(precblim = ifelse(v2 >= Settings[["Blim"]] & 
                                   v1 >= Settings[["Blim"]] & 
                                   data >= Settings[["Blim"]], TRUE, FALSE),
               precbpa  = ifelse(v2 >= Settings[["Bpa"]] & 
                                   v1 >= Settings[["Bpa"]] & 
                                   data >= Settings[["Bpa"]], TRUE, FALSE)) %>% 
        filter(!is.na(v1), !is.na(v2)) %>% 
        group_by(year) %>% 
        summarize(
          precblim = sum(precblim, na.rm=TRUE)/n(),
          precbpa  = sum(precbpa, na.rm=TRUE)/n()
        ) %>% 
        pivot_longer(names_to="perfstat", values_to="mean",precblim:precbpa) %>% 
        group_by(perfstat) %>% 
        filter(mean >= 0.5) %>% 
        filter(year == min(year)) %>% 
        dplyr::select(-mean) %>% 
        rename(mean=year) %>% 
        mutate(perfstat = gsub("recov","firstyearrebuildto", perfstat))
    ) %>% 
    
    mutate(iter = as.numeric(NA)) %>% 
    
    # add the worms
    bind_rows(
      dfsim %>% 
        filter(iter <= Settings[["numworm"]])
    ) %>% 
    
    # add descriptors
    mutate(
      iter = as.character(iter),
      assess = Settings[["assess"]]      ,
      assessyear = Settings[["assessyear"]]  ,
      method = Settings[["method"]]      ,
      stock = Settings[["stock"]]       ,
      om = Settings[["om"]]          ,
      mp = Settings[["mp"]]          ,
      runname = Settings[["runName"]]     ,
      blim = Settings[["Blim"]]         ,
      bpa = Settings[["Bpa"]]         ,
      msybtrig = Settings[["MSYBtrigger"]] ,
      fmsy = Settings[["Fmsy"]]        ,
      flow = Settings[["Flow"]]        ,
      nyr = Settings[["nyr"]]         ,
      niters = Settings[["niters"]]      ,
      numworm = Settings[["numworm"]]     ,
      rw = Settings[["RW"]]          ,
      rdist = Settings[["Rdist"]]       ,
      f.rw = Settings[["F.RW"]]        ,
      sr = Settings[["SR"]]          ,
      minobsyear = Settings[["minObsYear"]]  ,
      maxobsyear = Settings[["maxObsYear"]]  ,
      ystart = Settings[["yStart"]]      ,
      yend = Settings[["yEnd"]],
      ftgt = ftgt
    )
  
  # df %>% filter(period=="HI") %>% View()
  
  # df %>% 
  #   ggplot(aes(x=year,y=median)) +
  #   geom_ribbon(data=filter(df, period=="HI"),
  #               aes(ymin=lower, ymax=upper), fill="gray", alpha=0.4) +
  #   geom_ribbon(data=filter(df, !period=="HI"),
  #               aes(ymin=lower, ymax=upper), fill="blue", alpha=0.4) +
  #   geom_line() +
  #   facet_wrap(~perfstat, scales="free_y")
  
  df # return the data frame
  
} # end of function


