# ================================================================================================================
# 03 EqSim summarize
# 
# Summarize results of EqSim simulations
#
# 06/07/2020 tested on 1000 iters of SAM assessment
# ================================================================================================================

# rm(list=ls())
gc()

# library(tidyverse)

#basic display setting
# niters <- 1000
# maxyr <- 2040

stats.dir <- file.path(Res.dir, "Stats")

file.list <- list.files(path=stats.dir, pattern="_Stats.Rdata", full.names=TRUE)

df <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(file.list)) {
  print(file.list[i])
  df <- bind_rows(df, loadRData(file.list[i])[["df"]])
}
# NEEDS TO BE DONE IN SIMULATION CODE
df$method <- "EQSIM"
#df <- df %>%  mutate(method = ifelse(is.na(method),"EQSIM", method))
df <- df %>%  mutate(period = ifelse(is.na(period) & year < (an(assessyear)), "HI",period)) 
# df <- df %>%  mutate(metric = ifelse(is.na(metric) & !is.na(iter), "worm",metric)) 
# df <- df %>%  mutate(perfstat = ifelse(perfstat=="ssb", "stock",perfstat)) 
df <- df %>%  mutate(ftgt = ac(ftgt)) 
df <- df %>% 
  mutate(mean = ifelse(perfstat %in% c("firstyearrebuildtoblim","firstyearrebuildtobpa") & 
                      (!is.na(data)), 
                      data, mean)) %>% 
  mutate(mean = ifelse(is.infinite(mean),NA, mean))

t <- 
  df %>% 
  filter(perfstat=="stock") %>% 
  group_by(stock, assess, assessyear, method, om, mp, iter) %>% 
  arrange(stock, assess, assessyear, method, om, mp, iter,year) %>% 
  mutate(v1 = lag(data, n=1),
         v2 = lag(data, n=2)) %>% 
  mutate(recovblim = ifelse(v2 >= Blim & v1 >= Blim & data >= Blim, TRUE, FALSE),
         recovbpa  = ifelse(v2 >= Bpa & v1 >= Bpa & data >= Bpa, TRUE, FALSE)) %>% 
  filter(!is.na(v1), !is.na(v2)) %>% 
  group_by(year) %>% 
  summarize(
    recovblim = sum(recovblim, na.rm=TRUE)/n(),
    recovbpa  = sum(recovbpa, na.rm=TRUE)/n()
  ) %>% 
  
df %>% 
  ungroup() %>% 
  filter(mp=="MP5.23", assessyear==2019, method=="EQSIM") %>% 
  filter(perfstat %in% c("precblim", "precbpa","recovblim","recovbpa")) %>% 
  
  ggplot(aes(x=year, y=mean, group=perfstat)) +
  theme_publication() +
  geom_point(aes(colour=perfstat)) +
  geom_line(aes(colour=perfstat)) +
  facet_grid(om~ftgt)




rebuiltThreshold <- 0.5


# meta data of simulations
sim_meta <-
  df %>% 
  dplyr::select(stock, assess, assessyear, method, om, mp, niters, nyrs, ftgt) %>% 
  distinct() %>% 
  group_by(stock, assess, assessyear, method, om, mp, niters, nyrs) %>% 
  summarise(
    ftgt     = paste(ftgt, collapse=",")
  ) %>% 

  left_join(
    df %>% 
      dplyr::select(stock, assess, assessyear, method, om, mp, niters, nyrs, perfstat) %>% 
      distinct() %>% 
      group_by(stock, assess, assessyear, om, mp, method, niters, nyrs) %>% 
      summarise(
        perfstat     = paste(perfstat, collapse=",")
      )    
  )



# summary of simulations
sim_summary <-
  df %>% 
  group_by(stock, assess, assessyear, method, om, mp, niters, nyrs, ftgt, perfstat) %>% 
  mutate(data = ifelse(is.na(data), mean, data)) %>% 
  summarise(
    min     = min(data, na.rm=TRUE),
    mean    = mean(data, na.rm=TRUE),
    max     = max(data, na.rm=TRUE)
  ) 

sim_meta %>% 
  filter(grepl("OM2\\.2", om)) %>% 
  View()


sim_summary %>% 
  ungroup() %>% 
  filter(perfstat=="firstyearrebuildtoblim",
         grepl("MP5.23", mp)) %>% 
  mutate(scenario = paste(method, om, mp)) %>% 
  mutate(ftgt = an(ac(ftgt))) %>% 
  
  ggplot(aes(x=ftgt, y=mean, colour=scenario)) +
  theme_publication() +
  # geom_point() +
  geom_jitter(width = 0.00, height = 0.1) +
  geom_line(aes(colour=scenario)) +
  scale_y_continuous(limits=c(2020,2040),breaks=seq(2020,2040,5)) +
  facet_grid(assessyear~assess)

sim_summary %>% 
  ungroup() %>% 
  filter(perfstat=="precbpa",
         grepl("MP5.00|MP5.10|MP5.20", mp),
         assessyear == 2019) %>% 
  mutate(scenario = paste(method, om)) %>% 
  mutate(ftgt = an(ac(ftgt))) %>% 
  
  ggplot(aes(x=ftgt, y=mean, colour=scenario)) +
  theme_publication() +
  geom_point() +
  # geom_jitter(width = 0.1, height = 0.1) +
  geom_line(aes(colour=scenario)) +
  # scale_y_continuous(limits=c(2020,2040),breaks=seq(2020,2040,5)) +
  facet_grid(mp~assess)


# mystock      =  "WHOM"
# myassess     = "SAM"
# myassessyear = c("2019","2020")
# myom         = c("OM2.4","OM2.5")
# myniters     = "1000"
# mymp         = c("MP5.23")
# mynyrs       = "23"
# myftgt       = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15)
# myperfstat   = "stock"
# mycolour     = "blue"
# myyintercept = 834000
# myvalue      = "median"
# myfacets     = c("assessyear","ftgt")
# myperfstat   = "pblim"
# myvalue      = "mean"

# mystock      =  "WHOM"
# myassess     = c("SAM")
# myassessyear = c("2019")
# mymethod     = c("EQSIM","SAMHCR")
# myom         = c("OM2.4","no_OM")
# myniters     = "1000"
# mymp         = c("MP5.10")
# mynyrs       = "23"
# myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15)
# myperfstat   = "stock"
# mycolour     = "blue"
# myyintercept =  611814
# myvalue      = "median"
# myfacets     = c("method","ftgt")
# myfirstyear  = 2000
# mylastyear   = as.numeric(NA)

# plot function
plotvar <- function(mystock      =  "WHOM",
                    myassess     = "SAM",
                    myassessyear = c("2019","2020"),
                    mymethod  = "EQSIM",
                    myom         = c("OM2.4","OM2.5"),
                    myniters     = "1000",
                    mymp         = c("MP5.23"),
                    mynyrs       = "23",
                    myftgt       = ac(c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15)),
                    myperfstat   = "stock",
                    mycolour     = "blue",
                    myyintercept = 834000,
                    myvalue      = "median",
                    myfacets     = c("assessyear","ftgt"),
                    myfirstyear  = as.numeric(NA),
                    mylastyear   = as.numeric(NA)) {
  
  
  # history
  h <-
    df %>% 
    filter(period     %in% c("HI")) %>% 
    filter(perfstat   %in% myperfstat) %>% 
    filter(assess     %in% myassess) %>% 
    filter(assessyear %in% myassessyear) %>% 
    filter(method     %in% mymethod) %>% 
    filter(om         %in% myom) %>% 
    filter(mp         %in% mymp) %>% 
    # filter(nyrs       %in% mynyrs) %>% 
    filter(ftgt       %in% ac(myftgt)) %>% 
    
    {if(!is.na(myfirstyear)) filter(., year >= myfirstyear) else (.)} %>%    
    {if(!is.na(mylastyear))  filter(., year <= mylastyear) else (.)} %>%   
    
    mutate(age = as.numeric(age)) 

  # future
  d <- 
    df %>%
    filter(period     %in% c("CU","ST","MT",'LT')) %>% 
    filter(perfstat   %in% myperfstat) %>% 
    filter(stock      %in% mystock) %>% 
    filter(assess     %in% myassess) %>% 
    filter(assessyear %in% myassessyear) %>% 
    filter(method  %in% mymethod) %>% 
    filter(om         %in% myom) %>% 
    filter(mp         %in% mymp) %>%
    # filter(nyrs       %in% mynyrs) %>% 
    filter(ftgt       %in% ac(myftgt)) %>% 
    
    {if(!is.na(myfirstyear)) filter(., year >= myfirstyear) else (.)} %>%    
    {if(!is.na(mylastyear))  filter(., year <= mylastyear) else (.)} %>%   
    
    mutate(age  = as.numeric(age)) 
    
  # periods
  p <-
    bind_rows(h, d) %>% 
    distinct(assessyear, period, year) %>% 
    
    {if(!is.na(myfirstyear)) filter(., year >= myfirstyear) else (.)} %>%    
    {if(!is.na(mylastyear))  filter(., year <= mylastyear) else (.)} %>%   
    
    group_by(assessyear, period) %>% 
    summarise(
      minyear = min(year),
      maxyear = max(year)
    )
  
  # title
  t <- paste(
    toupper(mystock),
    paste(unique(myassess), collapse="-"),
    paste(unique(myassessyear), collapse="-"),
    paste(unique(mymethod), collapse="-"),
    paste(unique(myom), collapse="-"),
    paste(unique(myniters), collapse="-"),
    paste(unique(mymp), collapse="-"),
    paste(unique(mynyrs), collapse="-"),
    toupper(myperfstat),
    paste(c(myfacets[2],myfacets[1]), collapse="-"),
    paste(c(max(min(h$year), myfirstyear, na.rm=TRUE),
            min(max(d$year), mylastyear, na.rm=TRUE)), 
          collapse="-"),
    sep="_"
  )
  
  # h %>% filter(!is.na(iter), !is.na(data)) %>% View()
  
  myfig <- 

    ggplot(d, aes(x=year, y=get(myvalue), group=mp)) +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
    theme( panel.grid.major.x = element_blank()) +
    theme(legend.position = "none") +
    
    # historical ribbon
    geom_ribbon(data=filter(h, !is.na(get(myvalue))),
                aes(ymin=lower, ymax=upper, group=iter), alpha=0.4, fill="gray") +
    
    # historical worms
    geom_line(data=filter(h, metric=="worm"),
              aes(x=year, y=data, group=iter), colour="darkgray", size=0.5) +
    
    # historical median
    geom_line(data=filter(h, !is.na(get(myvalue))), 
              aes(x=year,y=get(myvalue),group=iter), colour="black", size=1) +
    
    # future ribbon
    geom_ribbon(data=filter(d, !is.na(get(myvalue))),
                aes(ymin=lower, ymax=upper, group=iter), alpha=0.4, fill=mycolour) +
    
    # future worms
    geom_line(data=filter(d, metric=="worm"),
              aes(x=year, y=data, group=iter), colour=mycolour, size=0.5) +
    
    # future median
    geom_line(data=filter(d, !is.na(get(myvalue))),
              aes(x=year,y=get(myvalue),group=iter), colour=mycolour, size=1) +
    
    # hline
    {if(!all(is.na(myyintercept))) geom_hline(yintercept=myyintercept, linetype="dashed")}  +
    
    # vertical lines (periods)
    geom_vline(data=filter(p, period=="CU"),
               aes(xintercept=minyear), linetype="dotted") +
    geom_vline(data=filter(p, period=="ST"),
               aes(xintercept=minyear), linetype="dotted") +
    geom_vline(data=filter(p, period=="MT"),
               aes(xintercept=minyear), linetype="dotted") +
    geom_vline(data=filter(p, period=="LT"),
               aes(xintercept=minyear), linetype="dotted") +
    geom_vline(data=filter(p, period=="LT"),
               aes(xintercept=maxyear), linetype="dotted") +
    
    expand_limits(y=0) +
    labs(x="", y=myperfstat, title=t) +
    facet_grid(get(myfacets[1]) ~ get(myfacets[2])) 
    
  print(myfig)
  
  ggsave(file = file.path(Res.dir,paste0(t, " summaryplot.png")),
         device="png", width = 30, height = 20, units = "cm")
  
}

#assessment/advice error investigations

#const F
#stock development
plotvar(mystock      = "WHOM", myassess     = "SS3",
        myassessyear = c("2019"), mymethod  = "EQSIM",
        myom         = c("OM2.2"), myniters     = "1000",
        mymp         = c("MP5.00","MP5.00.perf","MP5.00.def"),
        mynyrs       = "23", myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock", mycolour     = "blue",
        myyintercept = 611814, myvalue      = "median",
        myfacets     = c("mp","ftgt"), myfirstyear  = 2000)

#risk
plotvar(mystock      = "WHOM", myassess     = "SS3",
        myassessyear = c("2019"), mymethod  = "EQSIM",
        myom         = c("OM2.2"), myniters     = "1000",
        mymp         = c("MP5.00","MP5.00.perf","MP5.00.def"),
        mynyrs       = "23", myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "pblim", mycolour     = "blue",
        myyintercept = NA, myvalue      = "mean",
        myfacets     = c("mp","ftgt"), myfirstyear  = 2020)

plotvar(mystock      = "WHOM", myassess     = "SS3",
        myassessyear = c("2019"), mymethod  = "EQSIM",
        myom         = c("OM2.2"), myniters     = "1000",
        mymp         = c("MP5.23","MP5.23.perf","MP5.23.def"),
        mynyrs       = "23", myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "pblim", mycolour     = "blue",
        myyintercept = NA, myvalue      = "mean",
        myfacets     = c("mp","ftgt"), myfirstyear  = 2020)



#const F
MPLookup <- c("MP5.00" = "WGWIDE19", "MP5.00.perf" = "Perfect Knowledge", "MP5.00.def" = "Default Vals")
ggplot(data = filter(df, perfstat=="pblim" & 
                       mp %in% c("MP5.00","MP5.00.perf","MP5.00.def") & 
                       nyrs==23 & om=="OM2.2" & ftgt %in% c(0.025,0.05,0.075,0.1)) %>%
         mutate(Title = paste0(Title=paste0("Constant Ftarget = ",ftgt)), MPDesc=MPLookup[mp]),
       mapping = aes(x=year, y=100*mean, group=MPDesc, col=MPDesc)) +
         geom_line() + facet_wrap(~Title) + ylim(0,35) + xlim(2020,2040) + ylab("Risk to Blim (%)") + xlab("Year") + 
  theme(legend.position = "bottom",legend.title = element_blank()) + ggtitle("Risk Sensitivity to Assessment/Advice Error")

MPLookup <- c("MP5.10" = "WGWIDE19", "MP5.10.perf" = "Perfect Knowledge", "MP5.10.def" = "Default Vals")
ggplot(data = filter(df, perfstat=="pblim" & 
                       mp %in% c("MP5.10","MP5.10.perf","MP5.10.def") & 
                       nyrs==23 & om=="OM2.2" & ftgt %in% c(0.025,0.05,0.075,0.1)) %>%
         mutate(Title = paste0(Title=paste0("ICES AR Ftarget = ",ftgt)), MPDesc=MPLookup[mp]),
       mapping = aes(x=year, y=100*mean, group=MPDesc, col=MPDesc)) +
  geom_line() + facet_wrap(~Title) + ylim(0,35) + xlim(2020,2040) + ylab("Risk to Blim (%)") + xlab("Year") + 
  theme(legend.position = "bottom",legend.title = element_blank()) + ggtitle("Risk Sensitivity to Assessment/Advice Error")

HCRTypeLookup <- c("MP5.0" = "Constant F","MP5.1" = "ICES AR", "MP5.2" = "Double BP")
MPLookup <- c("MP5.23" = "WGWIDE19", "MP5.23.perf" = "Perfect Knowledge", "MP5.23.def" = "Default Vals","MP5.23.ext" = "Extreme Vals","MP5.23.ext0" = "Extreme, zero cor",
              "MP5.13" = "WGWIDE19", "MP5.13.perf" = "Perfect Knowledge", "MP5.13.def" = "Default Vals","MP5.13.ext" = "Extreme Vals","MP5.13.ext0" = "Extreme, zero cor",
              "MP5.03" = "WGWIDE19", "MP5.03.perf" = "Perfect Knowledge", "MP5.03.def" = "Default Vals","MP5.03.ext" = "Extreme Vals","MP5.03.ext0" = "Extreme, zero cor")
ggplot(data = filter(df, perfstat=="pblim" & 
                       mp %in% c("MP5.23","MP5.23.perf","MP5.23.def","MP5.23.ext","MP5.23.ext0",
                                 "MP5.13","MP5.13.perf","MP5.13.def","MP5.13.ext","MP5.13.ext0",
                                 "MP5.03","MP5.03.perf","MP5.03.def","MP5.03.ext","MP5.03.ext0") & 
                       nyrs==23 & om=="OM2.2" & ftgt == 0.1) %>%
         mutate(HCRType=HCRTypeLookup[substring(mp,1,5)], MPDesc=MPLookup[mp]),
       mapping = aes(x=year, y=100*mean, group=MPDesc, col=MPDesc)) +
  geom_line() + facet_wrap(~HCRType) + ylim(0,50) + xlim(2020,2040) + ylab("Risk to Blim (%)") + xlab("Year") + 
  theme(legend.position = "bottom",legend.title = element_blank()) + ggtitle("Risk Sensitivity to Assessment/Advice Error (Ftarget = 0.1)")


HCRTypeLookup <- c("MP5.0" = "Constant F","MP5.1" = "ICES AR", "MP5.2" = "Double BP")
MPLookup <- c("MP5.23" = "WGWIDE19", "MP5.23.perf" = "Perfect Knowledge", "MP5.23.def" = "Default Vals",
              "MP5.13" = "WGWIDE19", "MP5.13.perf" = "Perfect Knowledge", "MP5.13.def" = "Default Vals",
              "MP5.03" = "WGWIDE19", "MP5.03.perf" = "Perfect Knowledge", "MP5.03.def" = "Default Vals")
ggplot(data = filter(df, perfstat=="pblim" & 
                       mp %in% c("MP5.23","MP5.23.perf","MP5.23.def",
                                 "MP5.13","MP5.13.perf","MP5.13.def",
                                 "MP5.03","MP5.03.perf","MP5.03.def") & 
                       nyrs==23 & om=="OM2.2" & ftgt == 0.1) %>%
         mutate(HCRType=HCRTypeLookup[substring(mp,1,5)], MPDesc=MPLookup[mp]),
       mapping = aes(x=year, y=100*mean, group=MPDesc, col=MPDesc)) +
  geom_line() + facet_wrap(~HCRType) + ylim(0,35) + xlim(2020,2040) + ylab("Risk to Blim (%)") + xlab("Year") + 
  theme(legend.position = "bottom",legend.title = element_blank()) + ggtitle("Risk Sensitivity to Assessment/Advice Error (Ftarget = 0.1)")



HCRTypeLookup <- c("MP5.0" = "Constant F","MP5.1" = "ICES AR", "MP5.2" = "Double BP")
MPLookup <- c("MP5.20" = "WGWIDE19", "MP5.20.perf" = "Perfect Knowledge", "MP5.20.def" = "Default Vals",
              "MP5.10" = "WGWIDE19", "MP5.10.perf" = "Perfect Knowledge", "MP5.10.def" = "Default Vals",
              "MP5.00" = "WGWIDE19", "MP5.00.perf" = "Perfect Knowledge", "MP5.00.def" = "Default Vals")
ggplot(data = filter(df, perfstat=="pblim" & 
                       mp %in% c("MP5.20","MP5.20.perf","MP5.20.def",
                                 "MP5.10","MP5.10.perf","MP5.10.def",
                                 "MP5.00","MP5.00.perf","MP5.00.def") & 
                       nyrs==23 & om=="OM2.2" & ftgt == 0.1) %>%
         mutate(Title = paste0(Title=paste0("ICES AR Ftarget = ",ftgt)), HCRType=HCRTypeLookup[substring(mp,1,5)], MPDesc=MPLookup[mp]),
       mapping = aes(x=year, y=100*mean, group=MPDesc, col=MPDesc)) +
  geom_line() + facet_wrap(~HCRType) + ylim(0,50) + xlim(2020,2040) + ylab("Risk to Blim (%)") + xlab("Year") + 
  theme(legend.position = "bottom",legend.title = element_blank()) + ggtitle("Risk Sensitivity to Assessment/Advice Error")




#recruitment comparisons
plotvar(mystock      = "WHOM", myassess = "SS3", myassessyear = c("2019"),
        mymethod  = "EQSIM", myom = c("OM2.2","OM2.2.RR.5lowest","OM2.2.RR","OM2.2.RR.V5"),
        myniters     = "1000", mymp = c("MP5.23"),
        mynyrs       = "23", myftgt = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock", mycolour     = "blue",
        myyintercept = 611814, myvalue      = "median",
        myfacets     = c("om","ftgt"),
        myfirstyear  = 2000)

plotvar(mystock      = "WHOM", myassess = "SS3", myassessyear = c("2019"),
        mymethod  = "EQSIM", myom = c("OM2.2","OM2.2.RR.5lowest","OM2.2.RR","OM2.2.RR.V5","OM2.2.RR.V6"),
        myniters     = "1000", mymp = c("MP5.23"),
        mynyrs       = "23", myftgt = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "pblim", mycolour     = "blue",
        myyintercept = 0.05, myvalue      = "mean",
        myfacets     = c("om","ftgt"),
        myfirstyear  = 2000)


OMLookup <- c("OM2.2" = "Baseline","OM2.2.RR.V6" = "Baseline/2", "OM2.2.RR.V5" = "GM 02-13", 
              "OM2.2.RR.V7" = "Mn 5Low")

df <- dplyr::mutate(df,Title=paste0("Double BP With IAV, Ftarget = ",ftgt),OMDesc=OMLookup[om])

tt = filter(df,om %in% c("OM2.2","OM2.2.RR.V5","OM2.2.RR.V6","OM2.2.RR.V7") & mp=="MP5.23" 
            & nyrs==23 & ftgt %in% c(0.025,0.05,0.075,0.1))

ggplot(data = filter(tt,perfstat=="pblim"), mapping = aes(x=year, y=100*mean, group=OMDesc, col=OMDesc)) + geom_line() + 
  facet_wrap(~Title) + ylim(0,100) + xlim(2018,2040) + ylab("Risk to Blim (%)") + xlab("Year") + 
  theme(legend.position = "bottom",legend.title = element_blank()) + ggtitle("Risk Sensitivity to Reduced Recruitment 2014-2018")

ggplot(data = filter(tt,perfstat=="catch" & iter=="all"), mapping = aes(x=year, y=median/1e3, group=OMDesc, col=OMDesc)) + geom_line() + 
  facet_wrap(~Title) + ylim(0,130) + xlim(2018,2040) + ylab("Yield (kt)") + xlab("Year") + 
  theme(legend.position = "bottom",legend.title = element_blank()) + ggtitle("Yield Sensitivity to Reduced Recruitment 2014-2018")



ggplot(data = filter(tt,perfstat=="precblim"), mapping = aes(x=year, y=100*mean, group=OMDesc, col=OMDesc)) + geom_line() + 
  facet_wrap(~Title) + ylim(0,100) + xlim(2020,2040) + ylab("Risk to Blim (%)") + xlab("Year") + 
  theme(legend.position = "bottom",legend.title = element_blank()) + ggtitle("Risk Sensitivity to Reduced Recruitment 2014-2018")

df <- dplyr::mutate(df,Title=paste0("Constant Ftarget = ",ftgt),OMDesc=OMLookup[om])

tt = filter(df,om %in% c("OM2.2","OM2.2.RR.V5","OM2.2.RR.V6","OM2.2.RR.V7") & mp=="MP5.00" 
            & nyrs==23 & ftgt %in% c(0.025,0.05,0.075,0.1))
ggplot(data = filter(tt,perfstat=="pblim"), mapping = aes(x=year, y=100*mean, group=OMDesc, col=OMDesc)) + geom_line() + 
  facet_wrap(~Title) + ylim(0,100) + xlim(2020,2040) + ylab("Risk to Blim (%)") + xlab("Year") + 
  theme(legend.position = "bottom",legend.title = element_blank()) + ggtitle("Risk Sensitivity to Reduced Recruitment 2014-2018")






plotvar(mystock      = "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019"),
        mymethod     = c("EQSIM", "SAMHCR"),
        myom         = c("OM2.4","no_OM"),
        myniters     = "1000",
        mymp         = c("MP5.10"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept = 611814,
        myvalue      = "median",
        myfacets     = c("method","ftgt"),
        myfirstyear  = 2000)

plotvar(mystock      = "WHOM",
        myassess     = "SAM",
        myassessyear = c("2020"),
        mymethod     = c("EQSIM", "SAMHCR"),
        myom         = c("OM2.5","no_OM"),
        myniters     = "1000",
        mymp         = c("MP5.10"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept = 611814,
        myvalue      = "median",
        myfacets     = c("method","ftgt"),
        myfirstyear  = 2000)

# df %>% filter(method=="SAMHCR", !is.na(iter)) %>% View()

plotvar(mystock      = "WHOM",
        myassess     = "SS3",
        myassessyear = c("2019"),
        mymethod  = "EQSIM",
        myom         = c("OM2.2"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept = 611814,
        myvalue      = "median",
        myfacets     = c("assessyear","ftgt"),
        myfirstyear  = 2000)

plotvar(mystock      = "WHOM",
        myassess     = "SS3",
        myassessyear = c("2019"),
        mymethod  = "EQSIM",
        myom         = c("OM2.2"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept = 611814,
        myvalue      = "median",
        myfacets     = c("om","ftgt"),
        myfirstyear  = 2000)

plotvar(mystock      = "WHOM",
        myassess     = c("SAM"),
        myassessyear = c("2019"),
        mymethod  = "EQSIM",
        myom         = c("OM2.4", "OM2.4.WR"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "catch",
        mycolour     = "blue",
        myyintercept = 100000,
        myvalue      = "median",
        myfacets     = c("om","ftgt"),
        myfirstyear  = 2000)

plotvar(mystock      = "WHOM",
        myassess     = "SS3",
        myassessyear = c("2019","2019"),
        mymethod  = "EQSIM",
        myom         = c("OM2.2","OM2.2.RR.5lowest"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept = 611814,
        myvalue      = "median",
        myfacets     = c("om","ftgt"),
        myfirstyear  = 2000)

plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019","2020"),
        myom         = c("OM2.4","OM2.5"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.125, 0.15),
        myperfstat   = "fbar",
        mycolour     = "blue",
        myyintercept = 0.074,
        myvalue      = "median",
        myfacets     = c("assessyear","ftgt")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SS3",
        myassessyear = c("2019","2020"),
        myom         = c("OM2.2","OM2.3"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.125, 0.15),
        myperfstat   = "fbar",
        mycolour     = "blue",
        myyintercept = 0.074,
        myvalue      = "median",
        myfacets     = c("assessyear","ftgt")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019","2020"),
        myom         = c("OM2.4","OM2.5"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "rec",
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("assessyear","ftgt")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019","2020"),
        myom         = c("OM2.4","OM2.5"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "catch",
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("assessyear","ftgt")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019","2020"),
        myom         = c("OM2.4","OM2.5"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "pblim",
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("assessyear","ftgt")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SS3",
        myassessyear = c("2019","2020"),
        myom         = c("OM2.2","OM2.3"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "pblim",
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("assessyear","ftgt")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SS3",
        myassessyear = c("2019","2019"),
        myom         = c("OM2.2","OM2.2.RR"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "pblim",
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("myom","ftgt")
)


plotvar(mystock      =  "WHOM",
        myassess     = "SS3",
        myassessyear = c("2019","2019"),
        myom         = c("OM2.2","OM2.2.RR5lowest"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "pblim",
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("assessyear","ftgt")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019","2020"),
        myom         = c("OM2.4","OM2.5"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = c("recovbpa"),
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("assessyear","ftgt")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019"),
        myom         = c("OM2.4"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.075),
        myperfstat   = c("catch.n"),
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("ftgt","age")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019"),
        myom         = c("OM2.4"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.075),
        myperfstat   = c("catch.wt"),
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("ftgt","age")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019"),
        myom         = c("OM2.4"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.075),
        myperfstat   = c("stock.wt"),
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("ftgt","age")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019"),
        myom         = c("OM2.4"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.075),
        myperfstat   = c("mat"),
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("ftgt","age")
)

plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019"),
        myom         = c("OM2.4"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.075),
        myperfstat   = c("sel"),
        mycolour     = "blue",
        myyintercept = NA,
        myvalue      = "median",
        myfacets     = c("ftgt","age")
)




plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019"),
        myom         = c("OM2.4"),
        myniters     = "1000",
        mymp         = c("MP5.03", "MP5.13","MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept = 611814,
        myvalue      = "median",
        myfacets     = c("mp","ftgt")
)


plotvar(mystock      =  "WHOM",
        myassess     = c("SS3"),
        myassessyear = c("2019"),
        mymethod  = "EQSIM",
        myom         = c("OM2.2.WR","OM2.2"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept = 611814,
        myvalue      = "median",
        myfacets     = c("om","ftgt"),
        myfirstyear  = 2000)

plotvar(mystock      =  "WHOM",
        myassess     = c("SAM"),
        myassessyear = c("2019"),
        mymethod  = "EQSIM",
        myom         = c("OM2.4.WR","OM2.4"),
        myniters     = "1000",
        mymp         = c("MP5.23"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept =  834480,
        myvalue      = "median",
        myfacets     = c("om","ftgt"),
        myfirstyear  = 2000)

plotvar(mystock      =  "WHOM",
        myassess     = c("SS3"),
        myassessyear = c("2019"),
        mymethod  = "EQSIM",
        myom         = c("OM2.2"),
        myniters     = "1000",
        mymp         = c("MP5.23","MP5.23.DU"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept =  611814,
        myvalue      = "median",
        myfacets     = c("mp","ftgt"),
        myfirstyear  = 2000)

plotvar(mystock      =  "WHOM",
        myassess     = c("SAM"),
        myassessyear = c("2019"),
        mymethod  = "EQSIM",
        myom         = c("OM2.4"),
        myniters     = "1000",
        mymp         = c("MP5.23","MP5.23.DU"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept =  611814,
        myvalue      = "median",
        myfacets     = c("mp","ftgt"),
        myfirstyear  = 2000)


plotvar(mystock      =  "WHOM",
        myassess     = c("SAM"),
        myassessyear = c("2019"),
        mymethod  = "EQSIM",
        myom         = c("OM2.4"),
        myniters     = "1000",
        mymp         = c("MP5.23","MP5.23.def"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept =  611814,
        myvalue      = "median",
        myfacets     = c("mp","ftgt"),
        myfirstyear  = 2010,
        mylastyear   = 2030)




# ========================================================================================================

plotrecovery <- function(
                    mystock      =  "WHOM",
                    myassess     = "SAM",
                    myassessyear = c("2019","2020"),
                    mymethod  = "EQSIM",
                    myom         = c("OM2.4","OM2.5"),
                    myniters     = "1000",
                    mymp         = c("MP5.23"),
                    mynyrs       = "23",
                    myftgt       = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
                    myyintercept = 0.5,
                    myfacets     = c("assessyear","ftgt"),
                    myfirstyear  = as.numeric(NA),
                    mylastyear   = as.numeric(NA)) {
  
  # plot recovery to blim and bpa
  d <-
    df %>%
    filter(perfstat %in% c("precblim","precbpa")) %>%
    filter(stock      %in% mystock) %>% 
    filter(assess     %in% myassess) %>% 
    filter(assessyear %in% myassessyear) %>% 
    filter(method  %in% mymethod) %>% 
    filter(om         %in% myom) %>% 
    filter(mp         %in% mymp) %>% 
    filter(nyrs       %in% mynyrs) %>% 
    filter(ftgt       %in% myftgt) %>% 
    
    {if(!is.na(myfirstyear)) filter(., year >= myfirstyear) else (.)} %>%    
    {if(!is.na(mylastyear))  filter(., year <= mylastyear) else (.)} 
  
  # lines
  p <-
    df %>%
    filter(perfstat %in% c("firstyearrebuildtoblim","firstyearrebuildtobpa")) %>%
    filter(stock      %in% mystock) %>% 
    filter(assess     %in% myassess) %>% 
    filter(assessyear %in% myassessyear) %>% 
    filter(method  %in% mymethod) %>% 
    filter(om         %in% myom) %>% 
    filter(mp         %in% mymp) %>% 
    filter(nyrs       %in% mynyrs) %>% 
    filter(ftgt       %in% myftgt) %>% 
    
    mutate(perfstat = case_when(
      perfstat == "firstyearrebuildtoblim" ~ "precblim",
      perfstat == "firstyearrebuildtobpa"  ~ "precbpa",
      TRUE                                 ~ perfstat
    ))

  # title
  t <- paste(
    toupper(mystock),
    paste(unique(myassess), collapse="-"),
    paste(unique(myassessyear), collapse="-"),
    paste(unique(mymethod), collapse="-"),
    paste(unique(myom), collapse="-"),
    paste(unique(myniters), collapse="-"),
    paste(unique(mymp), collapse="-"),
    paste(unique(mynyrs), collapse="-"),
    "RECOVERY",
    paste(c(myfacets[2],myfacets[1]), collapse="-"),
    sep="_"
  )
  
  myfig <-
    d %>% 
    ggplot(aes(x=year, y=mean, group=perfstat)) +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
    theme( panel.grid.major.x = element_blank()) +
    
    geom_vline(data=p, aes(xintercept=mean, colour=perfstat), linetype="dashed") +
    
    geom_hline(yintercept=myyintercept, colour="gray", linetype="dashed") +
    
    geom_text(data=filter(p, perfstat %in% c("precblim")),
              aes(x=mean, label=substr(ac(mean),3,4), colour=perfstat),
              y=1.0, vjust=1, hjust=1, nudge_x = -1) +
    
    geom_text(data=filter(p, perfstat %in% c("precbpa")),
              aes(x=mean, label=substr(ac(mean),3,4), colour=perfstat),
              y=0.0, vjust=0, hjust=0, nudge_x = 1) +
    
    geom_line(aes(colour=perfstat)) +
    
    expand_limits(y=0) +
    labs(x="", y="", title=t) +
    facet_grid(get(myfacets[1]) ~ get(myfacets[2])) 
  
  print(myfig)
  
  ggsave(file = file.path(Res.dir,paste0(t, " recoveryplot.png")),
         device="png", width = 30, height = 20, units = "cm")
  
  
} # end of plotrecovery
  
plotrecovery(
  mystock      =  "WHOM",
  myassess     = "SAM",
  myassessyear = c("2019","2020"),
  mymethod  = "EQSIM",
  myom         = c("OM2.4","OM2.5"),
  myniters     = "1000",
  mymp         = c("MP5.23"),
  mynyrs       = "23",
  myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
  myyintercept = 0.5,
  myfacets     = c("assessyear","ftgt")
)
  
plotrecovery(
  mystock      =  "WHOM",
  myassess     = c("SS3", "SAM"),
  myassessyear = c("2019"),
  mymethod  = "EQSIM",
  myom         = c("OM2.2","OM2.4"),
  myniters     = "1000",
  mymp         = c("MP5.23"),
  mynyrs       = "23",
  myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
  myyintercept = 0.5,
  myfacets     = c("assess","ftgt")
)



