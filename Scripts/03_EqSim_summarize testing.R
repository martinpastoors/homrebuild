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

file.list <- list.files(path=stats.dir, pattern="eqSim_Stats", full.names=TRUE)

df <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(file.list)) {
  print(file.list[i])
  df <- bind_rows(df, loadRData(file.list[i])[["df"]])
}
df <- df %>% setNames(gsub("method","simulator", names(.))) %>% mutate(simulator = toupper(simulator))

i <- 11
print(file.list[i])
tmp <- loadRData(file.list[i])
t1 <- as.data.frame(tmp$stats[["0.1"]]$SSB$worm)
t2 <- as.data.frame(tmp$stats[["0.1"]]$SSB$val) %>% 
  filter(age %in% c("mean","50%","5%", "95%")) %>% 
  filter(!is.na(year)) %>% 
  mutate(
    var = case_when(
      age == "50%" ~ "median",
      age == "5%" ~ "lower",
      age == "95%" ~ "upper",
      TRUE           ~ age)
  ) %>% 
  dplyr::select(-age) %>% 
  pivot_wider(names_from=var, values_from=data)
t3 <- tmp$settings

myassess     = "SAM"
myassessyear = c("2019")
myom         = filter(t3, class=="OM", desc=="code")$value
mymp         = filter(t3, class=="MP", desc=="code")$value
myftgt       = "0.1"
myperfstat   = "stock"

# select from df
tdf <-
  df %>% 
  filter(
    assess   == myassess,
    assessyear == myassessyear,
    om       == myom,
    mp       == mymp,
    ftgt     == myftgt,
    perfstat == "stock"
  ) 


t1 <- as.data.frame(tmp$stats[["0.1"]]$SSB$val) %>% 
  filter(age %in% c("mean","50%","2.5%", "97.5%")) %>% 
  filter(year >= 2018) %>% 
  mutate(
    var = case_when(
      age == "50%" ~ "median",
      age == "2.5%" ~ "lower",
      age == "97.5%" ~ "upper",
      TRUE           ~ age)
  ) %>% 
  dplyr::select(-age) %>% 
  pivot_wider(names_from=var, values_from=data)

t2 <- 
  tdf %>% filter(year >= 2018, is.na(value))

t1w <- as.data.frame(tmp$stats[["0.1"]]$SSB$worm) %>% filter(year >= 2018) 
t2w <- tdf %>% filter(year >= 2018, !is.na(value))

ggplot() +
  theme_publication() +
  geom_ribbon(data=t1, aes(x=year, ymin=lower, ymax=upper, group=iter), fill="blue", alpha=0.4, inherit.aes = FALSE) +
  geom_ribbon(data=t2, aes(x=year, ymin=lower, ymax=upper, group=iter), fill="red", alpha=0.4, inherit.aes = FALSE) +
  geom_line(data=t1w, aes(x=year, y=data, group=iter), colour="blue") +
  geom_line(data=t2w, aes(x=year, y=value, group=iter), colour="red") +
  expand_limits(y=0)






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
  # myassess     = c("SS3")
  # myassessyear = c("2019")
  # mysimulator  = "EQSIM"
  # myom         = c("OM2.2")
  # myniters     = "1000"
  # mymp         = c("MP5.23","MP5.23.DU")
  # mynyrs       = "23"
  # myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15)
  # myperfstat   = "stock"
  # mycolour     = "blue"
  # myyintercept =  611814
  # myvalue      = "median"
  # myfacets     = c("mp","ftgt")
  # myfirstyear  = 2000
  # mylastyear   = as.numeric(NA)

# plot function
plotvar <- function(mystock      =  "WHOM",
                    myassess     = "SAM",
                    myassessyear = c("2019","2020"),
                    mysimulator  = "EQSIM",
                    myom         = c("OM2.4","OM2.5"),
                    myniters     = "1000",
                    mymp         = c("MP5.23"),
                    mynyrs       = "23",
                    myftgt       = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
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
    filter(stock      %in% mystock) %>% 
    filter(assess     %in% myassess) %>% 
    filter(assessyear %in% myassessyear) %>% 
    filter(simulator  %in% mysimulator) %>% 
    filter(om         %in% myom) %>% 
    # filter(niters     == myniters) %>% 
    
    {if(!is.na(myfirstyear)) filter(., year >= myfirstyear) else (.)} %>%    
    {if(!is.na(mylastyear))  filter(., year <= mylastyear) else (.)} %>%   
    
    mutate(age = as.numeric(age)) %>% 
    
    # fix for pblim 
    mutate(median = ifelse(perfstat=="pblim" & !is.na(mean), mean, median)) %>% 
    
    # add ftgt
    slice(rep(1:n(), each=length(myftgt))) %>% 
    mutate(ftgt = rep(myftgt, n()/length(myftgt))) %>% 
  
    # add mps
    slice(rep(1:n(), each=length(mymp))) %>% 
    mutate(mp = rep(mymp, n()/length(mymp))) 
    
  # future
  d <- 
    df %>%
    filter(period     %in% c("CU","ST","MT",'LT')) %>% 
    filter(mp         %in% mymp) %>%
    filter(perfstat   %in% myperfstat) %>% 
    filter(stock      %in% mystock) %>% 
    filter(assess     %in% myassess) %>% 
    filter(assessyear %in% myassessyear) %>% 
    filter(simulator  %in% mysimulator) %>% 
    filter(om         %in% myom) %>% 
    # filter(niters     ==   myniters) %>% 
    filter(mp         %in% mymp) %>%
    filter(nyrs       %in% mynyrs) %>% 
    filter(ftgt       %in% myftgt) %>% 
    
    {if(!is.na(myfirstyear)) filter(., year >= myfirstyear) else (.)} %>%    
    {if(!is.na(mylastyear))  filter(., year <= mylastyear) else (.)} %>%   
    
    mutate(code = paste("EqSim", assess,assessyear,om,mp,sep="_")) %>%  
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
    paste(unique(mysimulator), collapse="-"),
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
    
  myfig <- 

    ggplot(d, aes(x=year, y=get(myvalue), group=mp)) +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
    theme( panel.grid.major.x = element_blank()) +
    theme(legend.position = "none") +
    
    # historical ribbon
    geom_ribbon(data=filter(h, !is.na(get(myvalue))),
                aes(ymin=lower, ymax=upper, group=iter), alpha=0.4, fill="black") +
    
    # historical worms
    geom_line(data=filter(h, !is.na(value)), 
              aes(x=year,y=value,group=iter), colour="darkgray", size=0.5) +
    
    # historical median
    geom_line(data=filter(h, !is.na(get(myvalue))), 
              aes(x=year,y=get(myvalue),group=iter), colour="black", size=1) +
    
    # future ribbon
    geom_ribbon(data=filter(d, !is.na(get(myvalue))),
                aes(ymin=lower, ymax=upper, group=iter), alpha=0.4, fill=mycolour) +
    
    # future worms
    geom_line(data=filter(d, !is.na(value)),
              aes(x=year, y=value, group=iter), colour=mycolour, size=0.5) +
    
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


plotvar(mystock      =  "WHOM",
        myassess     = "SAM",
        myassessyear = c("2019","2020"),
        mysimulator  = "EQSIM",
        myom         = c("OM2.4","OM2.5"),
        myniters     = "1000",
        mymp         = c("MP5.13"),
        mynyrs       = "23",
        myftgt       = c(0.0, 0.05, 0.075, 0.10, 0.15),
        myperfstat   = "stock",
        mycolour     = "blue",
        myyintercept = 611814,
        myvalue      = "median",
        myfacets     = c("assessyear","ftgt"),
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
        mysimulator  = "EQSIM",
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
        mysimulator  = "EQSIM",
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
        mysimulator  = "EQSIM",
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
        mysimulator  = "EQSIM",
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
        mysimulator  = "EQSIM",
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
        myfirstyear  = 2010,
        mylastyear   = 2030)











# plot recovery to blim and bpa
# df %>%
#   filter(ftgt %in% myftgt) %>% 
#   filter(perfstat %in% c("recovblim","recovbpa")) %>% 
#   filter(runref %in% myruns) %>% 
#   # separate(runref, into=c("stock","assess","om","mp","iters","nyears"), sep="_", convert=FALSE) %>% 
#   mutate(code = paste(method, assess,om,mp,sep="_")) %>% 
#   
#   ggplot(aes(x=year, y=median, group=perfstat)) +
#   theme_publication() +
#   theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
#   theme( panel.grid.major.x = element_blank()) +
#   # theme(legend.position = "none") +
#   
#   geom_line(aes(colour=perfstat)) +
#   
#   geom_vline(xintercept=periods2$year[1], linetype="dotted") +
#   geom_vline(xintercept=periods2$year[2], linetype="dotted") +
#   geom_vline(xintercept=periods2$year[3], linetype="dotted") +
#   geom_vline(xintercept=periods2$year[4], linetype="dotted") +
#   
#   geom_vline(data=r, aes(xintercept=year, colour=perfstat), linetype="dashed", size=0.5) +
#   
#   # geom_text(data=r, aes(x=year, colour=perfstat, label=year), y = -Inf, vjust=-1.2) +
#   ggrepel::geom_text_repel(data=r, aes(x=year, colour=perfstat, label=year), y=-Inf)  +
#   
#   expand_limits(y=0) +
#   labs(x="", y="value", title="Recovery to Blim and Bpa") +
#   facet_grid(mp ~ ftgt, scales="free_y")
# 
# ggsave(file = file.path(Res.dir,paste0(mp, "_recovery_summary_byyear.png")),
#        device="png", width = 30, height = 20, units = "cm")






# Plot with comparison across methods and assessments

# myruns <- c("WHOM_SS3_OM2.2_MP5.03_1000_50",
#             "WHOM_SS3_OM2.2_MP5.13_1000_50",
#             "WHOM_SS3_OM2.2_MP5.23_1000_50")
# 
# myruns <- c("WHOM_SS3_OM2.2_MP5.1_1000_50", 
#             "WHOM_SAM_OM2.3_MP2.1_1000_20", 
#             "samhcr_WHOM_sam_-_5.1_1000_20")
# 
# myftgt <- c(0.05, 0.075, 0.10, 0.2)
# myperfstat <- "ssb"; mycolour   <- "blue"; myyintercept <- as.numeric(NA)
# myperfstat <- "harvest"; mycolour   <- "darkgreen"; myyintercept <- as.numeric(NA)
# myperfstat <- "catch"; mycolour   <- "purple"; myyintercept <- as.numeric(NA)
# myperfstat <- "rec"; mycolour   <- "orange"; myyintercept <- as.numeric(NA)
# myperfstat <- "pblim"; mycolour   <- "red"; myyintercept <- 0.05
# 
# dfsum %>%
#   filter(ftgt %in% myftgt) %>% 
#   filter(perfstat==myperfstat) %>% 
#   filter(runref %in% myruns) %>% 
#   # separate(runref, into=c("stock","assess","om","mp","iters","nyears"), sep="_", convert=FALSE) %>% 
#   mutate(code = paste(method, assess,mp, sep="_")) %>% 
#   mutate(blim = ifelse(assess=="SAM",612, 834)) %>% 
#   
#   ggplot(aes(x=year, y=mean, group=mp)) +
#   theme_publication() +
#   theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
#   theme( panel.grid.major.x = element_blank()) +
#   theme(legend.position = "none") +
#   
#   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2, fill=mycolour) +
#   
#   geom_line(data=filter(worms, ftgt %in% myftgt,
#                         perfstat %in% myperfstat,
#                         runref %in% myruns), 
#             aes(x=year, y=value, group=iter),
#             size=0.5, colour="gray", inherit.aes = FALSE) +
#   
#   geom_line(colour=mycolour) +
#   
#   geom_hline(yintercept=myyintercept, linetype="dashed") +
#   #geom_hline(aes(yintercept=blim), linetype="dashed") +
#   
#   geom_vline(xintercept=periods2$year[1], linetype="dotted") +
#   geom_vline(xintercept=periods2$year[2], linetype="dotted") +
#   geom_vline(xintercept=periods2$year[3], linetype="dotted") +
#   geom_vline(xintercept=periods2$year[4], linetype="dotted") +
#   
#   expand_limits(y=0) +
#   labs(x="", y="value", title=myperfstat) +
#   facet_grid(code ~ ftgt)

# ggsave(file = file.path(Res.dir,runName,paste0(runName,"_summary_byyear.png")),
#        device="png", width = 30, height = 20, units = "cm")

