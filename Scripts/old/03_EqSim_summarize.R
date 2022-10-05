# ================================================================================================================
# 03 EqSim summarize
# 
# Summarize results of EqSim simulations
#
# 06/07/2020 tested on 1000 iters of SAM assessment
# ================================================================================================================

rm(list=ls())
gc()

library(tidyverse)

#computer specific locations
Drive    <- "D:"
Base.dir <- file.path(Drive,"GIT")
MSE.dir <- file.path(Base.dir,"wk_WKREBUILD","EqSimWHM")
Res.dir <- file.path(MSE.dir, "Results")

# Get dropbox dir; for storing large RData files
source("EqSimWHM/R/get_dropbox.R")
source("EqSimWHM/R/utilities.R")
dropbox.dir <- file.path(get_dropbox(), "HOM FG", "05. Data","RData")

# ==================================================================================

#basic display setting
niters <- 1000
maxyr <- 2040

stats.dir <- file.path(Res.dir, "Stats")

list.files.eqsim.ss <- list.files(path=stats.dir, pattern="WHOM_SAM", full.names=TRUE)
#list.files.eqsim.ss <- list.files(path=stats.dir, pattern="WHOM_SS3", full.names=TRUE)
#list.files.eqsim.ss <- list.files(path=stats.dir, pattern="WHOM_SS3", full.names=TRUE)[c(-3,-7,-11)]
# list.files.eqsim.ss <- list.files(path=stats.dir, pattern="WHOM_SS", full.names=TRUE)[c(-6,-9,-12)]
# list.files.eqsim.ss <- 
#   c("D:/GIT/wk_WKREBUILD/EqSimWHM/Results/Stats/old/WHOM_SS_OM2.2_MP5.23_1000_23_eqSim_Stats.Rdata",
#     "D:/GIT/wk_WKREBUILD/EqSimWHM/Results/Stats/WHOM_SS3_OM2.2_MP5.23_1000_50_eqSim_Stats.Rdata"
#   )
# list.files.eqsim.ss <- 
#   c("D:/GIT/wk_WKREBUILD/EqSimWHM/Results/Stats/WHOM_SS3_OM2.2_MP5.00_1000_50_eqSim_Stats.Rdata")

periods <-
  loadRData(list.files.eqsim.ss[1])[["lStatPer"]][c("CU","ST","MT","LT")] %>% 
  as.data.frame() %>% 
  ungroup() %>%
  pivot_longer(cols=c(CU:LT), names_to="period", values_to="year") %>% 
  arrange(period, year) %>% 
  group_by(period) %>% 
  mutate(year2 = lead(year, n=1)) %>% 
  filter(!is.na(year2)) %>% 
  mutate(y = paste(seq(year, year2), collapse="_")) %>% 
  dplyr::select(-year, -year2) %>% 
  separate(y, into=paste0("X",seq(1:36)), sep="_") %>% 
  pivot_longer(cols=c(X1:X36), names_to="tmp", values_to="year") %>% 
  filter(!is.na(year)) %>% 
  dplyr::select(-tmp) %>% 
  ungroup() %>% 
  mutate(period = factor(period, levels=c("CU","ST","MT","LT")),
         year   = as.integer(year)) %>% 
  filter(year <= maxyr)

periods2 <-
  periods %>% 
  group_by(period) %>% 
  filter(year == max(year))


# Compare 2 specific cases; one run by Andy, one by Martin

# list.files.eqsim.ss <-
#   c("D:/GIT/wk_WKREBUILD/EqSimWHM/Results/Stats/WHOM_SS3_OM2.2_MP5.23_1000_23_eqSim_Stats.Rdata",
#     "D:/GIT/wk_WKREBUILD/EqSimWHM/Results/Stats/andy/WHOM_SS3_OM2.2_MP5.23_1000_50_eqSim_Stats.Rdata"
#   )
# x <- loadRData(list.files.eqsim.ss[1])[["df"]] %>% filter(Ftgt=="0.075") %>%
#    mutate(runby="Martin")
# y <- loadRData(list.files.eqsim.ss[2])[["stats"]][["0.075"]][["dfy"]] %>% filter(year <= 2040) %>%
#   mutate(runby="Andy", Ftgt=as.numeric(Ftgt)) %>%
#   mutate(value = ifelse(PerfStat %in% c("SSB", "CW"), value, value))
# 
# bind_rows(x,y) %>%
#   filter(PerfStat == "CW") %>%
#   group_by(year, runby, Ftgt, MP) %>%
#   summarize(median = median(value),
#             upper  = quantile(value, probs=0.975, na.rm=TRUE),
#             lower  = quantile(value, probs=0.025, na.rm=TRUE))  %>%
#   ggplot((aes(x=year, y=median))) +
#   theme_publication() +
#   geom_line(aes(colour=runby)) +
#   geom_ribbon(aes(fill=runby, ymin=lower, ymax=upper), alpha=0.3)

  

# SS / EqSim
i <- 1
df <- worms <- data.frame(stringsAsFactors = FALSE)
for (i in 1:length(list.files.eqsim.ss)) {
  
  gc()  
  cat(list.files.eqsim.ss[i],"\n")

  OM <- loadRData(list.files.eqsim.ss[i])[["OM"]]  
  MP <- loadRData(list.files.eqsim.ss[i])[["MP"]]  
  
  x <- 
    loadRData(list.files.eqsim.ss[i])[["df"]] %>% 
    lowcase() %>% 
    mutate(perfstat = tolower(perfstat)) %>%
    mutate(perfstat = ifelse(perfstat == "cw", "catch", perfstat)) %>% 
    mutate(method = "EqSim") %>% 
    mutate(blim = OM[["refPts"]][["Blim"]]) %>%
    mutate(bpa = OM[["refPts"]][["Bpa"]])
    
    # summarize the dataframe for this run
  summ <-
    x %>% 
    group_by(runref, assess, method, om, mp, niters, nyrs, label, ftgt, perfstat, unit, blim, bpa, period, year) %>% 
    mutate(value = ifelse(perfstat=="iav" & ftgt==0, NA, value)) %>% 
    summarize(mean = mean(value, na.rm=TRUE),
              median = median(value, na.rm=TRUE),
              upper = quantile(value, probs=0.975, na.rm=TRUE),
              lower = quantile(value, probs=0.025, na.rm=TRUE)) %>%
    ungroup() 
    
  df    <- bind_rows(df, summ)
    
  # add the worms of this run
  y <-
    x %>% 
    group_by(runref, assess, method, om, mp, niters, nyrs, label, ftgt, perfstat, unit, blim, bpa, period, year) %>% 
    filter(row_number() <= 5) %>% 
    ungroup() 
  
  worms <- bind_rows(worms, y)
    
  # }
}


# df %>% 
#   filter(perfstat=="recovbpa") %>% 
#   ggplot(aes(x=year, y=value, group=mp, colour=mp)) + 
#   geom_line() +
#   facet_wrap(~ftgt)

# SAM EqSim
# dfall2 <- data.frame(stringsAsFactors = FALSE)
# list.files.eqsim.sam <- list.files(path=stats.dir, pattern="WHOM_SAM", full.names=TRUE)
# for (i in 1:length(list.files.eqsim.sam)) {
#   gc()  
#   cat(list.files.eqsim.sam[i],"\n")
#   x <- 
#     loadRData(list.files.eqsim.sam[i]) %>% 
#     lowcase() %>% 
#     filter(year <= 2037) %>% 
#     mutate(iter = as.character(iter)) %>% 
#     mutate(ftgt = as.character(ftgt)) %>% 
#     mutate(perfstat = tolower(perfstat)) %>% 
#     mutate(perfstat = ifelse(perfstat == "cw", "catch", perfstat)) %>% 
#     # mutate(value = ifelse(perfstat %in% c("catch","tac"), value/1000, value)) %>% 
#     mutate(mp = ifelse(mp=="MP2.1", "MP5.1",mp)) %>% 
#     mutate(mp = ifelse(mp=="MP2.2", "MP5.12",mp)) %>% 
#     mutate(method="EqSim") %>% 
#     mutate(blim = 661917) %>% 
#     mutate(bpa = 911587) 
#     
#   dfall2 <- bind_rows(dfall2, x)
# }

# SAM / SAM HCR
# list.files.sam   <- list.files(path=stats.dir, pattern="^sam", full.names=TRUE)
# dfall3 <- data.frame(stringsAsFactors = FALSE)
# for (i in 1:length(list.files.sam)) {
#   gc()  
#   cat(list.files.sam[i],"\n")
#   x <- 
#     loadRData(list.files.sam[i]) %>% 
#     lowcase() %>% 
#     filter(year <= 2037) %>% 
#     filter(!perfstat %in% c("land", "fbarl", "tsb")) %>%  
#     mutate(iter = as.character(iter)) %>% 
#     mutate(ftgt = as.character(ftgt)) %>% 
#     mutate(perfstat = ifelse(perfstat == "fbar", "harvest", perfstat)) %>% 
#     mutate(value = ifelse(perfstat == "ssb", value/1000, value)) %>% 
#     mutate(value = ifelse(perfstat == "catch", value/1000, value)) %>% 
#     mutate(assess="SAM") %>% 
#     mutate(method="SAMhcr")  %>% 
#     mutate(blim = 661917) %>% 
#     mutate(bpa = 911587) %>% 
#     mutate(niters="1000", stock="WHOM", nyrs="20", label="ICES AR", mp="MP5.10")
#   
#   dfall3 <- bind_rows(dfall3, x)
# }

save(df,file = file.path(dropbox.dir,paste0("df.Rdata")))
save(worms,file = file.path(dropbox.dir,paste0("worms.Rdata")))

# load(file = file.path(dropbox.dir,paste0("dfall.Rdata")))
# load(file = file.path(dropbox.dir,paste0("dfsum.Rdata")))

# plot for single method and assessment; different MPs

myruns <- c("WHOM_SS3_OM2.2_MP5.03_1000_23", 
            "WHOM_SS3_OM2.2_MP5.13_1000_23",
            "WHOM_SS3_OM2.2_MP5.23_1000_23")
myruns <- c("WHOM_SS3_OM2.2_MP5.03_1000_50", 
            "WHOM_SS3_OM2.2_MP5.13_1000_50",
            "WHOM_SS3_OM2.2_MP5.23_1000_50")
myruns <- c("WHOM_SS3_OM2.2_MP5.0_1000_50", 
            "WHOM_SS3_OM2.2_MP5.01_1000_50",
            "WHOM_SS3_OM2.2_MP5.02_1000_50",
            "WHOM_SS3_OM2.2_MP5.03_1000_50")
myruns <- c("WHOM_SS3_OM2.2_MP5.1_1000_50", 
            "WHOM_SS3_OM2.2_MP5.11_1000_50",
            "WHOM_SS3_OM2.2_MP5.12_1000_50",
            "WHOM_SS3_OM2.2_MP5.13_1000_50")
myruns <- c("WHOM_SS3_OM2.2_MP5.2_1000_50", 
            "WHOM_SS3_OM2.2_MP5.21_1000_50",
            "WHOM_SS3_OM2.2_MP5.22_1000_50",
            "WHOM_SS3_OM2.2_MP5.23_1000_50")
myruns <- c("WHOM_SAM_OM2.3_MP2.1_1000_20", 
            "WHOM_SAM_OM2.3_MP2.2_1000_20")
myruns <- c("samhcr_WHOM_sam_-_5.1_1000_20")

# plot function
plotvar <- function(myruns =  c("WHOM_SS3_OM2.2_MP5.03_1000_23", 
                                "WHOM_SS3_OM2.2_MP5.13_1000_23",
                                "WHOM_SS3_OM2.2_MP5.23_1000_23"),
                    myftgt = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
                    myperfstat = "ssb",
                    mycolour   = "blue",
                    myyintercept = 834000,
                    myvalue="median") {
  
  d <- 
    df %>%
    filter(ftgt %in% myftgt) %>% 
    filter(perfstat==myperfstat) %>% 
    filter(runref %in% myruns) %>% 
    # separate(runref, into=c("stock","assess","om","mp","iters","nyears"), sep="_", convert=FALSE) %>% 
    mutate(code = paste(method, assess,om,mp,sep="_")) 
  
  print(head(d))
  
  mp <- d %>% distinct(mp) %>% summarize(s = paste(mp, collapse="_")) %>% as.character()
  
  p <-
    d %>% 
    ggplot(aes(x=year, y=get(myvalue), group=mp)) +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
    theme( panel.grid.major.x = element_blank()) +
    theme(legend.position = "none") +
    
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2, fill=mycolour) +
    
    geom_line(data=filter(worms, ftgt %in% myftgt,
                          perfstat %in% myperfstat,
                          runref %in% myruns), 
              aes(x=year, y=value, group=iter),
              size=0.5, colour="gray", inherit.aes = FALSE) +
    
    geom_line(colour=mycolour) +
    
    geom_hline(yintercept=myyintercept, linetype="dashed") +
    
    geom_vline(xintercept=periods2$year[1], linetype="dotted") +
    geom_vline(xintercept=periods2$year[2], linetype="dotted") +
    geom_vline(xintercept=periods2$year[3], linetype="dotted") +
    geom_vline(xintercept=periods2$year[4], linetype="dotted") +
    
    expand_limits(y=0) +
    labs(x="", y="value", title=myperfstat) +
    facet_grid(mp ~ ftgt, scales="free_y") 
    
  print(p)

  ggsave(file = file.path(Res.dir,paste0(mp, "_", myperfstat,"_summary_byyear.png")),
         device="png", width = 30, height = 20, units = "cm")
  
}



plotvar(myruns =  c("WHOM_SS3_OM2.2_MP5.00_1000_23", 
                    "WHOM_SS3_OM2.2_MP5.01_1000_23",
                    "WHOM_SS3_OM2.2_MP5.03_1000_23"),
        myftgt = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
        myperfstat = "ssb", mycolour   = "blue", myyintercept = 834000, myvalue="median")

plotvar(myruns =  c("WHOM_SS3_OM2.2_MP5.03_1000_23", 
                    "WHOM_SS3_OM2.2_MP5.13_1000_23",
                    "WHOM_SS3_OM2.2_MP5.23_1000_23"),
        myftgt = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
        myperfstat = "ssb", mycolour   = "blue", myyintercept = 834000, myvalue="median")
plotvar(myftgt = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
        myperfstat = "harvest", mycolour   = "darkgreen", myyintercept = 0.074, myvalue="median")
plotvar(myftgt = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
        myperfstat = "catch", mycolour   = "purple", myyintercept = as.numeric(NA), myvalue="median")
plotvar(myftgt = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
        myperfstat = "rec", mycolour   = "orange", myyintercept = as.numeric(NA), myvalue="median")
plotvar(myftgt = c(0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
        myperfstat = "iav", mycolour   = "brown", myyintercept = as.numeric(NA), myvalue="median")

plotvar(myruns =  c("WHOM_SS3_OM2.2_MP5.20_1000_23", 
                    "WHOM_SS3_OM2.2_MP5.21_1000_23",
                    "WHOM_SS3_OM2.2_MP5.23_1000_23"),
        myftgt = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
        myperfstat = "ssb", mycolour   = "blue", myyintercept = 834000, myvalue="median")
plotvar(myruns =  c("WHOM_SS3_OM2.2_MP5.20_1000_23", 
                    "WHOM_SS3_OM2.2_MP5.21_1000_23",
                    "WHOM_SS3_OM2.2_MP5.23_1000_23"),
        myftgt = c(0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
        myperfstat = "iav", mycolour   = "brown", myyintercept = as.numeric(NA), myvalue="median")

plotvar(myruns =  c("WHOM_SAM_OM2.3_MP5.03_1000_23", 
                    "WHOM_SAM_OM2.3_MP5.13_1000_23",
                    "WHOM_SAM_OM2.3_MP5.23_1000_23"),
        myftgt = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15),
        myperfstat = "ssb", mycolour   = "blue", myyintercept = 612000, myvalue="median")

# Calculate recovery year to Blim and Bpa (50%)
myruns =  c("WHOM_SAM_OM2.3_MP5.03_1000_23", 
            "WHOM_SAM_OM2.3_MP5.13_1000_23",
            "WHOM_SAM_OM2.3_MP5.23_1000_23")
myftgt = c(0.0, 0.025, 0.05, 0.075, 0.10, 0.125, 0.15)

r <-
  df %>% 
  filter(ftgt %in% myftgt) %>% 
  filter(perfstat %in% c("recovblim","recovbpa")) %>%
  filter(runref %in% myruns) %>% 
  filter(median >= 0.5) %>%
  group_by(runref, mp, perfstat, ftgt) %>% 
  filter(year == min(year))

mp <- r %>% ungroup() %>% distinct(mp) %>% summarize(s = paste(mp, collapse="_")) %>% as.character()

# plot recovery to blim and bpa
df %>%
  filter(ftgt %in% myftgt) %>% 
  filter(perfstat %in% c("recovblim","recovbpa")) %>% 
  filter(runref %in% myruns) %>% 
  # separate(runref, into=c("stock","assess","om","mp","iters","nyears"), sep="_", convert=FALSE) %>% 
  mutate(code = paste(method, assess,om,mp,sep="_")) %>% 
  
  ggplot(aes(x=year, y=median, group=perfstat)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  theme( panel.grid.major.x = element_blank()) +
  # theme(legend.position = "none") +
  
  geom_line(aes(colour=perfstat)) +
  
  geom_vline(xintercept=periods2$year[1], linetype="dotted") +
  geom_vline(xintercept=periods2$year[2], linetype="dotted") +
  geom_vline(xintercept=periods2$year[3], linetype="dotted") +
  geom_vline(xintercept=periods2$year[4], linetype="dotted") +
  
  geom_vline(data=r, aes(xintercept=year, colour=perfstat), linetype="dashed", size=0.5) +
  
  # geom_text(data=r, aes(x=year, colour=perfstat, label=year), y = -Inf, vjust=-1.2) +
  ggrepel::geom_text_repel(data=r, aes(x=year, colour=perfstat, label=year), y=-Inf)  +
  
  expand_limits(y=0) +
  labs(x="", y="value", title="Recovery to Blim and Bpa") +
  facet_grid(mp ~ ftgt, scales="free_y")

ggsave(file = file.path(Res.dir,paste0(mp, "_recovery_summary_byyear.png")),
       device="png", width = 30, height = 20, units = "cm")






# Plot with comparison across methods and assessments

myruns <- c("WHOM_SS3_OM2.2_MP5.03_1000_50",
            "WHOM_SS3_OM2.2_MP5.13_1000_50",
            "WHOM_SS3_OM2.2_MP5.23_1000_50")

myruns <- c("WHOM_SS3_OM2.2_MP5.1_1000_50", 
            "WHOM_SAM_OM2.3_MP2.1_1000_20", 
            "samhcr_WHOM_sam_-_5.1_1000_20")

myftgt <- c(0.05, 0.075, 0.10, 0.2)
myperfstat <- "ssb"; mycolour   <- "blue"; myyintercept <- as.numeric(NA)
myperfstat <- "harvest"; mycolour   <- "darkgreen"; myyintercept <- as.numeric(NA)
myperfstat <- "catch"; mycolour   <- "purple"; myyintercept <- as.numeric(NA)
myperfstat <- "rec"; mycolour   <- "orange"; myyintercept <- as.numeric(NA)
myperfstat <- "pblim"; mycolour   <- "red"; myyintercept <- 0.05

dfsum %>%
  filter(ftgt %in% myftgt) %>% 
  filter(perfstat==myperfstat) %>% 
  filter(runref %in% myruns) %>% 
  # separate(runref, into=c("stock","assess","om","mp","iters","nyears"), sep="_", convert=FALSE) %>% 
  mutate(code = paste(method, assess,mp, sep="_")) %>% 
  mutate(blim = ifelse(assess=="SAM",612, 834)) %>% 
  
  ggplot(aes(x=year, y=mean, group=mp)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  theme( panel.grid.major.x = element_blank()) +
  theme(legend.position = "none") +
  
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2, fill=mycolour) +
  
  geom_line(data=filter(worms, ftgt %in% myftgt,
                        perfstat %in% myperfstat,
                        runref %in% myruns), 
            aes(x=year, y=value, group=iter),
            size=0.5, colour="gray", inherit.aes = FALSE) +
  
  geom_line(colour=mycolour) +
  
  geom_hline(yintercept=myyintercept, linetype="dashed") +
  #geom_hline(aes(yintercept=blim), linetype="dashed") +
  
  geom_vline(xintercept=periods2$year[1], linetype="dotted") +
  geom_vline(xintercept=periods2$year[2], linetype="dotted") +
  geom_vline(xintercept=periods2$year[3], linetype="dotted") +
  geom_vline(xintercept=periods2$year[4], linetype="dotted") +
  
  expand_limits(y=0) +
  labs(x="", y="value", title=myperfstat) +
  facet_grid(code ~ ftgt)

# ggsave(file = file.path(Res.dir,runName,paste0(runName,"_summary_byyear.png")),
#        device="png", width = 30, height = 20, units = "cm")

