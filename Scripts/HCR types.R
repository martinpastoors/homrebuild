# ====================================================================================
# 1.SamtoFLStock.r
# ====================================================================================

library(tidyverse)
library(readxl)

source("EqSimWHM/R/utilities.R")

df <-
  read_excel(path=file.path(getwd(), "EqSimWHM","data","hcr_types.xlsx")) %>% 
  

df %>% 
  filter(grepl("IAV", scenario)) %>% 
  mutate(scenario=factor(scenario, levels=c("const F + IAV", "ICES AR + IAV", "Double BP + IAV"))) %>% 
  
  ggplot(aes(x=ssb,y=f, group=scenario)) +
  theme_publication() +
  theme(panel.grid.major = element_blank()) +
  theme(legend.position = "none") +
  geom_vline(xintercept=600, colour = "gray", linetype="dashed", size=0.6) +
  geom_vline(xintercept=800, colour = "gray", linetype="dotted", size=0.8) +
  geom_ribbon(aes(fill=scenario, ymin=flow, ymax=fupp), alpha=0.3) +
  geom_line(aes(colour=scenario), size=1) +
  expand_limits(y=0) +
  facet_wrap(~scenario)


