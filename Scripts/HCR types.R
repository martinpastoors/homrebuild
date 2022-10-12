# ====================================================================================
# 1.SamtoFLStock.r
# ====================================================================================

library(tidyverse)
library(readxl)

source("R/utilities.R")

df <-
  # read_excel(path=file.path(getwd(), "EqSimWHM","data","hcr_types.xlsx")) 
  read_excel(path=file.path(getwd(), "data","hcr_types v2.xlsx")) 


df %>% 
  mutate(scenario=factor(scenario, levels=c("ICES AR", "Double BP + IAV"))) %>% 
  
  ggplot(aes(x=ssb,y=f, group=scenario)) +
  theme_publication() +
  theme(panel.grid.major = element_blank()) +
  # theme(legend.position = "none") +
  geom_vline(xintercept=834, colour = "gray", linetype="dotted", size=0.6) +
  geom_vline(xintercept=1168, colour = "gray", linetype="dotted", size=0.6) +
  geom_hline(yintercept=0.074, colour = "gray", linetype="dotted", size=0.6) +
  geom_ribbon(aes(fill=scenario, ymin=flow, ymax=fupp), alpha=0.3) +
  geom_line(aes(colour=scenario, linetype=scenario), size=1) +
  scale_linetype(c("solid","dashed")) +
  expand_limits(y=0) +
  guides(linetype=FALSE) 
  # facet_wrap(~scenario)


