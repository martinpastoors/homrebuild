# ================================================================================================================
# EqSim HCR simulator
# 
# 01_EqSim_setup.R
# 
# The EqSim simulator was developed by David Miller and Colin Millar
# Further enhanced by Andy Campbell and Martin Pastoors and applied to Western Horse mackerel in June 2020
#
# 24/06/2020 generic option; code now independent of fish stock
# 25/06/2020 tested on mackerel stock
# 27/06/2020 tested on 1000 iters of SAM assessment
# ================================================================================================================
#Setup the Environment, folder locations

#clean up first
rm(list=ls())
invisible(gc())

try(dev.off(),silent=TRUE)
try(sink(),silent=TRUE)

#packages
library(stockassessment)  # for SAM forecasts etc.; devtools::install_github("fishfollower/SAM/stockassessment")
library(FLCore)
library(Cairo)    #plotting
library(devEMF)   
library(officer)  #for op to word doc
library(flextable)
library(gplots)   #rich.colors
library(dplyr)
library(tidyr)
library(ggplot2)
library(icesTAF) # install.packages("icesTAF")

library(tidyverse)
options(dplyr.summarise.inform = FALSE)

# library(Matrix)
# library(gridExtra)

#stop on warning
#options(warn=2)

#sessionInfo()
#R version 3.5.3 (2019-03-11)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 17763)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_Ireland.1252  LC_CTYPE=English_Ireland.1252    LC_MONETARY=English_Ireland.1252
#[4] LC_NUMERIC=C                     LC_TIME=English_Ireland.1252    

#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#[1] ggplot2_3.3.1    tidyr_1.0.2      dplyr_0.8.5      gplots_3.0.3     Cairo_1.5-12     FLCore_2.6.14    iterators_1.0.12
#[8] lattice_0.20-38 

#loaded via a namespace (and not attached):
#[1] Rcpp_1.0.3         pillar_1.4.3       compiler_3.5.3     bitops_1.0-6       tools_3.5.3        packrat_0.5.0     
#[7] lifecycle_0.2.0    tibble_3.0.1       gtable_0.3.0       pkgconfig_2.0.3    rlang_0.4.5        Matrix_1.2-18     
#[13] rstudioapi_0.11    withr_2.1.2        vctrs_0.2.4        gtools_3.8.1       caTools_1.17.1.2   hms_0.5.3         
#[19] stats4_3.5.3       grid_3.5.3         tidyselect_1.0.0   glue_1.4.0         R6_2.4.1           gdata_2.18.0      
#[25] readr_1.3.1        purrr_0.3.3        magrittr_1.5       scales_1.1.0       ellipsis_0.3.0     MASS_7.3-51.5     
#[31] assertthat_0.2.1   colorspace_1.4-1   KernSmooth_2.23-16 munsell_0.5.0      crayon_1.3.4  


#computer specific locations
# Drive    <- "C:"
# Base.dir <- file.path(Drive,"Stocks","hom_27_2a4a5b6a7a-ce-k8","MP_MSE")
# Drive    <- "D:"
Base.dir <- getwd()

#Basic MSE directory
# MSE.dir <- file.path(Base.dir,"EqSimWHM")
MSE.dir <- file.path(Base.dir)

#this is where the results of the 1000 assessment runs for initialisation of the MSE are saved
Data.dir <- file.path(MSE.dir,"Data")              

#any useful stuff contained in RData files
# RData.dir <- file.path(MSE.dir,"RData")
RData.dir <- file.path(MSE.dir,"WGWIDE22", "RData")

#debug/verbose output
Log.dir <- file.path(MSE.dir,"Logs")              

#Simulation and statistical outputs
Res.dir <- file.path(MSE.dir, "Results")

#Plot dir
Plot.dir <- file.path(MSE.dir, "Plots")

# Source dir and Scripts dir
Source.dir <- file.path(MSE.dir,"R")              #R functions
Scripts.dir <- file.path(MSE.dir, "Scripts")      #R scripts

# Load OMs, MPs, utilities
source(file = file.path(Scripts.dir,"OMs.R"))
source(file = file.path(Scripts.dir,"MPs.R"))

#source all functions in source.dir
sapply(list.files(path=file.path(Source.dir), pattern=".R", full.names=TRUE), source)

