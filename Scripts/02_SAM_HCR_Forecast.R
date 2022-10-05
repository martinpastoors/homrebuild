# ============================================================================
# 00 SAM HCR forecast
#
# Author: Vanessa Trijoulet vtri@aqua.dtu.dk 
# Adapted by: Martin Pastoors mpastoors@pelagicfish.eu
#
# 13/03/2021 adapted to new simulation outputs for WKWHMRP workshop
# ============================================================================

stock   = "WHOM"
assess  = "SAM"
om      = "no_OM"
method  = "SAMHCR"

# Define function arguments ---------
Blim       <- 611814
Bpa        <- 856540
MSYBtrig   <- 856540
Fmsy       <- 0.115 # Used as Ftarget in HCR
Flow       <- Fmsy/5 # To choose if HCR 3-5
numWorm    <- 5
nyr        <- 23 # years for the forecast
niters     <- 1000

# simulation periods
per1       <- 5
per2       <- 5    # per3 is simply the remainder

# Rebuilding threshold
rebuiltThreshold <- 0.5

assessyear     <- "2019"
name           <- "WHOM_SAM19_fit"
FLStockfile    <- "WHOM_SAM19_FLS_WGWIDE.RData"
FLStockSimfile <- "WHOM_SAM19_FLS_converged.RData"
# mp             <- "MP5.10"; Fscenario = 2
mp             <- "MP5.20"; Fscenario = 5

# assessyear     <- "2020"
# name           <- "WHOM_SAM20_fit"
# FLStockfile    <- "WHOM_SAM20_FLS_WGWIDE.RData"
# FLStockSimfile <- "WHOM_SAM20_FLS_converged.RData"
# mp             <- "MP5.10"; Fscenario = 2
# mp             <- "MP5.20"; Fscenario = 5

fit            <- loadRData(file.path(RData.dir, paste0(name, ".RData")))
FLS            <- loadRData(file.path(RData.dir,FLStockfile)) %>% FLCore::setPlusGroup(., 15)
FLSs           <- loadRData(file.path(RData.dir,FLStockSimfile))
FLSs@stock     <- ssb(FLSs)

runName = paste(stock,assess,assessyear, om,niters, mp,nyr,sep="_")

# define empty lists
Stats <- Settings <- list()


minObsYear <- min(fit$data$years)
maxObsYear <- an(assessyear) - 1
obsYears   <- ac(seq(minObsYear,maxObsYear))
yStart     <- as.numeric(maxObsYear)
yEnd       <- yStart + nyr - 1
simYears   <- ac(seq(yStart,yEnd))
histYears  <- (yStart-nyr):(yStart-1)

# years for SSB-R pairs, usually the same than ones used to calculate FMSY if relevant (SR=TRUE)
SRpar <- HS_fit_fixBlim(fit, pair.years = histYears) 

# for average for bio input (M, w, etc.)
Ay    <- max(fit$data$years)+(-9:0) 

# for average for recruitment input
Ry    <- histYears 

# for selectivity input
Sy    <-max(fit$data$years)+(-9:0) 

if (grepl("SAM19",name)) TACs <- c(101672, 110381, 83954) # 2018, 2019, 2020 
if (grepl("SAM20",name)) TACs <- c(124947,  69527, 81376) # 2019, 2020, 2021 

# simulation periods
yConstraints <- 3
CU <- c(yStart, yStart+yConstraints-1)
ST <- c(yStart+yConstraints,yStart+yConstraints+(per1-1))
MT <- c(yStart+yConstraints+per1,yStart+yConstraints+(per1+per2-1))
LT <- c(yStart+yConstraints+per1+per2,yStart+nyr-1)
HI <- c(range(FLSs)[["minyear"]], min(CU)-1)

# To change between MS scenariosRW=TRUE
RW    <- FALSE # random walk on recruitment
Rdist <- FALSE # recruitment with mean and sd from sampled years
F.RW  <- FALSE # if false no RW with increase variance in F in the forecast
SR    <- TRUE # for SR relationship for rec coming from SRpar


# create settings list
Settings[["assess"]]      <- assess
Settings[["assessyear"]]  <- assessyear
Settings[["method"]]      <- method
Settings[["stock"]]       <- stock
Settings[["om"]]          <- om
Settings[["mp"]]          <- mp
Settings[["runName"]]     <- runName
Settings[["Blim"]]        <- Blim
Settings[["Bpa"]]         <- Bpa
Settings[["MSYBtrigger"]] <- MSYBtrig
Settings[["Fmsy"]]        <- Fmsy
Settings[["Flow"]]        <- Flow
Settings[["nyr"]]         <- nyr
Settings[["niters"]]      <- niters
Settings[["numworm"]]     <- numWorm
Settings[["RW"]]          <- RW
Settings[["Rdist"]]       <- Rdist
Settings[["F.RW"]]        <- F.RW
Settings[["SR"]]          <- SR
Settings[["minObsYear"]]  <- minObsYear
Settings[["maxObsYear"]]  <- maxObsYear
Settings[["obsYears"]]    <- obsYears
Settings[["yStart"]]      <- yStart
Settings[["yEnd"]]        <- yEnd
Settings[["simYears"]]    <- simYears
Settings[["histYears"]]   <- histYears
Settings[["HI"]]          <- HI
Settings[["CU"]]          <- CU
Settings[["ST"]]          <- ST
Settings[["MT"]]          <- MT
Settings[["LT"]]          <- LT

# FC <- list()
df <- data.frame(stringsAsFactors = FALSE)

set.seed(12345) # same seed to repeat output
# i <- 4
# F_targets <- c(seq(0.05,0.1,by=0.025))
F_targets <- c(1E-8, seq(0.025,0.15,by=0.025))

for (i in 1:length(F_targets)) {
  
  # Run the forecast with HCR2 and SR relationship -----
  
  ftgt <- F_targets[i]
  
  cat(i," ",ftgt,"\n")
  
  FC            <- forecast2(fit, fscale            = c(rep(NA,nyr)), 
                                  catchval          = c(TACs, rep(NA,nyr-3)),
                                  fval              = rep(NA, nyr),
                                  rec.years         = Ry, 
                                  ave.years         = Ay,
                                  overwriteSelYears = Sy, 
                                  label             = runName,
                                  nosim             = niters,
                                  deterministic     = FALSE,
                                  Fscenario         = Fscenario,
                                  MSYBtrig          = c(rep(NA,3), rep(MSYBtrig,nyr-3)), 
                                  Blim              = c(rep(NA,3), rep(Blim,nyr-3)),
                                  Fmsy              = ftgt,
                                  Flow              = Flow, #for Fscenario= 3, 4 or 5
                                  RW                = RW,
                                  Rdist             = Rdist,
                                  SR                = SR,
                                  SRpar             = SRpar,
                                  F.RW              = F.RW )   
  
  
  df <- 
    bind_rows(
      df,
      samsummary_df(FC=FC, ftgt=ftgt, Settings=Settings, FLSs = FLSs)
    )
  
} # end of i loop

lStats <- list(settings=Settings, df=df)
dir.create(path = file.path(Res.dir,"Stats"), showWarnings = TRUE, recursive = TRUE)

cat("Saving SAMHCR Stats RData file", "\n")
save(lStats,file = file.path(Res.dir,"Stats",paste0(runName,"_SAMHCR_Stats.Rdata")))


# dfy %>% filter(ftgt > 0.05 & ftgt < 0.1) %>% View()
# dfy %>% filter(ftgt > 0.05 & ftgt < 0.1) %>% View()
# dfy %>% filter(ftgt == 0.075) %>% View()
# scales::comma(unique(dfy$ftgt), accuracy=0.0001)

