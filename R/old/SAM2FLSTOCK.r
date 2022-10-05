# ---------------------------------------------------------------------------------------------------
# SAM2FLSTOCK.r
# 
# Convert SAM fit object and data files to FLSTOCK object
# ---------------------------------------------------------------------------------------------------

library(FLCore)
library(FLSAM)
library(stockassessment)

# temp <- "D:/temp"
# sao.name <- "MackWGWIDE2019v02"
# sao.name="WHOM_2018"

SAM2FLSTOCK <- function(sao.name, temp="D:\\temp") {
  
  ## get fit object ##############################################################################
  
  url     <- paste0("https://stockassessment.org/datadisk/stockassessment/userdirs/user3/",sao.name,"/")
  
  tempdir <- file.path(temp,sao.name) 
    
  fit <- stockassessment::fitfromweb(sao.name, character.only=TRUE) 
  
  ## download data files #######################################################################
  
  dir.create(tempdir, showWarnings = TRUE)
  write.data.files(fit$data, dir = tempdir)
  
  ## Create FLSAM object ##############################################################################
  
  res <- SAM2FLSAM(sao.name)
  
  ## Create FLSTOCK object ##############################################################################
  
  inputs <- c(landings.fraction='lf.dat', catch.n='cn.dat', catch.wt='cw.dat',
              discards.wt='dw.dat', landings.wt='lw.dat', stock.wt='sw.dat',
              mat='mo.dat', m='nm.dat', harvest.spwn='pf.dat', m.spwn='pm.dat')
  
  fqs <- as.list(inputs)
  
  for (i in seq(inputs)) {
    # iurl <- paste(url, "data", inputs[i], sep="/")
    # file <- tempfile()
    # download.file(iurl, file)
    file <- file.path("D:/temp",sao.name,inputs[i])
    fqs[[names(inputs[i])]] <- readVPAFile(file)
  }
  
  # CREATE FLStock, drop landings.fraction
  # ERROR: this is falling over for the mackerel assessment
  # Error in if (any(range(object, c("minyear", "maxyear")) != 
  #    dms[c("minyear",  : missing value where TRUE/FALSE needed
                                                                   
  fls <- do.call("FLStock", fqs[-1])

  # DEBUG CALCULATE landings.n and discards.n
  fls@landings.n <- fqs[["catch.n"]] * fqs[["landings.fraction"]]
  fls@discards.n <- fqs[["catch.n"]] * (1-fqs[["landings.fraction"]])
  
  # COMPUTE totals
  landings(fls) <- computeLandings(fls)
  discards(fls) <- computeDiscards(fls)
  catch(fls) <- computeCatch(fls)
  
  # add stock numbers and harvest from FLSAM
  stock.n(fls) <- res@stock.n
  harvest(fls) <- res@harvest
  stock(fls) <- computeStock(fls)
  
  # SET to standard units
  units(fls) <- standardUnits(fls)

  # DEBUG GET fbar range
  fls@range["minfbar"] <- fit$conf$fbarRange[1]
  fls@range["maxfbar"] <- fit$conf$fbarRange[2]
  
  # SET name and desc
  name(fls) <- sao.name
  desc(fls) <- paste("Loaded from", url)
  
  # LOAD extra args
  # args <- list(...)
  # if(length(args > 0))
  #   for(a in seq(args))
  #     slot(fls, names(args)[a]) <- args[a]
  
  # DONE
  return(fls)
  
  # clean up data files; remove temp directory
  unlink(tempdir, recursive=TRUE)
  
}

# EXAMPLE
# t        <- SAM2FLSTOCK(sao.name="WHOM_2019", temp="D:/temp")
# t        <- SAM2FLSTOCK(sao.name="ARU.27.5b6a_WGDEEP_2020_", temp="D:/temp")
# plot(t)

# WG19SAM   <- SAM2FLSTOCK(sao.name="WHOM_2019", temp="D:/temp")
# save(WG19SAM, file="D:/temp/WGWIDE2019SAM.RData")
