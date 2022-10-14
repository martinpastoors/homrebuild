# =================================
# ----- MY FUNCTIONS ---------------

# FUN1: function to get and clean paramter names from SS3.par
clean_names <- function(myParNames){
  myParNames = melt(apply(myParNames, 1, t))
  myParNames = myParNames[complete.cases(myParNames),]
  myParNames = myParNames[!apply(myParNames, 1, function(x) any(x=="" | x=="#")),] 
  myParNames$Var3 = 1:dim(myParNames)[1]
  myParNames = myParNames[grep('^[A-Za-z]', myParNames$value),]
  myParNames$value = as.character(myParNames$value)
  myNames = NA
  for(i in 1:(dim(myParNames)[1])){
    if(i < dim(myParNames)[1]){
      temp = rep(myParNames[i,"value"], each=(myParNames$Var3[i+1]-myParNames$Var3[i]-1))} else {
        temp = rep(myParNames[i,"value"], each=1)}
    myNames = c(myNames, temp)
  }
  myNames = myNames[-c(1:2)]
  myNames = gsub(":", "", myNames) 
  return(myNames)
}


# FUN2: function to check that the new parameters are within bounds (and keep only those columns where all values are within bounds). 
checkBound <- function(bounds = bounds, new_par = new_par, verbose=FALSE){
  my_bounds = bounds[bounds$parNames %in% row.names(new_par) & !is.na(bounds$Min),]
  par_to_be_checked = new_par[row.names(new_par) %in%  my_bounds$parNames,]
  par_checked = data.frame(nrow=nrow(par_to_be_checked))
  if(!any(row.names(par_to_be_checked) %nin% my_bounds$parNames)){
    par_to_be_checked <- par_to_be_checked[order(match(row.names(par_to_be_checked) ,my_bounds$parNames)),]
    #par_to_be_checked[my_bounds$parNames,]
    for(i in 1:ncol(par_to_be_checked)){
      if(any(par_to_be_checked[,i]<my_bounds$Min) | any(par_to_be_checked[,i]>my_bounds$Max)){
        if(verbose){print(paste("Column", i, "is outside bounds", sep=" "))} }
      else {
        par_checked = cbind(par_checked, par_to_be_checked[,i])
      }
    }
  } else {
    print("Names order doesn't match")
  }
  return(par_checked)
}

# FUN3: function to read the par file 
SS_readpar <- function (dir=getwd(), file = "ss.par", verbose = TRUE, n_recdev_early=15, n_recdev1=36) 
{
  if (verbose) 
    cat("running SS_readpar\n")
  size <- file.info(file.path(dir,file))$size
  if (is.na(size) || size == 0) 
    stop("file empty or missing:", file)
  par <- readLines(file.path(dir,file), warn = F)
  mylist <- list()
  mylist$sourcefile <- file.path(dir, file)
  mylist$type <- "Stock_Synthesis_par_file"
  mylist$SSversion <- "SSv3.30b_or_later"
  par1 <- as.data.frame(par)
  allnums <- NULL
  for (i in 1:length(par)) {
    mysplit <- strsplit(par[i], split = "[[:blank:]]+")[[1]]
    mysplit <- mysplit[mysplit != ""]
    nums <- suppressWarnings(as.numeric(mysplit))
    if (sum(is.na(nums)) > 0) 
      maxcol <- min((1:length(nums))[is.na(nums)]) - 1
    else maxcol <- length(nums)
    if (maxcol > 0) {
      nums <- nums[1:maxcol]
      allnums <- c(allnums, nums)
    }
  }
  i <- 1
  mylist$"dummy_parm" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[1]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[2]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[3]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[4]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[5]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[6]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[7]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[8]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[9]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[10]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[11]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[12]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[13]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[14]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[15]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[16]" <- allnums[i]
  i <- i + 1
  mylist$"MGparm[17]" <- allnums[i]
  i <- i + 1
  mylist$"SR_parm[1]" <- allnums[i]
  i <- i + 1
  mylist$"SR_parm[2]" <- allnums[i]
  i <- i + 1
  mylist$"SR_parm[3]" <- allnums[i]
  i <- i + 1
  mylist$"SR_parm[4]" <- allnums[i]
  i <- i + 1
  mylist$"SR_parm[5]" <- allnums[i]
# i <- i + 1
#  mylist$"SR_parm[6]" <- allnums[i]
  i <- i + 1
  mylist$"recdev_early" <- allnums[i:(i+n_recdev_early-1)]
  i <- i + n_recdev_early
  mylist$"recdev1" <- allnums[i:(i+n_recdev1-1)]
  i <- i + n_recdev1
  mylist$"Fcast_recruitments"  <- allnums[i:(i+4-1)]
  i <- i + 4
  mylist$"Fcast_impl_error"  <- allnums[i:(i+3-1)]
  i <- i + 1
  mylist$"init_F[1]"  <- allnums[i]
  i <- i + 1
  mylist$"Q_parm[1]"  <- allnums[i]
  i <- i + 1
  mylist$"Q_parm[2]"  <- allnums[i]
  i <- i + 1
  mylist$"Q_parm[3]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[1]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[2]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[3]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[4]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[5]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[6]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[7]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[8]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[9]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[10]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[11]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[12]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[13]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[14]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[15]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[16]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[17]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[18]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[19]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[20]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[21]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[22]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[23]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[24]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[25]"  <- allnums[i]
  i <- i + 1
  mylist$"selparm[26]"  <- allnums[i]
  i <- i + 1
  mylist$"checksum999"  <- allnums[i]
  return(mylist)
}

# FUN4: write new par files 
SS_writepar <- function(mylist, myParSet, dir = NULL, file = "ss.par", overwrite = TRUE, 
                        verbose = TRUE, warn = TRUE, n_recdev_early=15, n_recdev1=36) 
{
  on.exit({
    if (sink.number() > 0) sink()
  })
  if (is.null(dir)) 
    dir <- getwd()
  if (grepl("/$", dir)) {
    outfile <- paste0(dir, file)
  } else {
    outfile <- paste(dir, file, sep = "/")
  }
  if (file.exists(outfile)) {
    if (!overwrite) {
      stop(paste("file exists:", outfile, "\n  set overwrite=TRUE to replace\n"))
    }
    else {
      if (warn) {
        cat("overwriting file:", outfile, "\n")
      }
      file.remove(outfile)
    }
  }
  oldwidth <- options()$width
  options(width = 1000)
  if (verbose) 
    cat("opening connection to", outfile, "\n")
  zz <- file(outfile, open = "at")
  sink(zz)
  wl <- function(name) {
    #value = mylist[[name]]
    value = as.numeric(mylist[names(mylist) == name])
    writeLines(paste0("# ", name, ":"))
    writeLines(paste0(value), con = zz)
  }
  writeLines("# par file written by R function SS_writepar")
  writeLines(paste("# number of parameters changed:", dim(myParSet)[1]))
  writeLines(paste("# file write time:", Sys.time()))
  writeLines(paste("# dummy_parm:"))
  writeLines(paste0("1.000"), con=zz)
  wl("MGparm[1]")
  wl("MGparm[2]")
  wl("MGparm[3]")
  wl("MGparm[4]")
  wl("MGparm[5]")
  wl("MGparm[6]")
  wl("MGparm[7]")
  wl("MGparm[8]")
  wl("MGparm[9]")
  wl("MGparm[10]")
  wl("MGparm[11]")
  wl("MGparm[12]")
  wl("MGparm[13]")
  wl("MGparm[14]")
  wl("MGparm[15]")
  wl("MGparm[16]")
  wl("MGparm[17]")
  wl("SR_parm[1]")
  wl("SR_parm[2]")
  wl("SR_parm[3]")
  wl("SR_parm[4]")
  wl("SR_parm[5]")
  writeLines(paste("# recdev_early:"))
  writeLines(paste(mylist[["recdev_early"]]), sep=" ")
  writeLines(paste("\n", "# recdev1:", sep=""))
  writeLines(paste(mylist[["recdev1"]]), sep=" ")
  writeLines(paste("\n", "# Fcast_recruitments:", sep=""))
  writeLines(paste(mylist[["Fcast_recruitments"]]), sep=" ")
  # for(i in 1:n_recdev_early){
  #   writeLines(paste(as.numeric(mylist[names(mylist) == "recdev_early"][[1]][i]), sep=""), sep=" ", con = zz)}
  # writeLines(paste("\n", "# recdev1"))
  # for(i in 1:n_recdev1){
  #   writeLines(paste(as.numeric(mylist[names(mylist) == "recdev1"][[1]][i]), sep=""), sep=" ", con = zz)}
  # writeLines(paste("\n", "# Fcast_recruitments"))
  # for(i in 1:4){
  #   writeLines(paste(as.numeric(mylist[names(mylist) == "Fcast_recruitments"][[1]][i]), sep=""), sep=" ", con = zz)}
  #writeLines(paste("\n", "# Fcast_impl_error"))
  #for(i in 1:3){
  #  writeLines(paste(as.numeric(mylist[names(mylist) == "Fcast_impl_error"][[1]][i]), sep=""), sep=" ", con = zz)}
  writeLines(paste("\n"), sep=" ")
  wl("Fcast_impl_error")  
  wl("init_F[1]")
  wl("Q_parm[1]")
  wl("Q_parm[2]")
  wl("Q_parm[3]")
  wl("selparm[1]")
  wl("selparm[2]")
  wl("selparm[3]")
  wl("selparm[4]")
  wl("selparm[5]")
  wl("selparm[6]")
  wl("selparm[7]")
  wl("selparm[8]")
  wl("selparm[9]")
  wl("selparm[10]")
  wl("selparm[11]")
  wl("selparm[12]")
  wl("selparm[13]")
  wl("selparm[14]")
  wl("selparm[15]")
  wl("selparm[16]")
  wl("selparm[17]")
  wl("selparm[18]")
  wl("selparm[19]")
  wl("selparm[20]")
  wl("selparm[21]")
  wl("selparm[22]")
  wl("selparm[23]")
  wl("selparm[24]")
  wl("selparm[25]")
  wl("selparm[26]")
  writeLines(paste("# checksum999:"))
  writeLines(paste0("999"), con=zz)
  options(width = oldwidth)
  sink()
  close(zz)
  if (verbose) 
    cat("file written to", outfile, "\n")
}

# FUN5: function prepare the new .par file and run the assessment for all the different parameter combination. 
SS_doPar <- function (oldsubdir=inpDir, parNames, parBound, myParSet, newsubdir = file.path("random_pop", "Set_newParameter"), 
                      subdirstart = "newParms",n_parSets = 1:100, overwrite = TRUE, extras = "-nox", #GL -nohess can be included in extras as "-nox -nohess" to run without hessian
                      intern = FALSE, CallType = "system", RemoveBlocks = FALSE) 
{
  oldwd <- getwd()
  on.exit(setwd(oldwd))
  olddir <- oldsubdir
  newdir <- file.path(inpDir, newsubdir)
  
  # print(newdir)
  
  exefile     <- dir(olddir)[grep(".exe", dir(olddir))]
  startfile   <- dir(olddir)[tolower(dir(olddir)) == "starter.ss"]
  forefile    <- dir(olddir)[tolower(dir(olddir)) == "forecast.ss"]
  wtatagefile <- dir(olddir)[tolower(dir(olddir)) == "wtatage.ss"]
  testfile    <- dir(olddir)[tolower(dir(olddir)) == "test.ss"]
  parfile     <- dir(olddir)[dir(olddir) == "SS3.par"]
  if (length(startfile) == 0) 
    stop("No starter.ss file found in ", olddir)
  startfile   <- file.path(olddir, startfile)
  cat("Get input file names from starter file:", startfile, 
      "\n")
  starter     <- SS_readstarter(startfile, verbose = FALSE)
  ctlfile     <- starter$ctlfile
  #control <- SS_readctl_3.30(file=file.path(olddir, ctlfile), nseas = 1, N_areas = 1, Nages = 20, Ngenders = 1, Npopbins = NA, Nfleet = 1, Nsurveys = 4)
  datfile     <- starter$datfile
  filenames   <- c(exefile, forefile, ctlfile, datfile, wtatagefile, 
                   testfile, parfile)
  cat("copying model files from\n", olddir, "\nto\n", newdir, 
      "\n")
  cat("model files to copy:", filenames, sep = "\n ")
  if (!file.exists(newdir)) 
    dir.create(newdir)
  # subdirnames <- paste(subdirstart, n_parSets, sep = "")
  newParFile <- list()
  mypar <- list()

      
  # ia <- n_parSets[1] 
  for (ia in n_parSets) {
    subdirnames <- paste(subdirstart, ia, sep = "")
    # if (!file.exists(file.path(newdir, subdirnames[ia]))) 
    if (!file.exists(file.path(newdir, subdirnames))) 
      dir.create(file.path(newdir, subdirnames))
    file.copy(file.path(olddir, filenames), file.path(newdir, 
                                                      subdirnames, filenames), overwrite = TRUE)
    starter$init_values_src <- 1
    starter$last_estimation_phase <- 0
    # control$MG_parms$PHASE <- abs(control$MG_parms$PHASE)*-1 
    # control$SRparm$PHASE <- abs(control$SRparm$PHASE)*-1 
    # control$init_F$PHASE <- abs(control$init_F$PHASE)*-1 
    # control$size_selex_parms$PHASE <- abs(control$size_selex_parms$PHASE)*-1 
    # control$age_selex_parms$PHASE <- abs(control$age_selex_parms$PHASE)*-1 
    
    setwd(file.path(newdir, subdirnames))
    SS_writestarter(starter, dir = getwd(), verbose = FALSE, 
                    overwrite = TRUE)
    #SS_writectl_3.24(control, outfile = "WHOM.ctl", verbose = FALSE, nseas = 1, N_areas = 1, overwrite = TRUE)
    
    #for(i in 1:ncol(newParSet)){
    #notEstPar <- parBound[parBound[,6] %nin% newParSet[,1],c(6,1)]
    #names(notEstPar)
    for(ipar in 1:(length(parNames)-1)){
      if(parBound$Phase[ipar]<0 | is.na(parBound$Phase[ipar])){
        mypar[[ipar]] <- parBound[which(parBound[,6]==parNames[ipar]), "Value"]
        #names(mypar[[i]]) <- parBound[i,6]
      } else {
        mypar[[ipar]] <- myParSet[which(myParSet$par==as.character(parBound[ipar,6])),ia]
        #names(mypar[[i]]) <- parBound[i,6]
      }
    }
    names(mypar) <- as.character(parBound[,6])
    newParFile[[ia]] <- mypar
    #mypar[rownames(newParSet)] <- newParSet[,ia]
    #newParFile[[ia]] <- mypar
    #   }
    SS_writepar(mylist=newParFile[[ia]], myParSet=myParSet[ia],dir = getwd()) 
    cat("Running model in ", getwd(), "\n", sep = "")
    if (file.exists("covar.sso")) 
      file.remove("covar.sso")
    if (intern) 
      cat("ADMB output generated during model run will be written to:\n   ", 
          getwd(), "/ADMBoutput.txt. \n   To change this, set intern=FALSE\n", 
          "Note: ignore message about 'Error trying to open data input file ss3.dat'\n", 
          sep = "")
    if (CallType == "system") 
      ADMBoutput <- system(paste(exefile, extras), intern = intern)
    if (CallType == "shell") 
      ADMBoutput <- shell(paste(exefile, extras), intern = intern)
    if (intern) 
      writeLines(c("###", "ADMB output", as.character(Sys.time()), 
                   "###", " ", ADMBoutput), con = "ADMBoutput.txt")
    setwd("..")
  }
  setwd(oldwd)
}


# FUN 6: problem with the function SS_readctl (different arguments specification between that one and the function SS_readctl_3.24 
# which is called internally), so I modified it (new parameters specifications are Nfleet and Nsurveys instead of Nfish and Nsurv)
SS_readctl_PC <- function (file, version = "3.24", verbose = TRUE, echoall = FALSE, 
          nseas = 4, N_areas = 1, Nages = 20, Ngenders = 1, Npopbins = NA, 
          Nfleet = 2, Nsurveys = 2, TG_Nrelgrp = NA) 
{
  if (version == "3.24") {
    ctllist <- SS_readctl_3.24(file = file, verbose = verbose, 
                               echoall = echoall, nseas = nseas, N_areas = N_areas, 
                               Nages = Nages, Ngenders = Ngenders, Npopbins = Npopbins, 
                               Nfleet = Nfleet, Nsurveys = Nsurveys, TG_Nrelgrp = TG_Nrelgrp)
  }
  if (version == "3.30" | version == 3.3) {
    stop("Function SS_readctl_3.30 has not been written yet")
  }
  return(ctllist)
}

# ---------------------------------
# =================================
