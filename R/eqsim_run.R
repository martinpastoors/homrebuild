#' Simulates the Equilibrium Results for a Population.
#'
#' Simulate a fish stock forward in time given biological parameters, fishery
#' parameters and advice parameters.
#'
#' @param fit A list returned from the function fitModels
#' @param bio.years The years to sample maturity, weights and M from, given as
#'                  a vector of length 2, i.e. c(2010, 2015) select from the
#'                  years 2010 to 2015 inclusive.
#' @param bio.const A flag (default FALSE), if TRUE mean of the biological values from the
#'                  years selected are used
#' @param sel.years The years to sample the selection patterns from, given as
#'                  a vector of length 2, i.e. c(2010, 2015) select from the
#'                  years 2010 to 2015 inclusive.
#' @param sel.const A flag (default FALSE), if TRUE mean of the selection patterns from the
#'                  years selected are used
#' @param Fscan F values to scan over, i.e. seq(0, 2, by = 0.05)
#' @param Fcv Assessment error in the advisory year
#' @param Fphi Autocorrelation in assessment error in the advisory year
#' @param SSBcv Spawning stock biomass error in the advisory year
#' @param rhologRec A flag for recruitment autocorrelation, default (TRUE), or a
#'                  vector of numeric values specifcying the autocorrelation
#'                  parameter for the residuals for each SR model.
#' @param Blim SSB limit reference point
#' @param Bpa SSB precuationary reference point
#' @param recruitment.trim A numeric vector with two log-value clipping the
#'        extreme recruitment values from a continuous lognormal distribution.
#'        The values must be set as c("high","low").
#' @param Btrigger If other than 0 (default) the target F applied is reduced by
#'                 SSB/Btrigger. This is the "ICES Advice Rule".
#' @param Nrun The number of years to run in total (the last 50 years from that
#'             will be retained to compute equilibrium values from)
#' @param process.error Use stochastic recruitment or mean recruitment?
#'                      TRUE (default) uses the predictive distribution of recruitment,
#'                      model estimate of recruitment + simulated observation
#'                      error.  FALSE uses model prediction of recruitment with
#'                      no observation error.
#' @param verbose Flag, if TRUE (default) indication of the progress of the
#'        simulation is provided in the console. Useful to turn to FALSE when
#'        knitting documents.
#' @param extreme.trim a pair of quantiles (low, high) which are used to trim
#'                     the equilibrium catch values, across simulations within
#'                     an F scenario, when calculating the mean catch and
#'                     landings for that F scenario.  These mean values
#'                     calculated accross simulations within an F scenario
#'                     are used to find which F scenario gave the maximum catch.
#'                     \code{extreme.trim} can therefore be used to stablise the
#'                     estimate of mean equilibrium catch and landings by F
#'                     scenario.  The default is c(0, 1) which includes all the
#'                     data and is effectively an untrimmed mean.
#' @param R.initial Initial recruitment for the simulations.  This is common
#'                  accross all simulations. Default = mean of all recruitments
#'                  in the series.
#' @param keep.sims Flag, if TRUE returns a matrix of population tragectories
#'                  for each value of F in Fscan.
#' @return
#' A list containing the results from the forward simulation and the reference
#' points calculated from it.
#'
#' @details
#' Details of the steps required to evaluate reference points are given in
#' ICES (2017).  WHile, details of the calculation of MSY ranges is given in
#' ICES (2015).
#'
#' @references
#' ICES (2015) Report of the Workshop to consider F MSY ranges for stocks in
#' ICES categories 1 and 2 in Western Waters (WKMSYREF4).
#' \href{http://ices.dk/sites/pub/Publication\%20Reports/Expert\%20Group\%20Report/acom/2015/WKMSYREF4/01\%20WKMSYREF4\%20Report.pdf}{01
#' WKMSYREF4 Report.pdf}
#'
#' ICES (2017) ICES fisheries management reference points for category 1 and 2
#' stocks.
#' DOI: \href{https://doi.org/10.17895/ices.pub.3036}{10.17895/ices.pub.3036}
#'
#' @seealso
#'
#' \code{\link{eqsr_fit}} fits multiple stock recruitment models to a data set.
#'
#' \code{\link{eqsr_plot}} plots the results from eqsr_fit.
#'
#' \code{\link{eqsim_plot}} summary plot of the forward simulation showing estimates
#'   of various reference points.
#'
#' \code{\link{eqsim_plot_range}} summary plots of the forward simulation showing
#'   the estimates of MSY ranges (ICES, 2015)
#'
#' \code{\link{msy-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' data(icesStocks)
#' FIT <- eqsr_fit(icesStocks$saiNS,
#'                 nsamp = 1000,
#'                 models = c("Ricker", "Segreg"))
#' SIM <-
#'   eqsim_run(
#'     FIT,
#'     bio.years = c(2004, 2013),
#'     sel.years = c(2004, 2013),
#'     Fcv = 0.24,
#'     Fphi = 0.42,
#'     Blim = 106000,
#'     Bpa = 200000,
#'     Fscan = seq(0, 1.2, len = 40)
#'    )
#' }
#'
#' @export
eqsim_run <- function(fit,
                      bio.years = c(-5, -1) + FLCore::dims(fit$stk)$maxyear, # years sample weights, M and mat
                      bio.const = FALSE,
                      sel.years= c(-5, -1) + FLCore::dims(fit$stk)$maxyear, # years sample sel and discard proportion by number from
                      sel.const = FALSE,
                      Fscan = seq(0, 2, len = 40), # F values to scan over
                      Fcv = 0,
                      Fphi = 0,
                      SSBcv = 0,
                      SSBphi = 0,
                      rhologRec = TRUE,
                      Blim,
                      Bpa,
                      recruitment.trim = c(3, -3),
                      Btrigger = 0,
                      Nrun = 200, # number of years to run in total
                      process.error = TRUE, # use predictive recruitment or mean recruitment? (TRUE = predictive)
                      verbose = TRUE,
                      extreme.trim = c(0, 1),
                      R.initial = mean(fit$rby$rec),
                      keep.sims = FALSE,
                      calc.RPs = TRUE,   #AC - calculate the reference points?
                      dfExplConstraints = NULL,    #AC - exploration constraints (F, Catch, minTAC)
                      HCRName = NA)         #AC HCR function
{
  
  #AC simulated stocks to output
  simStks <- list()
  #AC HCR function
  if (!is.na(HCRName)){fManagement <- fReadHCR(HCRName)}
  
  #Exploitation constraints
  dfExplConstraints <- 
    dplyr::filter(dfExplConstraints,toupper(YearNum)=="ALL") %>%
    ungroup() %>%                   # 02/10/2022 removed the group command; group_by(Type) %>%
    tidyr::expand(Type=Type,YearNum=seq(1,Nrun),Val=Val) %>%
    ungroup() %>%
    rbind(filter(dfExplConstraints,!toupper(YearNum)=="ALL")) %>%
    mutate(YearNum = as.integer(YearNum))
  
  if (abs(Fphi) >= 1) stop("Fphi, the autocorelation parameter for log F should be between (-1, 1)")
  if (diff(recruitment.trim) == 0) stop("recruitment truncation must be given as c(high, low)")
  # commented out as above line is a better check
  # if ((recruitment.trim[1] + recruitment.trim[2]) > 0) stop("recruitment truncation must be between a high - low range")
  
  if (verbose) icesTAF::msg("Setting up...")
  
  #browser()
  
  btyr1 <- bio.years[1]
  btyr2 <- bio.years[2]
  slyr1 <- sel.years[1]
  slyr2 <- sel.years[2]
  # Keep at most 50 simulation years (which will be the last 50 of the Nrun
  #  forward simulated years)
  keep <- min(Nrun, 50)
  
  SR <- fit $ sr.sto
  data <- fit $ rby[,c("rec","ssb","year")]
  stk <- fit $ stk
  #1000 iterations
  stks <- fit$stks
  
  # forecast settings (mean wt etc)
  stk.win    <- FLCore::window(stk, start = btyr1, end = btyr2)
  stk.winsel <- FLCore::window(stk, start = slyr1  , end = slyr2)
  
  # iterations
  stks.win    <- FLCore::window(stks, start = btyr1, end = btyr2)
  stks.winsel <- FLCore::window(stks, start = slyr1, end = slyr2)
  
  littleHelper <- function(x,i) {
    x2 <- x
    x2[i] <- NA
    x2[] <- apply(x2,1,mean,na.rm=TRUE)
    x[i] <- x2[i]
    return(x)
  }
  
  # get ready for the simulations
  Nmod <- nrow(SR)
  NF <- length(Fscan)
  ages <- FLCore::dims(stk)$age
  tyr <- range(fit$stk)["maxyear"]    #terminal assessment year
  ssb_lag <- fit$rby$ssb_lag[1]
  
  #iteration specific weights
  #west <- matrix(FLCore::stock.wt(stk.win), ncol = btyr2 - btyr1 + 1)
  #i <- weca == 0
  #if(any(i)) weca <- littleHelper(weca,i)
  
  west <- array(NA, c(ages,btyr2-btyr1+1,Nmod),dimnames=list(age=(range(stk)[1]:range(stk)[2]),year=seq(btyr1,btyr2),iter=1:Nmod))
  for (ii in 1:Nmod){
    west[,,ii] <- matrix(FLCore::stock.wt(stks.win[,,,,,ii]), ncol = btyr2 - btyr1 + 1)
    i <- west[,,ii] == 0
    if(any(i)) west[,,ii] <- littleHelper(west[,,ii],i)
    if (bio.const == TRUE) {west[,,ii] <- rowMeans(west[,,ii])}
  }
  
  #weca <- matrix(FLCore::catch.wt(stk.win), ncol = btyr2 - btyr1 + 1)
  #i <- weca == 0
  #if(any(i)) weca <- littleHelper(weca,i)
  
  weca <- array(NA, c(ages,btyr2-btyr1+1,Nmod),dimnames=list(age=(range(stk)[1]:range(stk)[2]),year=seq(btyr1,btyr2),iter=1:Nmod))
  for (ii in 1:Nmod){
    weca[,,ii] <- matrix(FLCore::catch.wt(stks.win[,,,,,ii]), ncol = btyr2 - btyr1 + 1)
    i <- weca[,,ii] == 0
    if(any(i)) weca[,,ii] <- littleHelper(weca[,,ii],i)
    if (bio.const == TRUE) {weca[,,ii] <- rowMeans(weca[,,ii])}
  }
  
  
  #wela <- matrix(FLCore::landings.wt(stk.win), ncol = btyr2 - btyr1 + 1)
  #if(any(i)) wela <- littleHelper(wela,i)
  
  wela <- array(NA, c(ages,btyr2-btyr1+1,Nmod),dimnames=list(age=(range(stk)[1]:range(stk)[2]),year=seq(btyr1,btyr2),iter=1:Nmod))
  for (ii in 1:Nmod){
    wela[,,ii] <- matrix(FLCore::landings.wt(stks.win[,,,,,ii]), ncol = btyr2 - btyr1 + 1)
    i <- wela[,,ii] == 0
    if(any(i)) wela[,,ii] <- littleHelper(wela[,,ii],i)
    if (bio.const == TRUE) {wela[,,ii] <- rowMeans(wela[,,ii])}
  }
  
  
  Mat <- matrix(FLCore::mat(stk.win), ncol = btyr2 - btyr1 + 1)
  M <- matrix(FLCore::m(stk.win), ncol = btyr2 - btyr1 + 1)
  landings <- matrix(FLCore::landings.n(stk.winsel), ncol = slyr2 - slyr1 + 1)
  # if zero, use 0.10 of minimum value
  
  catch <- matrix(FLCore::catch.n(stk.winsel), ncol = slyr2 - slyr1 + 1)
  sel <- matrix(FLCore::harvest(stk.winsel), ncol = slyr2 - slyr1 + 1)
  Fbar <- matrix(FLCore::fbar(stk.winsel), ncol = slyr2 - slyr1  + 1)
  sel <- sweep(sel, 2, Fbar, "/")
  
  if (sel.const == TRUE) { # take means of selection
    sel[] <- apply(sel, 1, mean)
    landings[]  <- apply(landings, 1, mean)
    catch[]  <- apply(catch, 1, mean)
  }
  
  #iteration specific selection
  sels <- array(NA, c(ages,slyr2-slyr1+1,Nmod),dimnames=list(age=(range(stk)[1]:range(stk)[2]),year=seq(slyr1,slyr2),iter=1:Nmod))
  Fbars <- array(NA, c(ages,slyr2-slyr1+1,Nmod),dimnames=list(age=(range(stk)[1]:range(stk)[2]),year=seq(slyr1,slyr2),iter=1:Nmod))
  for (ii in 1:Nmod){
    sels[,,ii] <- matrix(FLCore::harvest(stks.winsel[,,,,,ii]), ncol = slyr2 - slyr1 + 1)
    Fbars[,,ii] <- matrix(FLCore::fbar(stks.winsel[,,,,,ii]), ncol = slyr2 - slyr1  + 1, nrow = ages, byrow = TRUE)
    sels[,,ii] <- sels[,,ii]/Fbars[,,ii]
    if (sel.const == TRUE) {sels[,,ii] <- rowMeans(sels[,,ii])}
  }
  
  # 22.2.2014 Added weight of landings per comment from Carmen
  if (bio.const==TRUE){ # take means of wts Mat and M and ratio of landings to catch
    #west[] <- apply(west, 1, mean) - dealt with above
    #weca[] <- apply(weca, 1, mean) - dealt with above
    #wela[] <- apply(wela, 1, mean) - dealt with above
    Mat[] <- apply(Mat, 1, mean)
    M[] <- apply(M, 1, mean) #me
  }
  
  land.cat= landings / catch  # ratio of number of landings to catch
  
  # TODO: Check if this is sensible
  i <- is.na(land.cat)
  if(any(i)) land.cat[i] <- 1
  
  Fprop <- apply(FLCore::harvest.spwn(stk.winsel), 1, mean)[drop=TRUE] # vmean(harvest.spwn(stk.win))
  Mprop <- apply(FLCore::m.spwn(stk.win), 1, mean)[drop=TRUE] # mean(m.spwn(stk.win))
  
  ssby <- ssby.obs <- array(NA, c(Nrun,Nmod),dimnames=list(year=1:Nrun,iter=1:Nmod))
  Ferr <- SSBerr <- Fmgmt <- Fmgmt_err <- TAC <- array(0, c(Nrun,Nmod),dimnames=list(year=1:Nrun,iter=1:Nmod))
  Ny <- Ny2 <- Fy <- WSy <- Maty <- WCy <- Cy <- Wy <- Wl <- Ry <- sely <-
    array(0, c(ages, Nrun, Nmod),
          dimnames = list(age = (range(stk)[1]:range(stk)[2]),
                          year = 1:Nrun,
                          iter = 1:Nmod))
  # TODO per note from Carmen:
  #  NOTE: If we want Ferr to be a stationary AR(1) process, it would make
  #        more sense to initialise Ferr as a Normal dist with zero mean and
  #        standard deviation of AR(1) marginal distribution, i.e. standard
  #        deviation of initial Ferr = Fcv/sqrt(1- Fphi^2), instead of just
  #        initialising Ferr=0
  #  2014-03-12: Changed per note form Carmen/John
  Ferr[1,] <- stats::rnorm(n=Nmod, mean=0, sd=1)*Fcv/sqrt(1-Fphi^2)
  for(j in 2:Nrun)
    Ferr[j,] <- Fphi * Ferr[j-1,] + Fcv * stats::rnorm(n = Nmod, mean = 0, sd = 1)
  
  #2014-03-12: Changed per note form Carmen/John
  #Errors in SSB: this is used when the ICES MSY HCR is applied for F
  #SSBerr <- matrix(stats::rnorm(n = Nrun * Nmod, mean = 0, sd = 1), ncol = Nmod) * SSBcv
  #SSBphi ?
  SSBerr[1,] <- stats::rnorm(n=Nmod, mean=0, sd=1)*SSBcv/sqrt(1-SSBphi^2)
  for(j in 2:Nrun)
    SSBerr[j,] <- SSBphi * SSBerr[j-1,] + SSBcv * stats::rnorm(n = Nmod, mean = 0, sd = 1)
  
  #browser()
  
  rsam <- array(sample(1:ncol(weca), Nrun * Nmod, TRUE), c(Nrun, Nmod))
  rsamsel <- array(sample(1:ncol(sel), Nrun * Nmod, TRUE), c(Nrun, Nmod))
  #Wy[] <- c(weca[, c(rsam)])   - populated by iteration
  #WSy[] <- c(west[, c(rsam)])   - populated by iteration
  #Wl[] <- c(wela[, c(rsam)])   - populated by iteration
  Maty[] <- c(Mat[ ,c(rsam)])
  Ry[]  <- c(land.cat[, c(rsamsel)])
  
  # initial recruitment
  R <- R.initial
  ssbs <- cats <- lans <- recs <- array(0, c(7, NF))
  
  ferr <- ssbsa <- catsa <- lansa <- recsa <- array(0, c(NF, keep, Nmod))
  begin <- Nrun - keep + 1
  
  # New from Simmonds' 29.1.2014
  #   Residuals of SR fits (1 value per SR fit and per simulation year
  #     but the same residual value for all Fscan values):
  resids= array(stats::rnorm(Nmod*(Nrun+1), 
                             0, 
                             SR$cv),
                c(Nmod, Nrun+1))
  
  # 2014-03-12: Changed per note form Carmen/John
  #  Autocorrelation in Recruitment Residuals:
  if(rhologRec==TRUE){
    fittedlogRec <-  do.call(cbind, lapply( c(1:nrow(fit$sr.sto)), function(i){
      FUN <- match.fun(fit$sr.sto$model[i])
      FUN(fit$sr.sto[i, ], fit$rby$ssb) } )  )
    # Calculate lag 1 autocorrelation of residuals:
    rhologRec <- apply(log(fit$rby$rec)-fittedlogRec, 2, function(x){stats::cor(x[-length(x)],x[-1])})
  }
  
  if (is.numeric(rhologRec)) {
    # Draw residuals according to AR(1) process:
    for(j in 2:(Nrun+1)){ resids[,j] <- rhologRec * resids[,j-1] + resids[,j]*sqrt(1 - rhologRec^2) }
  }
  
  
  # Limit how extreme the Rec residuals can get:
  lims = t(array(SR$cv,c(Nmod,2))) * recruitment.trim
  for (k in 1:Nmod) { resids[k,resids[k,]>lims[1,k]]=lims[1,k]}
  for (k in 1:Nmod) { resids[k,resids[k,]<lims[2,k]]=lims[2,k]}
  # end New from Simmonds 29.1.2014
  
  if (verbose) icesTAF::msg("Running forward simulations.")
  if (verbose) loader(0)
  cat("\n")
  
  #browser()
  
  # Looping over each F value in Fscan. For each of the Nmod SR fits
  # (replicates), do a forward simulation during Nrun years
  # There are Rec residuals for each SR fit and year, which take the same
  # values for all Fscan
  
  fNAifEmpty <- function(x) {
    if (length(x)==0) {ret <- NA} else {ret <- x}
    return(ret)
  }
  
  # Loop over Fs
  # i <- 1
  for (i in 1:NF) {
    
    #browser()
    
    #The default F value to implement
    Fbar <- Fscan[i]
    
    ############################################################################
    #Population in simulation year 1 (Jan1)
    #Ny[,1,] <- unlist(lapply(lapply(fit$stks,FUN=FLCore::stock.n),'[',,ac(range(fit$stk)["maxyear"])))
    Ny[,1,] <- array(as.numeric(stock.n(stks[,ac(tyr)])),dim=c(ages,Nmod))   #FLStock iter version
    
    #ssby[1,] <- unlist(lapply(lapply(fit$stks,FUN=FLCore::ssb),'[',,ac(range(fit$stk)["maxyear"])))
    #ssby.obs[1,] <- ssby[1,]*exp(SSBerr[1,])
    #FLStock iterations version
    ssby[1,]     <- as.numeric(ssb(stks[,ac(tyr)]))[1:niters]
    ssby.obs[1,] <- ssby[1,]*exp(SSBerr[1,])
    
    #last catch (for IAV constraint)
    lastTAC <- rep(an(FLCore::catch(fit$stk)[,ac(range(fit$stk)["maxyear"]-1),,,,]),Nmod)
    
    #selections now iteration specific, put the selections for the current year in a temporary object simyear.sels
    #with dimensions ages x iters
    simyear.sels <- simyear.SW <- simyear.CW <- simyear.LW <- array(NA,c(ages,Nmod))
    for(ii in 1:Nmod){
      #simyear.sels[,ii] <- sel[,rsamsel[1,ii]]        #assessment based selection
      simyear.sels[,ii] <- sels[,rsamsel[1,ii],ii]    #iteration based selection
      sely[,1,ii] <- simyear.sels[,ii]
      simyear.SW[,ii] <- west[,rsam[1,ii],ii]
      WSy[,1,ii] <- simyear.SW[,ii]
      simyear.CW[,ii] <- weca[,rsam[1,ii],ii]
      Wy[,1,ii] <- simyear.CW[,ii]
      simyear.LW[,ii] <- wela[,rsam[1,ii],ii]
      Wl[,1,ii] <- simyear.LW[,ii]
    }
    
    #apply the HCR - for the y-1 ssb use the y values (any action will be overridden by the catch constraint in year 1 anyway)
    #Fmgmt[1,] <- do.call(fManagement, args=list(list("Fnext" = Fbar,"Btrigger" = Btrigger, "SSB" = ssby.obs, "Yr" = 1, 
    #                                                 "M" = M[,rsam[1,]], "sel" = simyear.sels, "N" = Ny[,1,], 
    #                                                 "SW" = west[,rsam[1,]], "Mat" = Mat[,rsam[1,]], "Blim" = Blim)))

    Fmgmt[1,] <- do.call(fManagement, args=list(list("Fnext" = Fbar,"Btrigger" = Btrigger, "SSB" = ssby.obs, "Yr" = 1, 
                                                     "M" = M[,rsam[1,]], "sel" = simyear.sels, "N" = Ny[,1,], 
                                                     "SW" = simyear.SW, "Mat" = Mat[,rsam[1,]], "Blim" = Blim)))
    
    #apply error on the management F to get the realised F
    Fnext <- Fmgmt_err[1,] <- exp(Ferr[1,]) * Fmgmt[1,]
    Fy[,1,] <- rep(Fnext, each = ages) * simyear.sels
    Cy[,1,] <- Ny[,1,] * Fy[,1,] / (Fy[,1,] + M[,rsam[1,]]) * (1 - exp(-Fy[,1,] - M[,rsam[1,]]))
    TAC[1,] <- Yld1 <- apply(Cy[,1,]*Wy[,1,], MARGIN=2, FUN="sum")
    Yld2 <- Yld3 <- Yld4 <- rep(NA,Nmod)
    
    #check for minTAC/maxTAC constraints
    if (nrow(dplyr::filter(dfExplConstraints,YearNum==1 & toupper(Type) %in% c("MINTAC","MAXTAC")))>0) {
      
      minTAC <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==1 & toupper(dfExplConstraints$Type)=="MINTAC",]$Val)
      maxTAC <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==1 & toupper(dfExplConstraints$Type)=="MAXTAC",]$Val)
      
      tgt <- rep(NA,Nmod)
      
      if (!is.na(minTAC) & sum(Yld1 < minTAC) > 0){
        tgt[Yld1<minTAC] <- minTAC
        icesTAF::msg(paste0(sum(Yld1<minTAC)," Minimum TAC set in year 1\n"))
      }
      if (!is.na(maxTAC) & sum(Yld1 > maxTAC) > 0) {
        tgt[Yld1>maxTAC] <- maxTAC
        icesTAF::msg(paste0(sum(Yld1>maxTAC)," Maximum TAC set in year 1\n"))
      }
      
      if (sum(is.na(tgt)) < Nmod) {

        Fmgmt_err[1,!is.na(tgt)] <- fFindF(N = Ny[,1,!is.na(tgt),drop=FALSE], CW = Wy[,1,!is.na(tgt),drop=FALSE], 
                                           SW = WSy[,1,!is.na(tgt),drop=FALSE], Mat = Mat[,rsam[1,!is.na(tgt)],drop=FALSE],
                                           M = M[,rsam[1,!is.na(tgt)],drop=FALSE], Sel = simyear.sels[,!is.na(tgt),drop=FALSE], 
                                           tgt = tgt[!is.na(tgt)], type = 1, ages = ages, iters = sum(!is.na(tgt)))
        
        Fnext <- Fmgmt_err[1,]
        Fy[,1,] <- rep(Fnext, each = ages) * simyear.sels
        Cy[,1,] <- Ny[,1,] * Fy[,1,] / (Fy[,1,] + M[,rsam[1,]]) * (1 - exp(-Fy[,1,] - M[,rsam[1,]]))
        Yld2 <- apply(Cy[,1,]*Wy[,1,], MARGIN=2, FUN="sum") 
        TAC[1,!is.na(tgt)] <- tgt[!is.na(tgt)]
      } else {
        Yld2 <- Yld1
      }
    } else {
      Yld2 <- Yld1
    } #End of check for minTAC/maxTAC constraints
    
    #check for IAV
    #if (nrow(dplyr::filter(dfExplConstraints,YearNum==1 & toupper(Type) %in% c("IAV")))>0) {
    if (nrow(dplyr::filter(dfExplConstraints,YearNum==1 & toupper(Type) %in% c("IAVINC","IAVDEC")))>0) {
      
      #IAV <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==1 & toupper(dfExplConstraints$Type)=="IAV",]$Val)
      IAVInc <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==1 & toupper(dfExplConstraints$Type)=="IAVINC",]$Val)
      IAVDec <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==1 & toupper(dfExplConstraints$Type)=="IAVDEC",]$Val)
      
      tgt <- rep(NA,Nmod)
      incs1 <- decs1 <- incs <- decs <- 0
      
      #increases
      if (!is.na(IAVInc)){
        # if (sum((Yld1-lastTAC)/lastTAC > IAVInc, na.rm = TRUE)>0){
        #   #adjust targets to limit increases
        #   incs <- sum((Yld1/lastTAC) > (1+IAVInc), na.rm=TRUE)
        #   tgt[(Yld1/lastTAC) > (1+IAVInc)] <- (1+IAVInc)*lastTAC[(Yld1/lastTAC) > (1+IAVInc)]
        # }
        if (sum((Yld2-lastTAC)/lastTAC > IAVInc, na.rm = TRUE)>0){
          #adjust targets to limit increases
          incs <- sum((Yld2/lastTAC) > (1+IAVInc), na.rm=TRUE)
          tgt[(Yld2/lastTAC) > (1+IAVInc)] <- (1+IAVInc)*lastTAC[(Yld2/lastTAC) > (1+IAVInc)]
        }
        
      }
      #decreases
      if (!is.na(IAVDec)){
        # if (sum((Yld1-lastTAC)/lastTAC < -1*IAVDec, na.rm = TRUE)>0){
        #   #adjust targets to limit decreases
        #   decs <- sum((Yld1/lastTAC) < (1-IAVDec), na.rm=TRUE)
        #   tgt[(Yld1/lastTAC) < (1-IAVDec)] <- (1-IAVDec)*lastTAC[(Yld1/lastTAC) < (1-IAVDec)]
        # }
        
        if (sum((Yld2-lastTAC)/lastTAC < -1*IAVDec, na.rm = TRUE)>0){
          #adjust targets to limit decreases
          decs <- sum((Yld2/lastTAC) < (1-IAVDec), na.rm=TRUE)
          tgt[(Yld2/lastTAC) < (1-IAVDec)] <- (1-IAVDec)*lastTAC[(Yld2/lastTAC) < (1-IAVDec)]
        }
        
      } 
      
      #message
      icesTAF::msg(paste0(incs1,"/",decs1," IAV capped increases/decreases all SSB in year 1"))
      icesTAF::msg(paste0(incs,"/",decs," IAV capped increases/decreases set in year 1"))
      
      # if (!is.na(IAV)){
      #   if (sum(abs((Yld1-lastTAC)/lastTAC) > IAV, na.rm=TRUE)>0) {
      #     #increase
      #     if (length(tgt[(Yld1/lastTAC) > (1+IAV)]) > 0){tgt[(Yld1/lastTAC) > (1+IAV)] <- (1+IAV)*lastTAC[(Yld1/lastTAC) > (1+IAV)]}
      #     #decrease
      #     if (length(tgt[(Yld1/lastTAC) < (1-IAV)]) > 0){tgt[(Yld1/lastTAC) < (1-IAV)] <- (1-IAV)*lastTAC[(Yld1/lastTAC) < (1-IAV)]}
      #     
      #     #message
      #     icesTAF::msg(paste0(sum((Yld1/lastTAC) > (1+IAV)),"/",
      #                         sum((Yld1/lastTAC) < (1-IAV)),
      #                         " IAV capped increases/decreases set in year 1"))
      #   }
      # }
      
      if (sum(is.na(tgt)) < Nmod) {
        
        Fmgmt_err[1,!is.na(tgt)] <- fFindF(N = Ny[,1,!is.na(tgt),drop=FALSE], CW = Wy[,1,!is.na(tgt),drop=FALSE], 
                                           SW = WSy[,1,!is.na(tgt),drop=FALSE], Mat = Mat[,rsam[1,!is.na(tgt)],drop=FALSE],
                                           M = M[,rsam[1,!is.na(tgt)],drop=FALSE], Sel = simyear.sels[,!is.na(tgt),drop=FALSE],
                                           tgt = tgt[!is.na(tgt)], type = 1, ages = ages, iters = sum(!is.na(tgt)))
        
        Fnext <- Fmgmt_err[1,]
        Fy[,1,] <- rep(Fnext, each = ages) * simyear.sels
        Cy[,1,] <- Ny[,1,] * Fy[,1,] / (Fy[,1,] + M[,rsam[1,]]) * (1 - exp(-Fy[,1,] - M[,rsam[1,]]))
        Yld2 <- Yld2
        Yld3[!is.na(tgt)] <- apply(Cy[,1,!is.na(tgt)]*Wy[,1,!is.na(tgt)], MARGIN=2, FUN="sum") 
        TAC[1,!is.na(tgt)] <- tgt[!is.na(tgt)]

      } else {
        Yld3 <- Yld2
      }
      
    } else {
      Yld3 <- Yld2
    }  # End of check for IAV
    
    
    #check for specified catch/F
    if (nrow(dplyr::filter(dfExplConstraints,YearNum==1 & toupper(Type) %in% c("CATCH","F")))>0) {
      
      Catch2 <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==1 & toupper(dfExplConstraints$Type)=="CATCH",]$Val)
      F2 <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==1 & toupper(dfExplConstraints$Type)=="F",]$Val)
      
      #check that only a catch or F constraint is provided - not permitted both
      
      if (!is.na(Catch2)){
        
        tgt <- rep(Catch2,Nmod)
        icesTAF::msg(paste0("Catch constraint applied in year 1"))
        
        #catch constraint
        Fmgmt_err[1,] <- fFindF(N = Ny[,1,,drop=FALSE], CW = Wy[,1,,drop=FALSE], 
                                SW = WSy[,1,,drop=FALSE], Mat = Mat[,rsam[1,],drop=FALSE], 
                                M = M[,rsam[1,],drop=FALSE], Sel = simyear.sels, 
                                tgt = tgt, type = 1, ages = ages, iters = sum(!is.na(tgt)))
        
        Fnext <- Fmgmt_err[1,]
        Fy[,1,] <- rep(Fnext, each = ages) * simyear.sels
        Cy[,1,] <- Ny[,1,] * Fy[,1,] / (Fy[,1,] + M[,rsam[1,]]) * (1 - exp(-Fy[,1,] - M[,rsam[1,]]))
        Yld4 <- apply(Cy[,1,]*Wy[,1,], MARGIN=2, FUN="sum")
        TAC[1,!is.na(tgt)] <- tgt[!is.na(tgt)]
        
      } else {
        Yld4 <- Yld3
      } 
      
      #TO DO........  
      #if (!is.na(F2)){
      #  tgt <- rep(F2,Nmod)
      #  icesTAF::msg(paste0("F constraint applied in year 1"))
      #} else {
      #  Yld3 <- Yld2
      #}
      
      
    } else {
      Yld4 <- Yld3
    } # End of check for specified catch/F
    
    #population numbers at year end
    Ny2[,1,] <- Ny[,1, ] * exp(-Fy[,1,] - M[,rsam[1,]])
    
    #AC - don't understand this next bit - not required for WHM so comment out for now
    
    # if rec recruiting year class comes from previous years ssb, as in fish recruiting
    # at age 2 or winter ring herring ageing then run some more initial years
    # using the same intial population
    # - NOTE roll forward one year incase ssb_lag is 0 so that we always have a year j-1.
    #for (j in 2:pmax(2, ssb_lag)) {
    #  Ny[,j,] <- rbind(N1[1:(ages-1),], colSums(N1[ages:50,]))
    #  ssby[j,] <- colSums(Mat[,rsam[j-1,]] * Ny[,1,] * west[,rsam[j-1,]] / exp(Zpre))
    #}
    
    # Years (2 + ssb_lag) to Nrun:
    #AC - will need to reinstate above code if ssb_lag>0 but OK for now
    j <- 2
    for (j in (2+ssb_lag):Nrun) {
      
      #cat("Year=",j,"\n")
      
      simyear.sels <- simyear.SW <- simyear.CW <- simyear.LW <- array(NA,c(ages,Nmod))
      for(ii in 1:Nmod){
        #simyear.sels[,ii] <- sel[,rsamsel[j,ii]]        #assessment based selection
        simyear.sels[,ii] <- sels[,rsamsel[j,ii],ii]    #iteration based selection
        sely[,j,ii] <- simyear.sels[,ii]
        simyear.SW[,ii] <- west[,rsam[j,ii],ii]
        WSy[,j,ii] <- simyear.SW[,ii]
        simyear.CW[,ii] <- weca[,rsam[j,ii],ii]
        Wy[,j,ii] <- simyear.CW[,ii]
        simyear.LW[,ii] <- wela[,rsam[j,ii],ii]
        Wl[,j,ii] <- simyear.LW[,ii]
      }
      
      #roll population forward
      #recruits
      Ny[1,j,] <- 0
      #mid ages
      Ny[2:(ages-1),j,] <- Ny2[1:(ages-2),j-1,]
      #PG
      Ny[ages,j,] <- Ny2[ages-1,j-1,] + Ny2[ages,j-1,]
      
      #SSB J1
      #ssby[j,] <- apply(array(Mat[,rsam[j,]] * Ny[,j,] * west[,rsam[j,]], c(ages, Nmod)), 2, sum)
      ssby[j,] <- apply(array(Mat[,rsam[j,]] * Ny[,j,] * simyear.SW, c(ages, Nmod)), 2, sum)
      #add observation error.
      #ss0by.obs[j,] <- ssby[j,]*exp(SSBerr[j-1,])
      ssby.obs[j,] <- ssby[j,]*exp(SSBerr[j,])
      
      SSBforRec <- ssby[j-ssb_lag,]
      
      if (process.error) {
        allrecs <- sapply(unique(SR$mod), function(mod) exp(match.fun(mod)(SR, SSBforRec) + resids[,j]))
      } else {
        allrecs <- sapply(unique(SR$mod), function(mod) exp(match.fun(mod) (SR, SSBforRec)))
      }
      
      select <- cbind(seq(Nmod), as.numeric(factor(SR$mod, levels = unique(SR$mod))))
      Ny[1,j,] <- allrecs[select]
      
      #after year 10, kill recruitment to 1/3 of normal
      #if (j<=10) {Ny[1,j,]<- Ny[1,j,]/3}
      
      #default F is the current Fscan value
      Fnext <- Fbar
      
      #apply the HCR
      #selections now iteration specific, put the selections for the current year in a temporary object simyear.sels
      #with dimensions ages x iters
      #simyear.sels <- array(NA,c(ages,Nmod))
      #for(ii in 1:Nmod){
      #  #simyear.sels[,ii] <- sel[,rsamsel[j,ii]]        #assessment based selection
      #  simyear.sels[,ii] <- sels[,rsamsel[j,ii],ii]    #iteration based selection
      #  sely[,j,ii] <- simyear.sels[,ii]
      #}


      
#      Fmgmt[j,] <- do.call(fManagement, args=list(list("Fnext" = Fbar, "Btrigger" = Btrigger, "SSB" = ssby.obs, "Yr" = j, 
#                                                       "M" = M[,rsam[j,]], "sel" = simyear.sels, "N" = Ny[,j,], 
#                                                       "SW" = west[,rsam[j,]], "Mat" = Mat[,rsam[j,]], "Blim" = Blim)))

      Fmgmt[j,] <- do.call(fManagement, args=list(list("Fnext" = Fbar, "Btrigger" = Btrigger, "SSB" = ssby.obs, "Yr" = j, 
                                                       "M" = M[,rsam[j,]], "sel" = simyear.sels, "N" = Ny[,j,], 
                                                       "SW" = simyear.SW, "Mat" = Mat[,rsam[j,]], "Blim" = Blim)))
      
      #apply F error to get realised F from management F
      Fnext <- Fmgmt_err[j,] <- Fmgmt[j,]*exp(Ferr[j,])
      Fy[,j,] <- rep(Fnext, each = ages) * simyear.sels
      Cy[,j,] <- Ny[,j,] * Fy[,j,] / (Fy[,j,] + M[,rsam[j,]]) * (1 - exp(-Fy[,j,] - M[,rsam[j,]]))
      Yld1 <- apply(Cy[,j,]*Wy[,j,], MARGIN=2, FUN="sum")
      TAC[j,] <- Yld1
      Yld2 <- Yld3 <- Yld4 <- rep(NA,Nmod)
      
      #check for minTAC/maxTAC constraints
      if (nrow(dplyr::filter(dfExplConstraints,YearNum==j & toupper(Type) %in% c("MINTAC","MAXTAC")))>0) {
        
        minTAC <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==j & toupper(dfExplConstraints$Type)=="MINTAC",]$Val)
        maxTAC <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==j & toupper(dfExplConstraints$Type)=="MAXTAC",]$Val)
        
        tgt <- rep(NA,Nmod)
        
        if (!is.na(minTAC) & sum(Yld1 < minTAC) > 0){
          tgt[Yld1<minTAC] <- minTAC
          icesTAF::msg(paste0(sum(Yld1<minTAC)," Minimum TAC set in year ",j))
        }
        if (!is.na(maxTAC) & sum(Yld1 > maxTAC) > 0) {
          tgt[Yld1>maxTAC] <- maxTAC
          icesTAF::msg(paste0(sum(Yld1>maxTAC)," Maximum TAC set in year ",j))
        }
        
        if (sum(is.na(tgt)) < Nmod) {
  
          #calculate new F for those to be updated
          Fmgmt_err[j,!is.na(tgt)] <- fFindF(N = Ny[,j,!is.na(tgt),drop=FALSE], CW = Wy[,j,!is.na(tgt),drop=FALSE], 
                                             SW = WSy[,j,!is.na(tgt),drop=FALSE], Mat = Mat[,rsam[j,!is.na(tgt)],drop=FALSE],
                                             M = M[,rsam[j,!is.na(tgt)],drop=FALSE], Sel = simyear.sels[,!is.na(tgt),drop=FALSE],
                                             tgt = tgt[!is.na(tgt)], type = 1, ages = ages, iters = sum(!is.na(tgt)))
          
          
          Fnext <- Fmgmt_err[j,]
          Fy[,j,] <- rep(Fnext, each = ages) * simyear.sels
          Cy[,j,] <- Ny[,j,] * Fy[,j,] / (Fy[,j,] + M[,rsam[j,]]) * (1 - exp(-Fy[,j,] - M[,rsam[j,]]))
          Yld2 <- apply(Cy[,j,]*Wy[,j,], MARGIN=2, FUN="sum")
          TAC[j,!is.na(tgt)] <- tgt[!is.na(tgt)]
          
        } else {
          Yld2 <- Yld1
        }
      } else {
        Yld2 <- Yld1
      }
      
      #IAV
      if (nrow(dplyr::filter(dfExplConstraints,YearNum==j & toupper(Type) %in% c("IAVINC","IAVDEC")))>0) {
        
        IAVInc <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==j & toupper(dfExplConstraints$Type)=="IAVINC",]$Val)
        IAVDec <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==j & toupper(dfExplConstraints$Type)=="IAVDEC",]$Val)
        
        #new target when restrictions are included
        tgt <- rep(NA,Nmod)
        incs1 <- decs1 <- incs <- decs <- 0
        
        #increases
        if (!is.na(IAVInc)){
          
          # if (sum((Yld1-TAC[j-1,])/TAC[j-1,] > IAVInc, na.rm = TRUE)>0){
          #   #adjust targets to limit increases
          #   incs <- sum((Yld1/TAC[j-1,]) > (1+IAVInc), na.rm=TRUE)
          #   tgt[(Yld1/TAC[j-1,]) > (1+IAVInc)] <- (1+IAVInc)*TAC[j-1,(Yld1/TAC[j-1,]) > (1+IAVInc)]
          # }
          
          
          #if (sum((Yld2-TAC[j-1,])/TAC[j-1,] > IAVInc, na.rm = TRUE)>0){
          #  #adjust targets to limit increases
          #  incs <- sum((Yld2/TAC[j-1,]) > (1+IAVInc), na.rm=TRUE)
          #  tgt[(Yld2/TAC[j-1,]) > (1+IAVInc)] <- (1+IAVInc)*TAC[j-1,(Yld2/TAC[j-1,]) > (1+IAVInc)]
          #}

          #limit to above Btrigger only
          if (sum((Yld2[ssby.obs[j,]>Btrigger]-TAC[j-1,ssby.obs[j,]>Btrigger])/TAC[j-1,ssby.obs[j,]>Btrigger] > IAVInc, na.rm = TRUE)>0){
            #adjust targets to limit increases
            incs1 <- sum((Yld2/TAC[j-1,]) > (1+IAVInc), na.rm=TRUE)
            incs <- sum((Yld2[ssby.obs[j,]>Btrigger]/TAC[j-1,ssby.obs[j,]>Btrigger]) > (1+IAVInc), na.rm=TRUE)
            ratio <- Yld2/TAC[j-1,]
            ratio[TAC[j-1,]==0] <- 0
            #tgt[((Yld2/TAC[j-1,]) > (1+IAVInc)) & (ssby.obs[j,]>Btrigger)] <- (1+IAVInc)*TAC[j-1,((Yld2/TAC[j-1,]) > (1+IAVInc)) & (ssby.obs[j,]>Btrigger)]
            tgt[(ratio > (1+IAVInc)) & (ssby.obs[j,]>Btrigger)] <- (1+IAVInc)*TAC[j-1,(ratio > (1+IAVInc)) & (ssby.obs[j,]>Btrigger)]
          }
          
                    
        }
        #decreases
        if (!is.na(IAVDec)){
          # if (sum((Yld1-TAC[j-1,])/TAC[j-1,] < -1*IAVDec, na.rm = TRUE)>0){
          #   #adjust targets to limit decreases
          #   decs <- sum((Yld1/TAC[j-1,]) < (1-IAVDec), na.rm=TRUE)
          #   tgt[(Yld1/TAC[j-1,]) < (1-IAVDec)] <- (1-IAVDec)*TAC[j-1,(Yld1/TAC[j-1,]) < (1-IAVDec)]
          # }
          
          # if (sum((Yld2-TAC[j-1,])/TAC[j-1,] < -1*IAVDec, na.rm = TRUE)>0){
          #   #adjust targets to limit decreases
          #   decs <- sum((Yld2/TAC[j-1,]) < (1-IAVDec), na.rm=TRUE)
          #   tgt[(Yld2/TAC[j-1,]) < (1-IAVDec)] <- (1-IAVDec)*TAC[j-1,(Yld2/TAC[j-1,]) < (1-IAVDec)]
          # }
          
          #limit to above Btrigger only
          if (sum((Yld2[ssby.obs[j,]>Btrigger]-TAC[j-1,ssby.obs[j,]>Btrigger])/TAC[j-1,ssby.obs[j,]>Btrigger] < -1*IAVDec, na.rm = TRUE)>0){
            #adjust targets to limit decreases
            decs1 <- sum((Yld2/TAC[j-1,]) < (1-IAVDec), na.rm=TRUE)
            decs <- sum((Yld2[ssby.obs[j,]>Btrigger]/TAC[j-1,ssby.obs[j,]>Btrigger]) < (1-IAVDec), na.rm=TRUE)
            ratio <- Yld2/TAC[j-1,]
            ratio[TAC[j-1,]==0] <- 0
            tgt[(ratio < (1-IAVDec)) & (ssby.obs[j,]>Btrigger)] <- (1-IAVDec)*TAC[j-1,(ratio < (1-IAVDec)) & (ssby.obs[j,]>Btrigger)]
            #tgt[((Yld2/TAC[j-1,]) < (1-IAVDec)) & (ssby.obs[j,]>Btrigger)] <- (1-IAVDec)*TAC[j-1,((Yld2/TAC[j-1,]) < (1-IAVDec)) & (ssby.obs[j,]>Btrigger)]
          }
          
        }
        
        #message
        icesTAF::msg(paste0(incs1,"/",decs1," IAV capped increases/decreases all SSB ",j))
        icesTAF::msg(paste0(incs,"/",decs," IAV capped increases/decreases set in year ",j))
        
        if (sum(is.na(tgt)) < Nmod & sum(is.na(tgt))>0) {
          
          #find the new F for those targets to be adjusted
          Fmgmt_err[j,!is.na(tgt)] <- fFindF(N = Ny[,j,!is.na(tgt),drop=FALSE], CW = Wy[,j,!is.na(tgt),drop=FALSE], 
                                             SW = WSy[,j,!is.na(tgt),drop=FALSE], Mat = Mat[,rsam[j,!is.na(tgt)],drop=FALSE],
                                             M = M[,rsam[j,!is.na(tgt)],drop=FALSE], Sel = simyear.sels[,!is.na(tgt),drop=FALSE], 
                                             tgt = tgt[!is.na(tgt)], type = 1, ages = ages, iters = sum(!is.na(tgt)))
          
          Fnext <- Fmgmt_err[j,]
          Fy[,j,] <- rep(Fnext, each = ages) * simyear.sels
          Cy[,j,] <- Ny[,j,] * Fy[,j,] / (Fy[,j,] + M[,rsam[j,]]) * (1 - exp(-Fy[,j,] - M[,rsam[j,]]))
          Yld3 <- Yld2
          
          if (sum(!is.na(tgt))==1){
            Yld3[!is.na(tgt)] <- sum(Cy[,j,!is.na(tgt)]*Wy[,j,!is.na(tgt)])} 
          else {
            Yld3[!is.na(tgt)] <- apply(Cy[,j,!is.na(tgt)]*Wy[,j,!is.na(tgt)], MARGIN=2, FUN="sum")
          }
            
          TAC[j,!is.na(tgt)] <- tgt[!is.na(tgt)]
          
        } else {
          Yld3 <- Yld2
        }
      } else {
        Yld3 <- Yld2
      }
      
      #check for specified catch/F
      if (nrow(dplyr::filter(dfExplConstraints,YearNum==j & toupper(Type) %in% c("CATCH","F")))>0) {
        
        Catch2 <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==j & toupper(dfExplConstraints$Type)=="CATCH",]$Val)
        F2 <- fNAifEmpty(dfExplConstraints[dfExplConstraints$YearNum==j & toupper(dfExplConstraints$Type)=="F",]$Val)
        
        #check that only a catch or F constraint is provided - not permitted both
        
        if (!is.na(Catch2)){
          
          tgt <- rep(Catch2,Nmod)
          
          icesTAF::msg(paste0("Catch constraint applied in year ",j))
          
          Fmgmt_err[j,] <- fFindF(N = Ny[,j,,drop=FALSE], CW = Wy[,j,,drop=FALSE], 
                                  SW = WSy[,j,,drop=FALSE], Mat = Mat[,rsam[j,],drop=FALSE], 
                                  M = M[,rsam[j,],drop=FALSE], Sel = simyear.sels, 
                                  tgt = tgt, type = 1, ages = ages, iters = sum(!is.na(tgt)))
          
          Fnext <- Fmgmt_err[j,]
          Fy[,j,] <- rep(Fnext, each = ages) * simyear.sels
          Cy[,j,] <- Ny[,j,] * Fy[,j,] / (Fy[,j,] + M[,rsam[j,]]) * (1 - exp(-Fy[,j,] - M[,rsam[j,]]))
          Yld4 <- apply(Cy[,j,]*Wy[,j,], MARGIN=2, FUN="sum")
          TAC[j,] <- tgt
          
        } else {
          Yld4 <- Yld3
        }
        
        #TO DO........  
        #if (!is.na(F2)){
        #  tgt <- rep(F2,Nmod)
        #  icesTAF::msg(paste0("F constraint applied in year ",j))
        #} else {
        #  Yld3 <- Yld2
        #}
        
      } else {
        Yld4 <- Yld3
      }
      
      #population numbers at year end
      Ny2[,j,] <- Ny[,j, ] * exp(-Fy[,j,] - M[,rsam[j,]])
      
    }
    
    # convert to catch weight
    Cw <- Cy * Wy   # catch Numbers *catch wts
    land <- Cy * Ry * Wl # catch Numbers * Fraction (in number) landed and landed wts
    Lan <- apply(land, 2:3, sum)
    Cat <- apply(Cw, 2:3, sum)
    
    # summarise everything and spit out!
    quants <- c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975)
    ssbs[, i] <- stats::quantile(ssby[begin:Nrun, ], quants)
    #cats[, i] <- stats::quantile(Cat[begin:Nrun, ], quants)
    #lans[, i] <- stats::quantile(Lan[begin:Nrun, ], quants)
    recs[, i] <- stats::quantile(Ny[1, begin:Nrun, ], quants)
    ferr[i, , ] <- Ferr[begin:Nrun, ]
    ssbsa[i, , ] <- ssby[begin:Nrun, ]
    catsa[i, , ] <- Cat[begin:Nrun, ]
    lansa[i, , ] <- Lan[begin:Nrun, ]
    recsa[i, , ] <- Ny[1, begin:Nrun, ]
    
    #AC info to save
    simStks[[ac(Fscan[i])]] <- list()
    #abundance @ Jan 1 (also ST)
    simStks[[ac(Fscan[i])]][["N"]] <- Ny
    #realised F at age
    simStks[[ac(Fscan[i])]][["F"]] <- Fy
    #management F i.e. that set by the HCR or superceded by any exploration constraints
    simStks[[ac(Fscan[i])]][["Fmgmt"]] <- Fmgmt
    simStks[[ac(Fscan)[i]]][["Fdev"]] <- Fmgmt_err - Fmgmt
    simStks[[ac(Fscan[i])]][["Fratio"]] <- Fmgmt_err/Fmgmt
    #TAC
    simStks[[ac(Fscan[i])]][["TAC"]] <- TAC
    #catch weights
    simStks[[ac(Fscan[i])]][["catW"]] <- Wy
    #landing weights
    #simStks[[ac(Fscan[i])]][["lanW"]] <- Wl
    #maturity
    simStks[[ac(Fscan[i])]][["Mat"]] <- Maty      
    #stock weights
    simStks[[ac(Fscan[i])]][["stkW"]] <- WSy      
    #random stock weight 
    simStks[[ac(Fscan[i])]][["stkWran"]] <- rsam
    #catch numbers
    simStks[[ac(Fscan[i])]][["C"]] <- Cy
    #simStks[[ac(Fscan[i])]][["L"]] <- Cy * Ry
    simStks[[ac(Fscan[i])]][["SSBdev"]] <- ssby.obs - ssby
    simStks[[ac(Fscan[i])]][["SSBratio"]] <- ssby.obs/ssby
    simStks[[ac(Fscan[i])]][["SimYears"]] <- seq(range(fit$stk)["maxyear"],range(fit$stk)["maxyear"]+Nrun)
    simStks[[ac(Fscan[i])]][["Sel"]] <- sely
    #AC end
    
    if (verbose) loader(i/NF)
    cat("\n")
  }
  
  if (verbose) icesTAF::msg("Summarising simulations")
  
  dimnames(ssbs) <- dimnames(cats) <-
    dimnames(lans) <- dimnames(recs) <-
    list(quants=c("p025","p05","p25","p50","p75","p95","p975"),
         fmort=Fscan)
  
  rbp2dataframe <- function(x,variable) {
    x <- data.frame(t(x))
    x$variable <- variable
    x$Ftarget <- as.numeric(row.names(x))
    rownames(x) <- NULL
    return(x)
  }
  rbp <- rbind(rbp2dataframe(recs,"Recruitment"),
               rbp2dataframe(ssbs,"Spawning stock biomass"),
               rbp2dataframe(cats,"Catch"),
               rbp2dataframe(lans,"Landings"))
  rbp <- rbp[,c(9,8,1:7)]
  
  #defaults
  Refs <- refs_interval <- pProfile <- Blim <- Bpa <- NA
  
  if (calc.RPs) { #AC
    
    # STOCK REFERENCE POINTS
    
    FCrash05 <- Fscan[which.max(cats[2,]):NF][ which(cats[2, which.max(cats[2,]):NF] < 0.05*max(cats[2,]) )[1] ]
    FCrash50 <- Fscan[which.max(cats[4,]):NF][ which(cats[4, which.max(cats[4,]):NF] < 0.05*max(cats[4,]) )[1] ]
    
    # Einar amended 30.1.2014
    if(missing(extreme.trim)) {
      catm <- apply(catsa, 1, mean)
      lanm <- apply(lansa, 1, mean)
    } else {
      
      # 2014-03-12 Outcommented per note from Carmen/John - see below
      #x <- catsa
      #i <- x > quantile(x,extreme.trim[2]) |
      #  x < quantile(x,extreme.trim[1])
      #x[i] <- NA
      #catm <- apply(x, 1, mean, na.rm=TRUE)
      #
      #x <- lansa
      #i <- x > quantile(x,extreme.trim[2]) |
      #  x < quantile(x,extreme.trim[1])
      #x[i] <- NA
      #lanm <- apply(x, 1, mean, na.rm=TRUE)
      
      # 2014-03-12: Above replaced with the following per note from Carmen/John
      #  If we want to remove whole SR models, we could use the following code. But it is too extreme, it ends up getting rid of most models:
      # auxi2 <- array( apply(catsa, 1, function(x){auxi<-rep(TRUE,Nmod); auxi[x > quantile(x, extreme.trim[2]) | x < quantile(x, extreme.trim[1])] <- FALSE; x <- auxi } ), dim=c(keep,Nmod,NF))
      # auxi2 <- (1:Nmod)[apply(auxi2, 2, function(x){length(unique(as.vector(x)))})==1]
      # apply(catsa[,,auxi2],1,mean)
      
      # So I think the alternative is not to get rid of whole SR models, but of different SR models depending on the value of F:
      catm <- apply(catsa, 1, function(x){mean(x[x <= stats::quantile(x, extreme.trim[2]) & x >= stats::quantile(x, extreme.trim[1])])})
      lanm <- apply(lansa, 1, function(x){mean(x[x <= stats::quantile(x, extreme.trim[2]) & x >= stats::quantile(x, extreme.trim[1])])})
    }
    
    # end Einar amended 30.1.2014
    
    maxcatm <- which.max(catm)
    maxlanm <- which.max(lanm)
    
    # Einar added 29.1.2014
    rbp$Mean <- NA
    rbp$Mean[rbp$variable == "Catch"] <- catm
    rbp$Mean[rbp$variable == "Landings"] <- lanm
    # end Einar added 29.1.2014
    
    catsam <- apply(catsa, c(1,3), mean)
    lansam <- apply(lansa, c(1,3), mean)
    maxpf <- apply(catsam, 2, which.max)
    maxpfl <- apply(lansam, 2, which.max)
    
    FmsyLan <- Fscan[maxpfl]
    msymLan <- mean(FmsyLan)
    vcumLan <- stats::median(FmsyLan)
    fmsy.densLan <- stats::density(FmsyLan)
    vmodeLan <- fmsy.densLan$x[which.max(fmsy.densLan$y)]
    
    FmsyCat <- Fscan[maxpf]
    msymCat <- mean(FmsyCat)
    vcumCat <- stats::median(FmsyCat)
    fmsy.densCat <- stats::density(FmsyCat)
    vmodeCat <- fmsy.densCat$x[which.max(fmsy.densCat$y)]
    
    pFmsyCat  <- data.frame(Ftarget=fmsy.densCat$x,
                            value=cumsum(fmsy.densCat$y * diff(fmsy.densCat$x)[1]),
                            variable="pFmsyCatch")
    pFmsyLan  <- data.frame(Ftarget=fmsy.densLan$x,
                            value=cumsum(fmsy.densLan$y * diff(fmsy.densLan$x)[1]),
                            variable="pFmsyLandings")
    pProfile <- rbind(pFmsyCat,pFmsyLan)
    
    # PA REFERENCE POINTS
    if(!missing(Blim)) {
      pBlim <- apply(ssbsa > Blim, 1, mean)
      
      i <- max(which(pBlim > .95))
      grad <- diff(Fscan[i + 0:1]) / diff(pBlim[i + 0:1])
      flim <- Fscan[i] + grad * (0.95 - pBlim[i]) # linear interpolation i think..
      
      i <- max(which(pBlim > .90))
      grad <- diff(Fscan[i + 0:1]) / diff(pBlim[i + 0:1])
      flim10 <- Fscan[i]+grad*(0.9-pBlim[i]) # linear interpolation i think..
      
      i <- max(which(pBlim > .50))
      grad <- diff(Fscan[i + 0:1]) / diff(pBlim[i + 0:1])
      flim50 <- Fscan[i]+grad*(0.5-pBlim[i]) # linear interpolation i think..
      
      pBlim <- data.frame(Ftarget = Fscan,value = 1-pBlim,variable="Blim")
      pProfile <- rbind(pProfile,pBlim)
    } else {
      flim <- flim10 <- flim50 <- Blim <- NA
    }
    
    if(!missing(Bpa)) {
      pBpa <- apply(ssbsa > Bpa, 1, mean)
      pBpa <- data.frame(Ftarget = Fscan,value = 1-pBpa,variable="Bpa")
      pProfile <- rbind(pProfile,pBpa)
    } else {
      Bpa <- NA
    }
    
    # GENERATE REF-TABLE
    catF <- c(flim, flim10, flim50, vcumCat, Fscan[maxcatm], FCrash05, FCrash50)
    lanF <- c(   NA,    NA,     NA, vcumLan, Fscan[maxlanm],       NA,       NA)
    catC <- stats::approx(Fscan, cats[4,], xout = catF)$y
    lanC <- stats::approx(Fscan, lans[4,], xout = lanF)$y
    catB <- stats::approx(Fscan, ssbs[4,], xout = catF)$y
    lanB <- stats::approx(Fscan, ssbs[4,], xout = lanF)$y
    
    Refs <- rbind(catF, lanF, catC, lanC, catB, lanB)
    rownames(Refs) <- c("catF","lanF","catch","landings","catB","lanB")
    colnames(Refs) <- c("F05","F10","F50","medianMSY","meanMSY","FCrash05","FCrash50")
    
    #TODO: id.sim - user specified.
    
    # 2014-03-12 Ammendments per note from Carmen/John
    # CALCULATIONS:
    
    # Fmsy: value that maximises median LT catch or median LT landings
    auxi <- stats::approx(Fscan, cats[4, ],xout=seq(min(Fscan),max(Fscan),length=200))
    FmsyMedianC <- auxi$x[which.max(auxi$y)]
    MSYMedianC <- max(auxi$y)
    # Value of F that corresponds to 0.95*MSY:
    FmsylowerMedianC <- auxi$x[ min( (1:length(auxi$y))[auxi$y/MSYMedianC >= 0.95] ) ]
    FmsyupperMedianC <- auxi$x[ max( (1:length(auxi$y))[auxi$y/MSYMedianC >= 0.95] ) ]
    
    auxi <- stats::approx(Fscan, lans[4, ],xout=seq(min(Fscan),max(Fscan),length=200))
    FmsyMedianL <- auxi$x[which.max(auxi$y)]
    MSYMedianL <- max(auxi$y)
    
    # Value of F that corresponds to 0.95*MSY:
    FmsylowerMedianL <- auxi$x[ min( (1:length(auxi$y))[auxi$y/MSYMedianL >= 0.95] ) ]
    FmsyupperMedianL <- auxi$x[ max( (1:length(auxi$y))[auxi$y/MSYMedianL >= 0.95] ) ]
    
    F5percRiskBlim <- flim
    
    refs_interval <- data.frame(FmsyMedianC = FmsyMedianC,
                                FmsylowerMedianC = FmsylowerMedianC,
                                FmsyupperMedianC = FmsyupperMedianC,
                                FmsyMedianL = FmsyMedianL,
                                FmsylowerMedianL = FmsylowerMedianL,
                                FmsyupperMedianL = FmsyupperMedianL,
                                F5percRiskBlim = F5percRiskBlim,
                                Btrigger = Btrigger)
    
    # END 2014-03-12 Ammendments per note from Carmen/John
    
  }
  
  sim <- list(ibya = list(Mat = Mat, M = M, Fprop = Fprop, Mprop = Mprop,
                          west = west, weca = weca, sel = sel),
              rbya = list(ferr=ferr),
              rby = fit$rby,
              rbp = rbp,
              Blim = Blim,
              Bpa = Bpa,
              Refs = Refs,
              pProfile = pProfile,
              id.sim = fit$id.sr,
              refs_interval = refs_interval,
              rhologRec = rhologRec,
              simStks = simStks)    #AC
  
  if (calc.RPs){
    if (verbose) icesTAF::msg("Calculating MSY range values")
    sim <- eqsim_range(sim)
  }
  
  return(sim)
  
}
