# sam.fit2
# silent version of sam.fit, developed by Vanessa Trijoulet
# 23/2/2020

sam.fit2 <- function (data, conf, parameters, newtonsteps = 3, rm.unidentified = FALSE, 
                      run = TRUE, 
                      lower = stockassessment:::getLowerBounds(parameters), 
                      upper = stockassessment:::getUpperBounds(parameters), 
                      sim.condRE = TRUE, ignore.parm.uncertainty = FALSE, rel.tol = 1e-10, 
                      ...) 
{
  if (length(conf$maxAgePlusGroup) == 1) {
    tmp <- conf$maxAgePlusGroup
    conf$maxAgePlusGroup <- defcon(data)$maxAgePlusGroup
    conf$maxAgePlusGroup[1] <- tmp
  }
  definit <- defpar(data, conf)
  if (!identical(parameters, relist(unlist(parameters), skeleton = definit))) {
    warning("Initial values are not consistent, so running with default init values from defpar()")
    parameters <- definit
  }
  data <- stockassessment:::clean.void.catches(data, conf)
  confTmp = defcon(data)
  for (i in 1:length(confTmp)) {
    if (!names(confTmp)[i] %in% names(conf)) {
      conf[[length(conf) + 1]] = confTmp[[i]]
      names(conf)[length(conf)] = names(confTmp)[i]
    }
  }
  tmball <- c(data, conf, list(simFlag = rep(as.integer(sim.condRE), 
                                             length = 2)))
  if (is.null(tmball$resFlag)) {
    tmball$resFlag <- 0
  }
  nmissing <- sum(is.na(data$logobs))
  parameters$missing <- numeric(nmissing)
  ran <- c("logN", "logF", "missing")
  args <- c(list(data = tmball, parameters = parameters, random = ran, 
                 DLL = "stockassessment"), list(...))
  mapRP <- list(logFScaleMSY = factor(NA), implicitFunctionDelta = factor(NA), 
                logScaleFmsy = factor(NA), logScaleFmax = factor(NA), 
                logScaleF01 = factor(NA), logScaleFcrash = factor(NA), 
                logScaleFext = factor(NA), logScaleFxPercent = factor(rep(NA, 
                                                                          length(args$parameters$logScaleFxPercent))), logScaleFlim = factor(NA))
  if (is.null(args$map) || !is.list(args$map) || length(args$map) == 
      0) {
    args$map <- mapRP
  }
  else {
    args$map <- c(args$map, mapRP)
  }
  if (!is.null(conf$hockeyStickCurve)) 
    if (is.null(args$map$rec_pars) & !is.na(conf$hockeyStickCurve) & 
        conf$stockRecruitmentModelCode == 63) 
      args$map$rec_pars = factor(c(1, 2, NA))
  obj <- do.call(MakeADFun, args)
  if (rm.unidentified) {
    gr <- obj$gr()
    safemap <- obj$env$parList(gr)
    safemap <- safemap[!names(safemap) %in% ran]
    safemap <- lapply(safemap, function(x) factor(ifelse(abs(x) > 
                                                           1e-15, 1:length(x), NA)))
    ddd <- list(...)
    if (!is.null(ddd$map)) {
      safemap <- c(ddd$map, safemap)
      ddd$map <- safemap
      ddd$data <- tmball
      ddd$parameters <- parameters
      ddd$random <- ran
      ddd$DLL <- "stockassessment"
      obj <- do.call(MakeADFun, ddd)
    }
    else {
      obj <- MakeADFun(tmball, parameters, random = ran, 
                       map = safemap, DLL = "stockassessment", 
                       ...)
    }
  }
  lower2 <- rep(-Inf, length(obj$par))
  upper2 <- rep(Inf, length(obj$par))
  for (nn in names(lower)) lower2[names(obj$par) == nn] = lower[[nn]]
  for (nn in names(upper)) upper2[names(obj$par) == nn] = upper[[nn]]
  if (!run) 
    return(list(sdrep = NA, pl = parameters, plsd = NA, data = data, 
                conf = conf, opt = NA, obj = obj))
  opt <- nlminb(obj$par, obj$fn, obj$gr, control = list(trace = 0, 
                                                        eval.max = 2000, iter.max = 1000, rel.tol = rel.tol), 
                lower = lower2, upper = upper2)
  for (i in seq_len(newtonsteps)) {
    g <- as.numeric(obj$gr(opt$par))
    h <- optimHess(opt$par, obj$fn, obj$gr)
    opt$par <- opt$par - solve(h, g)
    opt$objective <- obj$fn(opt$par)
  }
  rep <- obj$report()
  sdrep <- sdreport(obj, opt$par, ignore.parm.uncertainty = ignore.parm.uncertainty)
  idx <- c(which(names(sdrep$value) == "lastLogN"), which(names(sdrep$value) == 
                                                            "lastLogF"))
  sdrep$estY <- sdrep$value[idx]
  sdrep$covY <- sdrep$cov[idx, idx]
  idx <- c(which(names(sdrep$value) == "beforeLastLogN"), 
           which(names(sdrep$value) == "beforeLastLogF"))
  sdrep$estYm1 <- sdrep$value[idx]
  sdrep$covYm1 <- sdrep$cov[idx, idx]
  pl <- as.list(sdrep, "Est")
  plsd <- as.list(sdrep, "Std")
  sdrep$cov <- NULL
  ret <- list(sdrep = sdrep, pl = pl, plsd = plsd, data = data, 
              conf = conf, opt = opt, obj = obj, rep = rep, low = lower, 
              hig = upper)
  attr(ret, "RemoteSha") <- substr(packageDescription("stockassessment")$RemoteSha, 
                                   1, 12)
  attr(ret, "Version") <- packageDescription("stockassessment")$Version
  class(ret) <- "sam"
  return(ret)
}
