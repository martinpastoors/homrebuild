
HS_fit_fixBlim <- function(fit, pair.years=NULL){
  
  # fit=fitfromweb(name,character.only=TRUE)
  if (missing(pair.years)) pair.years=1:fit$data$noYears else pair.years=which(fit$data$years%in%pair.years) # choose SR pairs
  
  lag <- fit$conf$minAge
  
  if (lag>0) {
    y=rectable(fit)[pair.years[-(1:lag)],1]
    x=ssbtable(fit)[pair.years[-((length(pair.years)-(lag-1)):length(pair.years))],1]
  } else {
    y=rectable(fit)[pair.years,1]
    x=ssbtable(fit)[pair.years,1]
  }
  
  fn <- function(par){
    i <- (x < exp(log(Blim)))
    slope <- exp(par) / exp(log(Blim))
    sum( (log(y[i]) - log(slope * x[i]))^ 2 ) + sum( ( log(y[!i]) - par )^ 2  )
  }
  # n = 100
  # gr <- expand.grid(x=seq(log(min(x)),log(max(x)),length=n), y=seq(log(min(y)),log(max(y)),length=n))
  # m <- apply(gr,1,fn)
  # # Get close to global optimum
  # par = unlist(gr[which.min(m),]) # par are x,y coordinates for inflexion point
  # # Find the exact global optimum
  # par[1]=log(Blim)
  opt <- optimize( fn, interval=c(log(min(y)), log(max(y))) )
  par=c(log(Blim), opt$minimum)
  
  log.pred.y <- y
  i <- (x < exp(par[1]))
  slope <- exp(par[2]) / exp(par[1])
  log.pred.y[i] <- log(slope * x[i])
  log.pred.y[!i] <-(par[2])
  
  sd.log.y <- sd(log(y)-log.pred.y)
  # CI.low <- log.pred.y-1.96*sd.log.y
  # CI.up <- log.pred.y+1.96*sd.log.y
  #
  # plot(y=log(y), x=log(x), pch=16, type="p")
  # lines(log.pred.y~log(x))
  # polygon(x=c(sort(log(x)),rev(sort(log(x)))), y=c(sort(CI.low),rev(sort(CI.up))), col=rgb(0,0,0,alpha=0.2), border = NA)
  #
  # plot(y=y, x=x, pch=16, type="o", col="red")
  # lines(exp(log.pred.y)~x)
  # polygon(x=c(sort(x),rev(sort(x))), y=c(sort(exp(CI.low)),rev(sort(exp(CI.up)))), col=rgb(0,0,0,alpha=0.2), border = NA)

  return(c(as.vector(par), sd.log.y))
  #save.image(file=paste0("~/DTU/Manuscripts/MSE_forecast_paper/results/HS_param_", name, ".Rdata"))
  
}
