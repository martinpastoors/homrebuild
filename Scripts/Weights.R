#Exploration of stock/catch weights

#options for implementing stochastic weights in the EqSim runs for WHM

#setup

source(file.path(getwd(),"Scripts","01_EqSim_setup.R"))

ac <- function(s){as.character(s)}

#stock files
FLStockfile    <- "WGWIDE19.RData"   #point estimates
FLStockSimfile <- "MSE_WGWIDE19_FLStocks_1k15PG.RData"  #1000 iters

ages <- seq(0,15)
nAges <- length(ages)

#read in the historic catch datasets
#stock/catch weights (SS3 models catch/stock weights at age as time invariant)
dfSW <- readr::read_delim(file = file.path(Data.dir,"StockWeights.dat"),delim=",")
dfCW <- readr::read_delim(file = file.path(Data.dir,"CatchWeights.dat"),delim=",")

#single df
dfWeights <- dplyr::left_join(
  dfSW %>% pivot_longer(cols = paste0("Age",seq(0,15)), names_to = "Age", values_to = "SW", names_prefix = "Age"),
  dfCW %>% pivot_longer(cols = paste0("Age",seq(0,15)), names_to = "Age", values_to = "CW", names_prefix = "Age"),
  by=c("Year","Age"))

dfWeights <- within(dfWeights, Age <- factor(Age, levels = ac(ages)))

head(dfWeights)

#summary
dfSummaryWgt <- data.frame(Age=ages)

#2019 assessment
FLS <- loadRData(file.path(RData.dir,FLStockfile)) %>% FLCore::setPlusGroup(., 15)
plot(FLS)
#stock assessment weights (iter 0 is the assessment op)
dfSAWeights = data.frame(Age = ages, iter=0, CW = rowMeans(catch.wt(FLS)), SW = rowMeans(stock.wt(FLS)), stringsAsFactors = FALSE)
dfSAWeights <- within(dfSAWeights, Age <- factor(Age, levels = ac(ages)))

#1000 iters
#start with simulated initial populations
FLSs <- loadRData(file.path(RData.dir,FLStockSimfile))
length(FLSs)

#extract catch, stock weight info from each iteration
FLSs.CW <- lapply(lapply(FLSs,FLCore::catch.wt),rowMeans)
FLSs.SW <- lapply(lapply(FLSs,FLCore::stock.wt),rowMeans)
iCW <- matrix(unlist(FLSs.CW),nrow=nAges,ncol=length(FLSs),dimnames=list(age=ac(ages),iter=seq(1,length(FLSs))))
iSW <- matrix(unlist(FLSs.SW),nrow=nAges,ncol=length(FLSs),dimnames=list(age=ac(ages),iter=seq(1,length(FLSs))))
dfSAWeights <- dplyr::bind_rows(dfSAWeights,data.frame(Age = ac(ages), iter=rep(seq(1,length(FLSs)),each=nAges), CW = c(iCW), SW = c(iSW)))
#make Age a factor so plot order appropriate
dfSAWeights <- within(dfSAWeights, Age <- factor(Age, levels = ac(ages)))

#data, assessment point estimates and range of values from 1000 iters
gCW <- ggplot(data = dfWeights, mapping = aes(x=Year,y=CW)) +
  geom_line(aes(group=Age)) +
  geom_hline(data = filter(dfSAWeights,iter==0), mapping=aes(yintercept=CW), col="red") +
  geom_hline(data = dfSAWeights %>% group_by(Age) %>% summarise(mn = min(CW), mx = max(CW)), mapping=aes(yintercept=mx), col="red", lty=2) +
  geom_hline(data = dfSAWeights %>% group_by(Age) %>% summarise(mn = min(CW), mx = max(CW)), mapping=aes(yintercept=mn), col="red", lty=2) +
  facet_wrap(~Age) + ylab("Catch Weight")

#distribution of values from 1000 iterations, point estimates from assessment
#sort stupid long x ais labels
ggplot(data = dfSAWeights, mapping = aes(CW)) + 
  geom_bar() + scale_x_binned(n.breaks=10, nice.breaks=FALSE) +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1)) +
  geom_vline(data = filter(dfSAWeights, iter==0), mapping=aes(xintercept=CW), col="red", lwd=1) +
  facet_wrap(~Age, scales="free") + ylab("Catch Weight")


