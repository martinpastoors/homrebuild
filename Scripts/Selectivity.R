#Exploration of selectivity

#setup

source(file.path(getwd(),"Scripts","01_EqSim_setup.R"))

ac <- function(s){as.character(s)}

#stock files
FLStockfile    <- "WGWIDE19.RData"   #point estimates
FLStockSimfile <- "MSE_WGWIDE19_FLStocks_1k15PG.RData"  #1000 iters

ages <- seq(0,15)
nAges <- length(ages)

#2019 assessment, 1000 iterations
FLS <- loadRData(file.path(RData.dir,FLStockfile)) %>% FLCore::setPlusGroup(., 15)
FLSs <- loadRData(file.path(RData.dir,FLStockSimfile))
plot(FLS)

fGetFLStockSelection <- function(stk){
  sel <- matrix(FLCore::harvest(stk), ncol = dim(stk)[2])
  Fbar <- matrix(FLCore::fbar(stk), ncol = dim(stk)[2])
  sel <- sweep(sel, 2, Fbar, "/")
  sel <- sel/max(sel)
}

#extract selection info from each iteration
Sels <- lapply(FLSs,fGetFLStockSelection)
iSel <- matrix(unlist(Sels),nrow=nAges,ncol=length(FLSs),dimnames=list(age=ac(ages),iter=seq(1,length(FLSs))))

dfSASelection = data.frame(Age = ages, iter=0, Sel = rowMeans(fGetFLStockSelection(FLS)), stringsAsFactors = FALSE)
dfSASelection <- within(dfSASelection, Age <- factor(Age, levels = ac(ages)))

dfSASelection <- dplyr::bind_rows(dfSASelection,data.frame(Age = ac(ages), iter=rep(seq(1,length(FLSs)),each=nAges), Sel = c(iSel)))
#make Age a factor so plot order appropriate
dfSASelection <- within(dfSASelection, Age <- factor(Age, levels = ac(ages)))

#selection patterns

#data, assessment point estimates and range of values from 1000 iters
gSelProfs <- ggplot(data = dfSASelection) + geom_line(aes(x=Age,y=Sel,group=iter)) +
  geom_line(filter(dfSASelection,iter==0),mapping=aes(x=Age,y=Sel,group=1),col="red",lwd=1)


#distributions of selection@age
#distribution of values from 1000 iterations, point estimates from assessment
#sort stupid long x ais labels
gSelHistatAge <- ggplot(data = dfSASelection, mapping = aes(Sel)) + 
  geom_bar() + scale_x_binned(n.breaks=15, nice.breaks=FALSE) +
  theme(text = element_text(size=10), axis.text.x = element_text(angle=45, hjust=1)) +
  geom_vline(xintercept=c(0.25,0.5,0.75), col="grey", lwd=1, lty=2) +
  facet_wrap(~Age) + ylab("Count") +
  theme(axis.text.x=element_blank())


  

#quick look at an ouput 
load(file=file.path(Res.dir,"WHOM_SS3_OM2.2_MP5.23_1000_23","WHOM_SS3_OM2.2_MP5.23_1000_23_SimRuns.RData"))
#const F
load(file=file.path(Res.dir,"WHOM_SS3_OM2.2_MP5.00_1000_23","WHOM_SS3_OM2.2_MP5.00_1000_23_SimRuns.RData"))
names(SimRuns)
t<-SimRuns[[4]]$Sel  #target 0.075

dim(t)
#all iters, year 1
plot(seq(0,15),t[,1,1], type="l")
for (iter in seq(1,1000)) {lines(seq(0,15),t[,1,iter])}
#all iters, year 2
plot(seq(0,15),t[,2,1], type="l")
for (iter in seq(1,1000)) {lines(seq(0,15),t[,2,iter])}
#all years, iter 1 - this is the assessment output
plot(seq(0,15),t[,1,1], type="l")
for (yr in seq(1,23)) {lines(seq(0,15),t[,yr,1])}
#all years, iter 2
plot(seq(0,15),t[,1,2], type="l")
for (yr in seq(1,23)) {lines(seq(0,15),t[,yr,2])}

#catch weight
tcw<-SimRuns[[4]]$catW
dim(tcw)

#10 iters, year 1
plot(seq(0,15),tcw[,1,1], type="l")
for (iter in seq(1,10)) {lines(seq(0,15),tcw[,1,iter])}
#10 iters, year 2
plot(seq(0,15),tcw[,2,1], type="l")
for (iter in seq(1,10)) {lines(seq(0,15),tcw[,2,iter])}
#all years, iter 1 - this is the assessment output
plot(seq(0,15),tcw[,1,1], type="l")
for (yr in seq(1,23)) {lines(seq(0,15),tcw[,yr,1])}
#all years, iter 2 - this is the assessment output
plot(seq(0,15),tcw[,1,2], type="l")
for (yr in seq(1,23)) {lines(seq(0,15),tcw[,yr,2])}


tsw<-SimRuns[[4]]$stkW
dim(t)

#10 iters, year 1
plot(seq(0,15),tsw[,1,1], type="l")
for (iter in seq(1,10)) {lines(seq(0,15),tsw[,1,iter])}

#all iters, year 2
plot(seq(0,15),tsw[,2,1], type="l")
for (iter in seq(1,10)) {lines(seq(0,15),tsw[,2,iter])}
#all years, iter 1 - this is the assessment output
plot(seq(0,15),tsw[,1,1], type="l")
for (yr in seq(1,23)) {lines(seq(0,15),tsw[,yr,1])}
#all years, iter 2 - this is the assessment output
plot(seq(0,15),tsw[,1,2], type="l")
for (yr in seq(1,23)) {lines(seq(0,15),tsw[,yr,2])}
