# ================================================================================================================
# EqSim HCR simulator
# 
# 02_EqSim_simulate.R
# 
# The EqSim simulator was developed by David Miller and Colin Millar
# Further enhanced by Andy Campbell and Martin Pastoors and applied to Western Horse mackerel in June 2020
#
# 24/06/2020 generic option; code now independent of fish stock
# 25/06/2020 tested on mackerel stock
# 27/06/2020 tested on 1000 iters of SAM assessment
# 29/06/2020 modified and bugchecked by Andy Campbell
# 01/07/2020 included additional features by Martin Pastoors
# ================================================================================================================

# source(file.path(getwd(),"Scripts","01_EqSim_setup.R"))

#Note: niters and nyr could be included in the OM or MP definitions

#basic simulation settings
#niters <- 10000
#niters <- 1000
niters <- 100
nyr <- 20

# simulation periods
per1 <- 5
per2 <- 5
# per3 is simply the remainder

#OM <- OM2; MP <- MP2.0_10000
#OM <- OM2; MP <- MP3.0

#OM <- OM2.1   #WGWIDE 2019, const weights, selection
OM <- OM2.2   #WGWIDE 2019, stochastic weights, selection

#testing
#MP <- MP1.0   #baseline, constant F harvest rule, no IAV control, no minimum TAC, no assessment/advice error
#MP <- MP1.1   #baseline, constant F harvest rule, no IAV control, 80kt minimum TAC, no assessment/advice error
#MP <- MP1.2   #baseline, constant F harvest rule, no IAV control, 150kt maximum TAC, no assessment/advice error
#MP <- MP1.3   #baseline, constant F harvest rule, no IAV control, 80kt min TAC, 150kt max TAC, no assessment/advice error
#MP <- MP1.4   #baseline, constant F harvest rule, no IAV control, no min/max TAC, includes assessment/advice error
#MP <- MP1.5   #20% IAV Test
#MP <- MP1.6   #30% IAV Test
#MP <- MP1.7   #10% IAV Test
#MP <- MP1.8   #10%/20% asymmetric IAV Test
#MP <- MP1.9   #0%/10% asymmetric IAV Test

#MP <- MP5.00    #Constant F
#MP <- MP5.01   #Const , min TAC = 50kt
#MP <- MP5.02   #Const , 20% IAV
MP <- MP5.03   #Const , 20% IAV, only above Btrigger

#MP <- MP5.1    #ICES AR
#MP <- MP5.11   #ICES AR, min TAC = 50kt
#MP <- MP5.12   #ICES AR, 20% IAV
#MP <- MP5.13   #ICES AR, 20% IAV, only above Btrigger

#MP <- MP5.2    #Double BP
#MP <- MP5.21   #Double BP, min TAC = 50kt
#MP <- MP5.22   #Double BP, 20% IAV
#MP <- MP5.23   #Double BP, 20% IAV, only above Btrigger

#test
#MP <- MP99
#MP <- MP98

stock          <- "WHOM"
assess         <- "SS3"
runName <- paste(stock,assess,OM$code,MP$code,niters,nyr,sep="_")
#runName <- paste(OM$code,MP$code,niters,nyr,sep="_")

# set up the OM =========================================================================================================

#assessment FLStock
load(file = file.path(RData.dir,"WGWIDE19.RData"))
FLStockfile    <- "WGWIDE19.RData"

#reduce plus group to 15, to match available data for stock and catch weights
WHOM.WGWIDE2019 <- FLCore::setPlusGroup(WHOM.WGWIDE2019,15)

#Blim <- min(ssb(WHOM.WGWIDE2019))
#The IBP in 2019 selected SSB in 2003 as a proxy for Bpa and derived Blim from this (Bpa/1.4)
#given the lack of any clear SRR and sensitivity of the proportions of mixed models
#to individual data points, a segmented regression with breakpoint at Blim is the default SRR

# MP 24/6/2020: this seems wrong; segmented regression should go through Blim, not through Bloss
# Blimloss <- min(OM$refPts$Blim,min(ssb(WHOM.WGWIDE2019)))
Blimloss <- OM$refPts$Blim
SegregBlim  <- function(ab, ssb) log(ifelse(ssb >= Blimloss, ab$a * Blimloss, ab$a * ssb))

set.seed(1)

#segmented regression with breakpoint at Blim, from 1995 excluding terminal
SRR <- eqsr_fit(window(WHOM.WGWIDE2019,1995,2018), remove.years = c(2018), nsamp=niters, models = c("SegregBlim"))

emf(file = file.path(Res.dir,"SRR.emf"), width = 7, height = 7)
eqsr_plot(SRR)
dev.off()

#stock/catch weights (SS3 models catch/stock weights at age as time invariant)
dfSW <- readr::read_delim(file = file.path(Data.dir,"StockWeights.dat"),delim=",")
dfCW <- readr::read_delim(file = file.path(Data.dir,"CatchWeights.dat"),delim=",")

dfWeights <- dplyr::left_join(
  dfSW %>% pivot_longer(cols = paste0("Age",seq(0,15)), names_to = "Age", values_to = "SW", names_prefix = "Age"),
  dfCW %>% pivot_longer(cols = paste0("Age",seq(0,15)), names_to = "Age", values_to = "CW", names_prefix = "Age"),
  by=c("Year","Age"))

dfWeights <- within(dfWeights, Age <- factor(Age, levels = as.character(seq(0,15))))
dfSAWeights = data.frame(Age = seq(0,15), CW = rowMeans(catch.wt(WHOM.WGWIDE2019)), SW = rowMeans(stock.wt(WHOM.WGWIDE2019)), stringsAsFactors = FALSE)
dfSAWeights <- within(dfSAWeights, Age <- factor(Age, levels = as.character(seq(0,15))))

#quick look, comparing with the assessment ouput
gCW <- ggplot(data = dfWeights, mapping = aes(x=Year,y=CW)) +
  geom_line(aes(group=Age)) +
  geom_hline(data = dfSAWeights, mapping=aes(yintercept=CW), col="red") + 
  facet_wrap(~Age) + ylab("Catch Weight")

emf(file = file.path(Res.dir,"CW.emf"), width = 7, height = 7)
print(gCW)
dev.off()

gSW <- ggplot(data = dfWeights, mapping = aes(x=Year,y=SW)) +
  geom_line(aes(group=Age)) +
  geom_hline(data = dfSAWeights, mapping=aes(yintercept=SW), col="red") + 
  facet_wrap(~Age) + ylab("Stock Weight")

emf(file = file.path(Res.dir,"SW.emf"), width = 7, height = 7)
print(gSW)
dev.off()

read_docx() %>%
  body_add("Catch Weights") %>%
  body_add_img(src = file.path(Res.dir,"CW.emf"), width = 7, height = 7) %>% 
  body_add_break() %>%
  body_add("Stock Weights") %>%
  body_add_img(src = file.path(Res.dir,"SW.emf"), width = 7, height = 7) %>% 
  body_add_break() %>%
  body_add("Stock Weights") %>%
  body_add_img(src = file.path(Res.dir,"SRR.emf"), width = 7, height = 7) %>% 
  print(target = file.path(Res.dir,"Graphics.docx"))

#remove temp graphics
sapply(list.files(path=file.path(Res.dir), pattern=".emf", full.names=TRUE), file.remove)

#assign the stock and catch weights into the assessment FLStock object
tSW <- FLQuant(t(dfSW[,-1]), dim=dim(stock.wt(WHOM.WGWIDE2019)), dimnames=dimnames(stock.wt(WHOM.WGWIDE2019)))
tCW <- FLQuant(t(dfCW[,-1]), dim=dim(catch.wt(WHOM.WGWIDE2019)), dimnames=dimnames(catch.wt(WHOM.WGWIDE2019)))

#remove the weights assignment for now until further investigated
#stock.wt(WHOM.WGWIDE2019) <- tSW
#catch.wt(WHOM.WGWIDE2019) <- tCW

#reassign FLStock object with updated weights into stk slot of SRR 
SRR$stk <- WHOM.WGWIDE2019

# #initial populations
# load(file=file.path(RData.dir,"MSE_WGWIDE19_FLStocks_15PG.RData"))
# 
# #weights - compare those in the 1000 iteration with those from the final assessment - seems close enough
# swaa <- array(NA, dim=c(16,1000), dimnames=list("Age"=seq(0,15), Iter=seq(1,1000)))
# mat <- array(NA, dim=c(16,1000), dimnames=list("Age"=seq(0,15), Iter=seq(1,1000)))
# abd <- array(NA, dim=c(16,1000), dimnames=list("Age"=seq(0,15), Iter=seq(1,1000)))
# for (i in 1:1000){
#   swaa[,i] <- as.numeric(stock.wt(lWHM[[i+1]])[,'2018'])
#   abd[,i] <- as.numeric(stock.n(lWHM[[i+1]])[,'2018'])
#   mat[,i] <- as.numeric(mat(lWHM[[i+1]])[,'2018'])
# }
# apply(swaa, MARGIN=1, FUN=median)/stock.wt(WHOM.WGWIDE2019)[,'2018']
# apply(mat, MARGIN=1, FUN=median)/mat(WHOM.WGWIDE2019)[,'2018']
# apply(abd, MARGIN=1, FUN=median)/stock.n(WHOM.WGWIDE2019)[,'2018']
# bio <- swaa*mat*abd
# hist(colSums(bio))
# median(colSums(bio))
# 
# #in simulation weights from assessment are used
# bio.sim <- as.numeric(stock.wt(WHOM.WGWIDE2019)[,'2018'])*mat*abd
# hist(colSums(bio.sim))
# median(colSums(bio.sim))

#load(file=file.path(RData.dir,"MSE_WGWIDE19_FLStocks_10k.RData"))
#for (ii in 1:10001){lWHM[[ii]]<-FLCore::setPlusGroup(lWHM[[ii]],15)}
#save(lWHM,file=file.path(RData.dir,"MSE_WGWIDE19_FLStocks_10k15PG.RData"))

#load(file=file.path(RData.dir,"MSE_WGWIDE19_FLStocks_10k15PG.RData"))
load(file=file.path(dropbox.dir,"MSE_WGWIDE19_FLStocks_10k15PG.RData"))
FLStockSimfile <- "MSE_WGWIDE19_FLStocks_10k15PG.RData"


# ssb.10k <- lapply(lWHM,ssb)
# dfssb.2018.10k <- data.frame(iter=seq(2,10001),ssb=rep(NA,10000))
# for (i in 2:10001){dfssb.2018.10k$ssb[i-1] <- as.numeric(ssb.10k[[i]][,'2018'])}
# quantile(dfssb.2018.10k$ssb,probs=c(0.025,0.5,0.975))
#2.5%       50%     97.5% 
#576016.1  841177.3 1146767.2 

# iters <- c()
# #2.3% from less than 563kt
# iters <- c(iters,sample(dfssb.2018.10k$iter[dfssb.2018.10k$ssb<=563000], size=23, replace=FALSE))
# #4.4% from 563-625.25
# iters <- c(iters,sample(dfssb.2018.10k$iter[dfssb.2018.10k$ssb>563000 & dfssb.2018.10k$ssb<=625250], size=44, replace=FALSE))
# #9.2% from 625.25 to 687.5
# iters <- c(iters,sample(dfssb.2018.10k$iter[dfssb.2018.10k$ssb>625250 & dfssb.2018.10k$ssb<=687500], size=92, replace=FALSE))
# #15% from 687.5 to 749.75
# iters <- c(iters,sample(dfssb.2018.10k$iter[dfssb.2018.10k$ssb>687500 & dfssb.2018.10k$ssb<=749750], size=150, replace=FALSE))
# #19.1% from 749.75 to 812
# iters <- c(iters,sample(dfssb.2018.10k$iter[dfssb.2018.10k$ssb>749750 & dfssb.2018.10k$ssb<=812000], size=191, replace=FALSE))
# #19.1% from 812 to 874.25
# iters <- c(iters,sample(dfssb.2018.10k$iter[dfssb.2018.10k$ssb>812000 & dfssb.2018.10k$ssb<=874250], size=191, replace=FALSE))
# #15% from 874.25 to 936.5
# iters <- c(iters,sample(dfssb.2018.10k$iter[dfssb.2018.10k$ssb>874250 & dfssb.2018.10k$ssb<=936500], size=150, replace=FALSE))
# #9.2% from 936.5 to 998.75
# iters <- c(iters,sample(dfssb.2018.10k$iter[dfssb.2018.10k$ssb>936500 & dfssb.2018.10k$ssb<=998750], size=92, replace=FALSE))
# #4.4% from 998.75 to 1061
# iters <- c(iters,sample(dfssb.2018.10k$iter[dfssb.2018.10k$ssb>998750 & dfssb.2018.10k$ssb<=1061000], size=44, replace=FALSE))
# #2.3% over 1061
# iters <- c(iters,sample(dfssb.2018.10k$iter[dfssb.2018.10k$ssb>1061000], size=23, replace=FALSE))
# 
# #randomise the iteration order
# iters <- sample(iters,1000,replace=FALSE)

iters <- c(6109,8929,9359,7959,3037,899,1237,8485,3811,4445,3232,9431,8336,5602,6846,1620,2857,9951,7240,6807,433,3766,9333,
           9412,8810,6810,7277,5760,6372,144,4636,7812,216,8531,40,65,7438,5610,2513,3060,3791,2889,1000,9710,3183,4543,
           2077,3125,2388,8703,7794,402,7125,6207,2174,2826,3478,7837,1334,2504,1632,748,8734,7482,4340,3871,1676,2285,3565,
           9943,1354,4691,8250,1125,6790,2105,8376,9205,1001,7537,1745,2773,6787,9660,4796,2138,2153,2386,7035,5877,302,8374,
           6645,7444,3617,8615,891,5233,5829,7434,3534,1655,578,5779,1963,6105,9329,5388,2268,7248,5913,9136,233,6131,6911,
           8620,2508,6743,8716,5359,4185,8307,3568,8961,4152,5426,3109,6313,1588,6342,2509,1035,1871,555,4364,9860,6900,4420,
           5730,3720,5379,6544,8902,315,8744,6371,5948,7406,7899,6110,503,821,8785,7618,6405,4473,5227,5153,6271,5260,9050,
           1387,3186,6485,617,8076,1892,4790,4363,8028,5075,8796,7670,468,6045,4359,7123,8959,8110,7478,5280,4083,8524,3142,
           8133,3887,1674,1867,5315,3359,7779,2688,7458,6031,7839,5015,4459,9449,4432,1520,5619,1504,9238,8687,9661,8830,5999,
           3780,15,7830,8756,839,9281,4131,4375,4794,2489,259,8595,264,2487,6032,843,4833,2560,8778,38,6133,4080,3369,
           8661,1210,5622,7551,1681,5795,1459,2294,6594,3459,2067,203,1133,3040,3085,4696,9240,6757,7322,5825,8549,7384,9308,
           4959,172,263,1294,1394,4668,769,6724,3344,4683,651,3476,9712,7445,4435,1911,1408,2949,3653,4708,8897,3001,5714,
           5957,2265,1397,6964,3320,20,6862,969,5528,9212,7231,9196,2011,9581,6408,1319,8147,8092,1375,2349,9362,9403,6697,
           1718,6621,2526,3513,515,4498,5587,9655,5487,7525,8675,4518,9448,3753,9510,2767,3757,6815,6468,6998,8978,7816,4861,
           7002,802,2181,3878,6368,9539,4575,106,3755,6272,869,7685,4079,5854,2118,5906,9401,7606,3180,6294,6202,8153,8995,
           9364,5991,7626,4171,5725,5844,2633,9813,401,2242,432,1960,8539,8974,7207,1550,9137,3559,8300,2478,9191,751,4767,
           3175,1486,749,8685,7220,5284,5881,4014,4655,954,1268,1389,2999,1204,8560,6336,1653,3450,7975,4520,5258,7145,6548,
           8126,4889,1841,1586,7135,1434,2989,2732,9186,6857,7382,2969,2186,1539,2401,3816,8784,9610,1989,6116,4746,4807,1984,
           811,8326,3393,3384,5975,4169,2408,5072,6798,9150,7570,7093,4986,5288,6547,1105,4992,7391,3700,5691,9257,4726,516,
           9487,3081,6064,1216,1844,7924,804,208,8106,8832,5738,4029,3269,1587,7821,3401,1967,1042,3563,4463,9363,2967,7501,
           3484,7915,1275,9803,4929,1974,3810,6317,8608,7167,1318,8892,8476,3015,6928,9366,8127,2148,5098,443,5490,8418,5956,
           7728,7442,3807,5264,2836,7976,6320,7162,6791,5745,7284,9940,2804,9604,5119,8864,3254,1115,2637,8873,4808,5952,1868,
           9906,9654,8350,6991,4687,775,2884,4669,1951,7992,3016,5064,1016,9971,3028,112,6287,738,2039,1183,72,5102,5149,
           7666,7530,2151,8032,5196,6916,383,3914,9936,1599,6600,941,3487,3010,4096,554,1293,6795,2691,7061,5364,4945,4127,
           2303,7639,8473,3066,220,9684,5955,3200,8885,1529,100,9413,5413,3665,6515,3987,2510,5175,3590,1623,2295,9534,8514,
           5816,7906,8280,7390,3947,9464,8650,3281,2019,8526,9495,3981,4843,2632,4480,5645,5733,7218,786,1983,1723,3806,5683,
           3249,2132,326,2745,209,5445,1347,1344,6700,8027,5094,3216,7320,7165,9863,5406,8448,1902,1845,4170,2471,9477,9045,
           498,1187,9461,7736,6667,7377,2815,3501,810,6082,7966,5303,5967,946,5219,2720,4057,8845,601,3967,4734,1423,6306,
           2353,3702,138,3628,2279,9087,4118,7625,7865,3504,3167,1608,6556,6528,3854,6276,5950,567,1056,4066,1942,5434,691,
           5993,2178,5088,3006,4303,6132,7971,218,1485,6888,5263,7295,4222,1543,2916,636,387,7515,7084,2539,1399,5200,7340,
           9266,8774,607,4067,6097,30,5036,6343,8361,7929,6280,1849,4707,1356,253,619,8188,6793,1916,3885,9204,1156,4285,
           2630,3678,1787,8318,4302,533,7003,462,4194,6866,2352,7513,1630,5689,9968,5107,1203,2045,1777,8440,431,1727,3196,
           8304,8195,2917,3465,9315,9811,1795,3132,798,7678,8413,3762,7520,8487,1424,5580,1402,7210,5226,6060,6730,6392,3448,
           8229,186,4205,2563,3447,1756,4572,5141,7101,1907,9993,710,6833,2013,7801,8483,6717,4820,6510,7723,3497,1265,7447,
           8498,716,1373,948,4490,6216,898,9838,8345,1695,9473,5185,3575,576,3148,6769,2669,7309,2346,6328,7561,9141,1182,
           7925,8132,2327,3370,439,5781,8124,8548,5892,119,8865,1450,3693,8044,1538,740,4864,5545,5688,4055,9512,4519,5856,
           7472,9695,7394,3784,7518,3052,5899,6046,5758,2855,7177,6709,6615,2556,6927,4951,5231,8655,9651,5206,9837,5960,9350,
           9997,4812,8795,8911,8986,2886,2037,1242,968,6628,9750,8101,3287,5540,1033,1322,6256,9140,2956,5474,1742,2946,9294,
           2121,9003,2675,5985,3622,8423,6473,8060,2683,4507,7502,9479,867,8411,9491,2085,8816,4125,2187,7250,5201,8976,604,
           1988,1292,8758,9525,4226,4637,7389,8334,2206,6552,6434,9446,3752,8805,3098,1349,3899,9107,6438,5111,9459,6784,4580,
           5329,7895,5137,3891,8162,6726,8182,892,9111,408,2486,4658,9736,6160,320,6341,8645,5937,4663,2778,6702,284,7698,
           3500,8118,4657,6834,3240,5259,3585,6721,3306,3475,1185,5189,3352,5551,5503,4197,9383,3097,2082,996,623,6238,8272,
           2410,2847,8664,8570,6839,363,194,4919,8577,1232,5751,975,8955,8031,5559,2491,2968,1088,3102,1878,5005,889,5041,
           5121,4910,7550,1073,7392,2334,3308,5191,9061,3511,4151)

lWHM.10k <- lWHM
lWHM <- lWHM.10k[iters]

# #test the SSB distribution
# ssb.1k <- lapply(lWHM,ssb)
# ssb.2018 <- rep(NA,1000)
# for (i in 1:1000){ssb.2018[i] <- as.numeric(ssb.1k[[i]][,'2018'])}
# quantile(ssb.2018,probs=c(0.025,0.5,0.975))
# #2.5%       50%     97.5% 
# #568534.3  811861.6 1055037.4 
# #similar to 2019 assessment op

# lWHM2 <- lWHM
# ssb.10k <- lapply(lWHM2,ssb)
# ssb.2018.10k <- rep(NA,10000)
# for (i in 2:10001){ssb.2018.10k[i-1] <- as.numeric(ssb.10k[[i]][,'2018'])}
# ssb.2018 <- 811684
# below <- which(ssb.2018.10k<ssb.2018)
# above <- which(ssb.2018.10k>ssb.2018)
# 
# res <- data.frame(lower=c(),median=c(),upper=c(),samps=c(),stringsAsFactors = FALSE)
# 
# sink(file=file.path(getwd(),"search2parts.txt"))
# for (j in 1:100){
#    sampsbelow <- sample(below, size=500, replace=FALSE)
#    sampsabove <- sample(above, size=500, replace=FALSE)
#    lWHM3 <- lWHM[c(sampsbelow,sampsabove)]
#    t<-lapply(lWHM3,ssb)
#    tt<-rep(NA,1000)
#    for (i in 1:1000){tt[i] <- as.numeric(t[[i]][,'2018'])}
#    tq<-quantile(tt,probs=c(0.025,0.5,0.975))
#    if (tq[2]<830000 & tq[2]>800000) {
#      cat(tq[2],"close one!\n")
#      cat(tq,"\n")
#      cat(samps,"\n")
#      res <- dplyr::bind_rows(res,
#                              data.frame(lower=tq[1],median=tq[2],upper=tq[3],samps=paste(samps,collapse=","),stringsAsFactors = FALSE))
#      }
#  }
# sink()

#bootstrap from the 10k iterations to find a combination close to the assessment median
#res <- data.frame(lower=c(),median=c(),upper=c(),samps=c(),stringsAsFactors = FALSE)

# sink(file=file.path(getwd(),"search.txt"))
# for (j in 1:10000){
#   samps <- sample(seq(2,10000), size=1000, replace=FALSE)
#   lWHM3 <- lWHM[samps]
#   t<-lapply(lWHM3,ssb)
#   tt<-rep(NA,1000)
#   for (i in 1:1000){tt[i] <- as.numeric(t[[i]][,'2018'])}
#   tq<-quantile(tt,probs=c(0.025,0.5,0.975))
#   if (tq[2]<830000 & tq[2]>800000) {
#     cat(tq[2],"close one!\n")
#     cat(tq,"\n")
#     cat(samps,"\n")
#     res <- dplyr::bind_rows(res,
#                             data.frame(lower=tq[1],median=tq[2],upper=tq[3],samps=paste(samps,collapse=","),stringsAsFactors = FALSE))
#     }
# }
# sink()

#min(res$median)
#[1] 822303.3

#iters <- as.numeric(unlist(stringr::str_split(res$samps[res$median==min(res$median)],",")))


#test SSB range
#t<-lapply(SRR$stks,ssb)
#tt<-rep(NA,1000)
#for (i in 1:1000){tt[i] <- as.numeric(t[[i]][,'2018'])}
#quantile(tt,probs=c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975))
#2.5%        5%       10%       25%       50%       75%       90%       95%     97.5% 
#570452.6  616404.6  673512.5  756386.1  849037.9  950893.7 1033378.4 1080138.6 1128863.5 
#from 2019 assessment, SSB in 2018
#2.5%     50%       97.5%
#562585   811685    1060785

#add required number of stochastic FLStocks to FIT object
#SRR$stks <- lWHM[as.character(seq(1,niters))]
SRR$stks <- lWHM

# Define MP ================================================================================================================

#start,end,vectors of observation and simulation years
#simulation starts in assessment terminal year
minObsYear <- range(SRR$stk)["minyear"]
maxObsYear <- range(SRR$stk)["maxyear"]
obsYears <- ac(seq(minObsYear,maxObsYear))
yStart <- as.numeric(maxObsYear)
yEnd <- yStart + nyr - 1
simYears <- ac(seq(yStart,yEnd))

#exploitation constraints
#2018 catch known, 2019 as assumed during WGWIDE 2019, 2020 as advised
dfExplConstraints <- data.frame("Type" = c("Catch","Catch","Catch"), 
                                "YearNum" = c("1","2","3"),
                                "Val" = c(101682,110381,83954), 
                                stringsAsFactors = FALSE)

#test for recruitment failure, keep exploration constant at 80kt (regardless of HCR) for first 10 years during which recruitment
#failure is simulated (1/10 of normal). Then, all HCRs should start from same point
#dfExplConstraints <- data.frame("Type" = rep("Catch",10), 
#                                "YearNum" = as.character(seq(1,10)),
#                                "Val" = c(101682,110381,83954,rep(80000,7)), 
#                                stringsAsFactors = FALSE)


#min/max TAC
if (!is.na(MP$minTAC)) {
  dfExplConstraints <- dplyr::bind_rows(dfExplConstraints,
                                        data.frame("Type" = "MinTAC",
                                                   "YearNum" = "all",
                                                   "Val" = MP$minTAC,
                                                   stringsAsFactors = FALSE))
}

#min/max TAC
if (!is.na(MP$maxTAC)) {
  dfExplConstraints <- dplyr::bind_rows(dfExplConstraints,
                                        data.frame("Type" = "MaxTAC",
                                                   "YearNum" = "all",
                                                   "Val" = MP$maxTAC,
                                                   stringsAsFactors = FALSE))
}

#IAV
if (!any(is.na(MP$TAC_IAV))) {
  if (length(MP$TAC_IAV)==2){
    #  dfExplConstraints <- dplyr::bind_rows(dfExplConstraints,
    #                                        data.frame("Type" = "IAV",
    #                                                   "YearNum" = "all",
    #                                                   "Val" = MP$TAC_IAV,
    #                                                   stringsAsFactors = FALSE))
    dfExplConstraints <- dplyr::bind_rows(dfExplConstraints,
                                          data.frame("Type" = c("IAVInc","IAVDec"),
                                                     "YearNum" = c("all","all"),
                                                     "Val" = c(MP$TAC_IAV[1],MP$TAC_IAV[2]),
                                                     stringsAsFactors = FALSE))
  } else {
    stop("IAV needs to be vector of length 2 (limit on increase, limit on decrease)")
    #assume same for both
    dfExplConstraints <- dplyr::bind_rows(dfExplConstraints,
                                          data.frame("Type" = c("IAVInc","IAVDec"),
                                                     "YearNum" = c("all","all"),
                                                     "Val" = c(MP$TAC_IAV[1],MP$TAC_IAV[1]),
                                                     stringsAsFactors = FALSE))
  }
}

#statistical periods for reporting
lStatPer <- lStatPer2 <- list()
#create a list for the output statistical periods
#annual statistics, for each historic and simulated year
for (y in seq(minObsYear,yEnd)){lStatPer[[ac(y)]]<-c(y,y)}
for (y in seq(maxObsYear+1,yEnd)){lStatPer2[[ac(y)]]<-c(y,y)}

#Short (first 5 after constraints), Medium (next 5) and Long Term (next 20)
yConstraints <- 3
lStatPer[['CU']] <- lStatPer2[['CU']] <- c(yStart, yStart+yConstraints-1)
lStatPer[['ST']] <- c(yStart+yConstraints,yStart+yConstraints+(per1-1))
lStatPer2[['ST']] <- c(yStart+yConstraints+1,yStart+yConstraints+(per1-1))
lStatPer[['MT']] <- lStatPer2[['MT']] <- c(yStart+yConstraints+per1,yStart+yConstraints+(per1+per2-1))
lStatPer[['LT']] <- lStatPer2[['LT']] <- c(yStart+yConstraints+per1+per2,yStart+nyr-1)

set.seed(1)

sim <- eqsim_run(fit = SRR, 
                 bio.years = OM$BioYrs, 
                 bio.const = OM$BioConst,
                 sel.years = OM$SelYrs, 
                 sel.const = OM$SelConst,
                 Fscan = fGetValsScan(MP$F_target,OM$refPts), 
                 Fcv = MP$Obs$cvF, 
                 Fphi = MP$Obs$phiF,
                 SSBcv = MP$Obs$cvSSB, 
                 SSBphi = MP$Obs$phiSSB,
                 Blim = OM$refPts$Blim,
                 Nrun = nyr, 
                 calc.RPs = FALSE,
                 dfExplConstraints = dfExplConstraints, 
                 Btrigger = fGetValsScan(MP$B_trigger,OM$refPts),
                 HCRName = paste0("fHCR_",MP$HCRName))

SimRuns <- sim$simStks

#save ouptut

#create a folder for the output and save simRuns data
dir.create(path = file.path(Res.dir,runName), showWarnings = TRUE, recursive = TRUE)
save(SimRuns,file = file.path(Res.dir,runName,paste0(runName,"_SimRuns.RData")))

#Write the output to dropbox dir (necessary to save entire image?)
#save.image(file = file.path(dropbox.dir,paste0(runName,"_Workspace.Rdata")))
save.image(file = file.path(Res.dir,runName,paste0(runName,"_Workspace.Rdata")))

#Percentiles to report, number of worm lines for plots
percentiles = c(0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975)
numWorm <- 40

#Create list objects to store stocks, stats
Stocks <- AllStats <- list()
#create a template by extending the time frame out to the final year of the simulation
stockTemplate <- window(SRR$stk, end=yEnd)

#set the maturity, natural mortality and f/m proportions
mat(stockTemplate)[,simYears] <- mat(stockTemplate)[,ac(yStart-1)]
m(stockTemplate)[,simYears] <- m(stockTemplate)[,ac(yStart-1)]
m.spwn(stockTemplate)[,simYears] <- m.spwn(stockTemplate)[,ac(yStart-1)]
harvest.spwn(stockTemplate)[,simYears] <- harvest.spwn(stockTemplate)[,ac(yStart-1)]
#quick look
#plot(stockTemplate)

#extend object to store neessary number of iterations
stockTemplate <- FLCore::propagate(stockTemplate,niters)

#populate each stock object
#year dimension is trimmed to the actual simulation period (may be longer depending on HCR implemented)
for (ii in names(SimRuns)) {
  
  cat("Calculating statistics for run with f =",ii,"\n")
  
  Stocks[[ii]] <- stockTemplate
  stock.n(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$N[,1:(yEnd-yStart+1),]
  harvest(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$F[,1:(yEnd-yStart+1),]
  catch.n(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$C[,1:(yEnd-yStart+1),]
  catch.wt(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$catW[,1:(yEnd-yStart+1),]
  #landings.n(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$L[,1:(yEnd-yStart+1),]
  landings.n(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$C[,1:(yEnd-yStart+1),]
  stock.wt(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$stkW[,1:(yEnd-yStart+1),]
  mat(Stocks[[ii]])[,simYears,,,,] <- SimRuns[[ii]]$Mat[,1:(yEnd-yStart+1),]
  discards.wt(Stocks[[ii]]) <- catch.wt(Stocks[[ii]])
  landings.wt(Stocks[[ii]]) <- catch.wt(Stocks[[ii]])
  discards.n(Stocks[[ii]])[,simYears,,,,] <- 0
  catch(Stocks[[ii]])[,simYears] <- apply(catch.n(Stocks[[ii]])[,simYears]*catch.wt(Stocks[[ii]])[,simYears],2:6,sum)
  landings(Stocks[[ii]])[,simYears] <- apply(landings.n(Stocks[[ii]])[,simYears]*landings.wt(Stocks[[ii]])[,simYears],2:6,sum)
  discards(Stocks[[ii]])[,simYears] <- apply(discards.n(Stocks[[ii]])[,simYears]*discards.wt(Stocks[[ii]])[,simYears],2:6,sum)
  
  #statistics
  Stats <- list()
  
  #store the operating model and harvest rules
  Stats[["OM"]] <- OM
  Stats[["MP"]] <- MP
  Stats[["simYears"]] <- as.integer(simYears)
  Stats[["obsYears"]] <- as.integer(obsYears)
  
  #SSB
  SSB.true <- ssb(Stocks[[ii]])
  Stats[["SSB"]][["val"]] <- fStatPercs(SSB.true, lStatPer=lStatPer)
  Stats[["SSB"]][["worm"]] <- FLCore::iter(SSB.true,1:numWorm)
  
  #time to recovery after falling below Blim
  firstBelow <- recTimeBlim <- recTimeBpa <- rep(NA,dim(SSB.true)[6])
  names(firstBelow) <- names(recTimeBlim) <- names(recTimeBpa) <- as.character(seq(1,dim(SSB.true)[6]))
  
  #drop the unused dimensions
  SSB1 <- drop(as.array(SSB.true))
  
  #iterations during which SSB fell below Blim
  anyBelow <- apply(SSB1,MARGIN=2,FUN = function(x){any(x<OM$refPts$Blim)})
  #exclude those that never fell below Blim
  SSB1[,!anyBelow]<-NA
  recTimeBlim[names(anyBelow[!anyBelow])] <- NA
  recTimeBpa[names(anyBelow[!anyBelow])] <- NA
  
  #year in which SSB fell below Blim
  firstBelow[names(anyBelow[anyBelow])] <- apply(SSB1[,anyBelow], MARGIN=2, FUN = function(x){min(which(x<OM$refPts$Blim))})
  #remove SSB records prior to the year when it fell below Blim
  for (i in 1:length(firstBelow)){if (!is.na(firstBelow[i])) {SSB1[1:firstBelow[i],i]<-NA}}
  
  #Blim############
  #did any iterations recover above Blim?
  anyBackAboveBlim <- apply(SSB1[,names(anyBelow[anyBelow])], MARGIN=2, FUN = function(x){any(x>OM$refPts$Blim, na.rm=TRUE)})
  #set the recovery time for those that did not recover to 0
  recTimeBlim[names(anyBackAboveBlim[!anyBackAboveBlim])] <- 0
  
  #iterations which fell below and subsequently recovered
  recoveredBlim <- intersect(names(anyBelow[anyBelow]),names(anyBackAboveBlim[anyBackAboveBlim]))
  #get recovery time
  recTimeBlim[recoveredBlim] <- apply(SSB1[,recoveredBlim], MARGIN=2, FUN = function(x){min(which(x>OM$refPts$Blim))})
  
  #recovery time
  rBlim <- recTimeBlim-firstBelow
  
  Stats[["recBlim"]][["val"]] <- quantile(rBlim[rBlim>0],probs=percentiles,na.rm=T)
  Stats[["recBlim"]][["nobelow"]] <- sum(!anyBelow)
  Stats[["recBlim"]][["norecover"]] <- sum(!anyBackAboveBlim)
  
  #Bpa############
  #did any iterations recover above Bpa?
  anyBackAboveBpa <- apply(SSB1[,names(anyBelow[anyBelow])], MARGIN=2, FUN = function(x){any(x>OM$refPts$Bpa, na.rm=TRUE)})
  #set the recovery time for those that did not recover to 0
  recTimeBpa[names(anyBackAboveBpa[!anyBackAboveBpa])] <- 0
  
  #iterations which fell below and subsequently recovered
  recoveredBpa <- intersect(names(anyBelow[anyBelow]),names(anyBackAboveBpa[anyBackAboveBpa]))
  #get recovery time
  recTimeBpa[recoveredBpa] <- apply(SSB1[,recoveredBpa], MARGIN=2, FUN = function(x){min(which(x>OM$refPts$Bpa))})
  
  #recovery time  
  rBpa <- recTimeBpa-firstBelow
  
  Stats[["recBpa"]][["val"]] <- quantile(rBpa[rBpa>0],probs=percentiles,na.rm=T)
  Stats[["recBpa"]][["nobelow"]] <- sum(!anyBelow)
  Stats[["recBpa"]][["norecover"]] <- sum(!anyBackAboveBpa)
  
  #SSB error
  tSSB <- FLQuant(SimRuns[[ii]]$SSBratio[1:(yEnd-yStart+1),],
                  dim = c(1,yEnd-yStart+1,1,1,1,niters),
                  dimnames = list(age="all",year=ac(seq(yStart,yEnd)),unit="unique",season="all",
                                  area="unique",iter=ac(seq(1,niters))))
  
  Stats[["SSBratio"]][["val"]] <- fStatPercs(tSSB, lStatPer = lStatPer)
  Stats[["SSBratio"]][["worm"]] <- FLCore::iter(tSSB,1:numWorm)
  
  SSB.dev <- SimRuns[[ii]][["SSBdev"]]
  Stats[["SSB.dev"]] <- SSB.dev
  
  #FBar - realised F
  FBar <- fbar(Stocks[[ii]])
  Stats[["FBar"]][["val"]] <- fStatPercs(FBar, lStatPer=lStatPer)
  Stats[["FBar"]][["worm"]] <- FLCore::iter(FBar,1:numWorm)
  
  #FBar error
  tFBar <- FLQuant(SimRuns[[ii]]$Fratio[1:(yEnd-yStart+1),],
                   dim = c(1,yEnd-yStart+1,1,1,1,niters),
                   dimnames = list(age="all",year=ac(seq(yStart,yEnd)),unit="unique",season="all",
                                   area="unique",iter=ac(seq(1,niters))))
  
  #browser()
  Stats[["Fratio"]][["val"]] <- fStatPercs(tFBar, lStatPer = lStatPer)
  Stats[["Fratio"]][["worm"]] <- FLCore::iter(tFBar,1:numWorm)
  
  Fdev <- SimRuns[[ii]][["Fdev"]]
  Stats[["Fdev"]] <- Fdev
  
  #yield
  Catch <- catch(Stocks[[ii]])
  Stats[["Catch"]][["val"]] <- fStatPercs(Catch, lStatPer=lStatPer)
  Stats[["Catch"]][["worm"]] <- FLCore::iter(Catch,1:numWorm)
  
  #TAC
  tTAC <- FLQuant(SimRuns[[ii]]$TAC[1:(yEnd-yStart+1),],
                  dim = c(1,yEnd-yStart+1,1,1,1,niters),
                  dimnames = list(age="all",year=ac(seq(yStart,yEnd)),unit="unique",season="all",
                                  area="unique",iter=ac(seq(1,niters))))
  
  Stats[["TAC"]][["val"]] <- fStatPercs(tTAC, lStatPer = lStatPer)
  Stats[["TAC"]][["worm"]] <- FLCore::iter(tTAC,1:numWorm)
  
  #IAV - multiplied by 100 this stat is the absolute percentage change (no indication of up or down)
  IAV <- abs(1-Catch[,as.character(seq(yStart+1,yEnd))]/Catch[,as.character(seq(yStart,yEnd-1))])
  #replace Inf with NA (NA results from comparing with zero catch)
  IAV <- ifelse(is.finite(IAV),IAV,NA)
  
  Stats[["IAV"]][["val"]] <- fStatPercs(IAV, lStatPer = lStatPer2)
  Stats[["IAV"]][["worm"]] <- FLCore::iter(IAV,1:numWorm)
  
  #IAV increases/decreases
  IAVup <- IAVdown <- IAVupdown <- Catch[,as.character(seq(yStart+1,yEnd))]/Catch[,as.character(seq(yStart,yEnd-1))] - 1
  IAVup[IAVup<0] <- NA
  IAVdown[IAVdown>0] <- NA
  
  Stats[["IAVupdown"]][["worm"]] <- FLCore::iter(IAVupdown,1:numWorm)
  Stats[["IAVup"]][["val"]] <- fStatPercs(IAVup, lStatPer = lStatPer2)
  Stats[["IAVdown"]][["val"]] <- fStatPercs(IAVdown, lStatPer = lStatPer2)
  
  #Recruitment
  Rec <- rec(Stocks[[ii]])
  Stats[["Rec"]][["val"]] <- fStatPercs(Rec, lStatPer=lStatPer)
  Stats[["Rec"]][["worm"]] <- FLCore::iter(Rec,1:numWorm)
  
  #probability that SSB is below RP (should this be true or observed SSB?)
  Stats[["pBlim"]][["val"]] <- fStatRisk(SSB = SSB.true, RP = OM$refPts$Blim, lStatPer = lStatPer)
  Stats[["pBpa"]][["val"]] <- fStatRisk(SSB = SSB.true, RP = OM$refPts$Bpa, lStatPer = lStatPer)
  Stats[["pExt"]][["val"]] <- fStatExtinct(SSB = SSB.true, depletion=0.01, firstYear = maxObsYear)
  
  #data frame format
  Stats[["df"]]  <- fsummary_df(
    run=runName, ftgt = ii, simRuns = SimRuns,
    Res.dir = Res.dir, Plot.dir = Plot.dir,
    lStatPer = lStatPer, simYears = simYears, xlab = MP$xlab,
    Blim = OM$refPts$Blim, 
    Fbarrange=c(range(WHOM.WGWIDE2019)[["minfbar"]], range(WHOM.WGWIDE2019)[["maxfbar"]])) 
  
  Stats[["dfy"]] <- fsummary_byyear_df(
    run=runName, ftgt=ii, simRuns=simRuns,
    Res.dir = Res.dir, Plot.dir = Plot.dir,
    lStatPer = lStatPer, simYears = simYears, xlab = MP$xlab,
    Blim = OM$refPts$Blim, 
    Fbarrange=c(range(WHOM.WGWIDE2019)[["minfbar"]], range(WHOM.WGWIDE2019)[["maxfbar"]])) 
  
  Stats[["settings"]] <- fGetSettings(
    lStatPer=lStatPer, SimRuns=SimRuns, 
    FLStockfile=FLStockfile, FLStockSimfile=FLStockSimfile,
    OM=OM, MP=MP, niters=niters, nyr=nyr)
  
  AllStats[[ac(ii)]] <- Stats
  
}

## Save data
lStats <- list(stats = AllStats, runName = runName, lStatPer = lStatPer, OM = OM, MP = MP)
save(lStats,file = file.path(Res.dir,runName,paste0(runName,"_eqSim_Stats.Rdata")))

# Save settings
settings <- fGetSettings(lStats, SimRuns, FLStockfile = FLStockfile,FLStockSimfile = FLStockSimfile,OM=OM, MP=MP, niters=niters, nyr=nyr)
save(settings,file = file.path(Res.dir,runName,paste0(runName,"_eqSim_Settings.Rdata")))

#generate the stock/stat trajectories
fPlotTraj(sim = lStats, plot.dir = file.path(Res.dir,runName), lStatPer = lStatPer)
suppressWarnings(fPlotSummary(sim = lStats, plot.dir = Res.dir, lStatPer = lStatPer, FtoPlot=fGetValsScan(MP$F_target,OM$refPts)))
fTabulateStats(sim = lStats, setting= settings, plot.dir = Res.dir)
