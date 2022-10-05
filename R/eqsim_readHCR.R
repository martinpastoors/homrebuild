#read HCR function

fReadHCR <- function(HCRName){
  out <- tryCatch(
    {
      match.fun(HCRName)
    },
    error = function(cond){
      stop("HCR function ",HCRName," does not seem to exist. Check you have defined function")
    },
    warning = function(cond){
      message("HCR function ",HCRName," caused a warning")
      message("Original warning is")
      message(cond)
      return(NULL)
    }
  )
}

#no HCR
fHCR_None <- function(lIP){
  #empty HCR (takes no action)
  y <- lIP[["Yr"]]
  ssb <- lIP[["SSB"]]
  btrig <- lIP[["Btrigger"]]
  fnext <- lIP[["Fnext"]]
  M <- lIP[["M"]]
  N <- lIP[["N"]]
  sel <- lIP[["sel"]]
  SW <- lIP[["SW"]]
  Mat <- lIP[["Mat"]]
  numAges <- dim(N)[1]
  numIters <- dim(N)[2]
  
  rtn <- rep(fnext,numIters)
  rtn
}

#ICES AR
fHCR_ICES <- function(lIP){
  
  #y,CW,SW,Mat,M,N,Sel,ssb,F,Btrigger,Fmin=0.01,ages,iters
  
  #implementation of ICES type advice rule
  #if SSB>Btrigger then F=Ftarget, else a linear reduction to origin 
  #no special action below Blim
  
  y <- lIP[["Yr"]]
  ssb <- lIP[["SSB"]]
  btrig <- lIP[["Btrigger"]]
  fnext <- lIP[["Fnext"]]
  M <- lIP[["M"]]
  N <- lIP[["N"]]
  sel <- lIP[["sel"]]
  SW <- lIP[["SW"]]
  Mat <- lIP[["Mat"]]
  numAges <- dim(N)[1]
  numIters <- dim(N)[2]
  
  #browser()
  
  rtn <- rep(fnext,numIters)
  
  #identify those iterations with observed SSB below the HCR biomass trigger
  cond <- (ssb[y,] < btrig)
  
  if (sum(cond)>0) {
    cat("Reducing on",sum(cond),"iterations by average of",100*(1.0 - mean(ssb[y,cond]/btrig)),"% \n")
    rtn[cond] <- rtn[cond]*ssb[y,cond]/btrig
  }
  
  rtn
  
}

#Double breakpoint, lower F is 1/5 of upper

fHCR_DoubleBP <- function(lIP){
  
  #y,CW,SW,Mat,M,N,Sel,ssb,F,Btrigger,Fmin=0.01,ages,iters
  
  #implementation of BWtype rule
  #if SSB>Btrigger then F=Ftarget, else a linear reduction to Ftgt/5 at lower breakpoint (Blim) 
  
  y <- lIP[["Yr"]]
  ssb <- lIP[["SSB"]]
  btrig <- lIP[["Btrigger"]]
  fnext <- lIP[["Fnext"]]
  blower <- lIP[["Blim"]]
  flower <- fnext/5
  M <- lIP[["M"]]
  N <- lIP[["N"]]
  sel <- lIP[["sel"]]
  SW <- lIP[["SW"]]
  Mat <- lIP[["Mat"]]
  numAges <- dim(N)[1]
  numIters <- dim(N)[2]
  
  rtn <- rep(fnext,numIters)
  
  #identify those iterations with observed SSB below the HCR biomass trigger
  cond <- (ssb[y,] < btrig & ssb[y,] >= blower)
  
  if (sum(cond)>0) {
    cat("Reducing on",sum(cond),"iterations between Blim and Btrigger \n")
    rtn[cond] <- flower + ((ssb[y,cond]-blower)/(btrig-blower))*(fnext-flower)
  }
  
  #those below blower
  cond <- (ssb[y,] < blower)
  
  if (sum(cond)>0) {
    cat("Reducing on",sum(cond),"iterations to 0.2*Ftgt \n")
    rtn[cond] <- flower
  }
  
  
  rtn
  
}


#suspend fishing
fHCR_Suspend <- function(y,CW,SW,Mat,M,N,Sel,ssb,F,Btrigger,Fmin=0.01,ages,iters){
  
  cat("fHCR_Suspend\n")
  rtn <- rep(0,iters)
  rtn
  
}

#SimpSim HCR function definitions
#fHCR_NFD <- function(y,CW,SW,Mat,M,N,Sel,ssb,F,Btrigger,Fmin=0.01,ages,iters){
fHCR_NFD <- function(lIP){
  
  #cat("fHCR_NFD\n")
  
  #an implementation of Dankert's "No Further Decline" (NFD) proposed rule
  #y - the management year i.e. the year for which F advice is required
  #ssb - SSB array (year,iteration,era)
  #Btrigger - trigger biomass
  
  y <- lIP[["Yr"]]
  ssb <- lIP[["SSB"]]
  btrig <- lIP[["Btrigger"]]
  fnext <- lIP[["Fnext"]]
  M <- lIP[["M"]]
  N <- lIP[["N"]]
  sel <- lIP[["sel"]]
  SW <- lIP[["SW"]]
  Mat <- lIP[["Mat"]]
  numAges <- dim(N)[1]
  numIters <- dim(N)[2]

  #minimum F, number of years for recent average
  Fmin <- 0.01
  numYrs <- 2
  
  #dummy return vector
  rtn <- rep(fnext,numIters)
  
  #subset SSB array to keep numYrs most recent years. If insufficient, just keep what there is
  s0 <- ssb[seq(from = max((y-numYrs+1),1), to = y),,drop=FALSE]
  #calculate average
  s0 <- apply(s0,MARGIN=2,FUN=function(x){mean(x,na.rm=TRUE)})
  
  #if (y>numYrs){s0 <- ssb[c(y-1,y),,drop=FALSE]} else {s0 <- lIP[["SSB"]][lIP[["Yr"]],,drop=FALSE]}
    
  #apply the target fishing mortality (fnext)
  #year end numbers
  N2 <- N * exp(-fnext*sel - M)
  #following year (recruitment = 0 - will not contribute to SSB anyway)
  N3 <- N2
  N3[1,] <- 0
  N3[2:(numAges-1),] <- N2[1:(numAges-2),]
  N3[numAges,] <- N2[numAges-1,] + N2[numAges,]
  
  #SSB
  s1 <- apply(N3*Mat*SW,MARGIN=2,sum)
  
  #if S0>Btrigger or S1>s0 then we do not update the target F
  #if s1>=s0 then we do not update the target F
  #if s1 is lower than s0 then and s0 is below the trigger then we reduce the target F such that SSB s1=s0, subject to a minimum Fmin
  
  cond <- (s1<s0) & (s0<btrig) & (fnext>Fmin)
  
  if (sum(cond)>0) {
    
    cat(" falling SSB for",sum(cond),"iterations\n")
    
    #find F that leads to s1=s0, with a minimum of Fmin
    rtn[cond] <- fFindF(N = N[, cond, drop = FALSE], CW = SW[, cond, drop = FALSE], 
                        SW = SW[, cond, drop = FALSE], Mat = Mat[, cond, drop = FALSE],
                        M = M[, cond, drop = FALSE], Sel = sel[, cond, drop = FALSE], 
                        tgt = s0[cond, drop = FALSE], type = 2, ages = numAges, iters = sum(cond))
    
    rtn[cond] <- pmax(rtn[cond],Fmin)
    
    cat("Mean updated Ftgt = ",mean(rtn[cond]), "with ",sum(rtn[cond]==Fmin), "minimum F set\n")
  }

    
  # if (sum(cond)>0){
  #   
  #   #temporary vars to select specific year (this dimension will be dropped for the call to fFindF)
  #   tN <- N[,y,];tCW <- CW[,y,];tSW <- SW[,y,];tMat <- Mat[,y,];tM <- M[,y,];tSel <- Sel[,y,]
  #   
  #   rtn[cond] <- fFindF(tN[,cond,drop=FALSE], tCW[,cond,drop=FALSE], tSW[,cond,drop=FALSE],
  #                       tMat[,cond,drop=FALSE], tM[,cond,drop=FALSE], tSel[,cond,drop=FALSE],
  #                       s0[cond], type=2, ages, iters=sum(cond))
  # }
  # 
  # cat("year",y,"reducing on",sum(cond),"iterations\n")
  # 
  # rtn <- pmax(rtn,Fmin)
  
  rtn
}
