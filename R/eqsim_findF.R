fFindF <- function(N,CW,SW,Mat,M,Sel,tgt,type=1,ages,iters){
  
  #type specifies the target type with 1 for "catch" and 2 for "SSB"
  #return the appropriate fishing mortality vector that yields the provided catch
  
  #N,CW,SW,Mat,M,Se - 2d arrays, first dimension age, second dimension iteration
  
  #dimension and initialise return array
  #ret <- array(data=NA,dim=c(ages,iters))
  rtn <- rep(NA,length=iters)
  
  #browser()
  
  for (i in 1:iters){  
    
    if (length(dim(N))==3) {
    rtn[i] <- optimize(f = fDelta,
                       interval = c(-100,100),
                       ages = ages,
                       N = N[,1,i],
                       M = M[,i],
                       Sel = Sel[,i],
                       CW = CW[,1,i],
                       SW = SW[,1,i],
                       Mat = Mat[,i],
                       tgt = tgt[i],
                       type = type,
                       debug = FALSE,
                       tol = 0.000000001)$minimum
    }
    
    if (length(dim(N))==2) {
      rtn[i] <- optimize(f = fDelta,
                         interval = c(-100,100),
                         ages = ages,
                         N = N[,i],
                         M = M[,i],
                         Sel = Sel[,i],
                         CW = CW[,i],
                         SW = SW[,i],
                         Mat = Mat[,i],
                         tgt = tgt[i],
                         type = type,
                         debug = FALSE,
                         tol = 0.000000001)$minimum
    }
    
    #if (i==1)(cat(i,rtn[i],"\n"))
  }
  
  return(rtn)
  
}

fDelta <- function(Fmult,ages,N,M,Sel,CW,SW,Mat,tgt,type,debug=FALSE){
  
  #Fmult - fishing mortality multiplier
  #N - population numbers
  #M - natural mortality
  #Sel - selection pattern
  #CW,SW - catch,stock weights
  #Mat - maturity
  #tgt - the target catch
  
  #fishing mortality
  F <- Fmult*Sel
  #total mortality
  Z <- F + M
  
  #return the difference between the target that corresponding to the vectors provided
  if (type==1) {
    
    #catch
    rtn <- abs(tgt - sum(N*(1-exp(-Z))*CW*(F/Z),na.rm=TRUE))
    
  } else {
    
    #SSB
    N.next <- N
    N.next[1]  <- 0
    N.next[2:ages] <- N[1:(ages-1)] * exp(-F[1:(ages-1)] - M[1:(ages-1)])
    N.next[ages] <- N.next[ages] + N[ages] * exp(-F[ages] - M[ages])
    
    rtn <- abs(tgt - sum(Mat*N.next*SW))
    
  }
  
  if(debug){
    cat("fDelta\n")
    cat("tgt=",tgt,"\n")
    cat("N=",N,"\n")
    cat("M=",M,"\n")
    cat("F=",F,"\n")
    cat("Sel=",Sel,"\n")
    cat("CW=",CW,"\n")
    cat("Total=",sum(N*(1-exp(-Z))*CW*(F/Z)),rtn,"\n")
  }
  
  rtn
  
}