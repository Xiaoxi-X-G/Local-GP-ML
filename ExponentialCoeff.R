ExponentialCoeff<-function(lg, Weights){
  # InputData = numeric numbers
  # Weights = numeric number in [0 1)
  
  # Coeff = exponentially increasing weights
  
  
  if (lg == 1){
    return(1)

  }else{
    if (Weights>=1 | Weights <0 ){
      stop("Weights has to be in [0 1)")
    }
    
    tmp <- rep(0, lg) 
    tmp[1]<- Weights
    mm<-0
    for (j in (2:lg)){
      mm<- (1-sum(tmp))*sum(tmp)
      tmp[j] <-mm
    }
    
    Coeff<-rev(tmp/sum(tmp))
  }
  
  return(Coeff)
}