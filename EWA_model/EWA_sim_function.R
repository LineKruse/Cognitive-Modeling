EWA_function <- function(nagents, ntrials, ntokens, pi, parameters){
  
  #Variables to update in simulation 
  c <- array(0, c(nagents, ntrials)) #Amount contributed 
  N <- array(0, c(nagents, ntrials)) #Experience (count)
  A <- array(0, c(nagents, ntrials, ntokens)) #Attractions 
  expA <- array(0, c(nagents, ntrials, ntokens)) #Numerator for softmax 
  P <- array(0, c(nagents, ntrials, ntokens)) #Probability of choosing each amount to contribute 
  
  #-------- Parameters ----------#
  #Weighting of forgone versus received payoffs
  delta <- parameters$delta 
  
  #Discounting of old trials - speed at which agents commit to a strategy priors (0,1)
  #Higher number means older trials have great influence and strategy fixation is slower 
  rho <- parameters$rho
  
  #Memory of old attractions - volatility assumption about environment priors (0,1)
  #Higher number means old attractions are weighted higher 
  phi <- parameters$phi
  
  #Consistency of choice with attractions - inverse heat - explore/exploit
  lambda <- parameters$lambda 
  
  #--------- Trial 1 values --------#
  N[,1] <- 1
  A[,1,] <- 0
  c[,1] <- 20 
  
  #---------- EWA model  --------- #
  for (t in 2:ntrials){
    
    for(n in 1:nagents){
      
      #Experience updating 
      N[n,t] <- (rho[n]*N[n,t-1])+1 
      
      for (j in 1:ntokens){
        
        #EWA learnign rule 
        A[n,t,j] <- (
          
          #Prior attractions
          (phi[n]*N[n,t-1]*A[n,t-1,j]) +
            
            #Indication of whether jth token was chosen 
            (delta[n]+((1-delta[n])*(c[n,t-1]==j))) *
            
            #Calculate payoff for each possible contribution 
            ((((j+sum(c[-n,t-1]))*pi)/nagents)-j)
          
        )/
          N[n,t] #Experience weighting 
        
        expA[n,t,j] <- exp(lambda[n]*A[n,t,j])
        
      }
      
      #Softmax function 
      for (j in 1:ntokens){
        
        P[n,t,j] <- expA[n,t,j]/sum(expA[n,t,])
        
      }
      
      c[n,t] <- rcat(1,P[n,t,])
      
    }
    
  }
  
  result <- list(c=c, N=N)
  return(result)
  
}