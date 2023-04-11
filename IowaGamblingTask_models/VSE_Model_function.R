VSE_model <- function(R, L, ntrials, theta, delta, a, phi, C){
  
  #Setup arrays to keep track of variables 
  x <- array(0, c(ntrials)) #Choice on each trial 
  r <- array(0, c(ntrials)) #reward on each trial 
  l <- array(0, c(ntrials)) #loss on each trial 
  v <- array(0, c(ntrials,4)) #value for each deck on each trial 
  Exploit <- array(0, c(ntrials,4)) #outcome of value learning function for each deck on each trial 
  Explore <- array(0, c(ntrials,4)) #outcome of the sequential exploration function for each deck on each trial 
  exp_p <- array(0, c(ntrials,4)) #for calculating the softmax function 
  p <- array(0, c(ntrials,4)) #probability of choosing each deck on each trial 
  
  #Set values for first trial 
  x[1] <- sample(1:4,1)
  
  Exploit[1,1] <- 0
  Exploit[1,2] <- 0
  Exploit[1,3] <- 0
  Exploit[1,4] <- 0
  
  Explore[1,1] <- 0
  Explore[1,2] <- 0
  Explore[1,3] <- 0
  Explore[1,4] <- 0
  
  for (t in 2:ntrials){
    
    v[t-1] <- R[t-1,x[t-1]]^theta - L[t-1, x[t-1]]^theta
    
    for (k in 1:4){
      
      #Value learning function 
      Exploit[t,k] <- ifelse(x[t-1]==k,
                             Exploit[t-1,k]*delta+v[t-1],
                             Exploit[t-1,k]*delta)
      
      #Sequential exploration function 
      Explore[t,k] <- ifelse(x[t-1]==k,
                             0,
                             Explore[t-1,k]+a*(phi-Explore[t-1,k]))
      
      #Apply softmax (using both Explore and Exploit values)
      exp_p[t,k] <- exp(Explore[t,k]+Exploit[t,k]*C)
      
    }
    
    for (k in 1:4){
      p[t,k] <- exp_p[t,k]/sum(exp_p[t,]) #apply softmax function 
    }
    
    x[t] <- rcat(1,p[t,]) #choice is sample from categorical distribution of the four probability distributions (updated)
    r[t] <- R[t,x[t]]
    l[t] <- L[t,x[t]]
    
  }
  
  output <- list(x=x, r=r, l=l, v=v, Exploit=Exploit, Explore=Explore)
  return(output)
  
}
