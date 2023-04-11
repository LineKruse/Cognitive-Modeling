PVL <- function(payoff, ntrials, w, A, a, theta){
  
  #Arrays to populate for simulation 
  x <- array(0, c(ntrials)) #choice
  X <- array(0, c(ntrials)) #reward
  u <- array(0, c(ntrials,4)) #utility (four because we have a utility for each deck)
  Ev <- array(0, c(ntrials,4)) #Expected value 
  Ev_update <- array(0, c(ntrials,4)) #Our update to Ev along the simulation 
  exp_p <- array(0, c(ntrials,4)) #exponential part of the probability value
  p <- array(0, c(ntrials,4)) #probability value (one for each deck)
  
  x[1] <- rcat(1, c(.25,.25,.25,.25)) #Initalize first choice, equal prob for all four decks 
  
  ###Build model 
  for (t in 2:ntrials){
    
    for (d in 1:4){
      
      #PVL delta model (prospect theory)
      u[t,d] <- ifelse(X[t-1]<0, -w*abs(X[t-1])^A, abs(X[t-1])^A) #see meaning of the parameters in the Rmd script 
      
      Ev_update[t,d] <- Ev[t-1,d]+ (a*(u[t,d] - Ev[t-1,d]))  #Expected valence, applied to every deck as it was chosen, Ev on last trial + delta learning rule
      #THe delta rule make sure that we do not just update our EV based on the prediction error (distance from correct to 
      #actual expectation) - but that we use some of our old expectation + some of our prediction error (small updates to smooth
      #out the learning curve. 
      
      Ev[t,d] <- ifelse(x[t-1]==d, Ev_update[t,d], Ev[t-1,d]) #If the deck we chose is the loop we are also in 
      
      exp_p[t,d] <- exp(theta*Ev[t,d])#put it into softmax - turn it into a choice prob - Luce's choice rule 
      
    }
    
    for (d in 1:4){
      p[t,d] <- exp_p[t,d]/sum(exp_p[t,])
    }
    
    x[t] <- rcat(1,p[t,]) #choice is sample from categorical distribution of the four probability distributions (updated)
    X[t] <- payoff[t,x[t]]
    
  }
  
  result <- list(x=x, X=X, Ev=Ev)
  return(result)
  
}
