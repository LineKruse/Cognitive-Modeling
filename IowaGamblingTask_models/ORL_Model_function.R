ORL_model <- function(payoff, ntrials, Arew, Apun, betaF, betaP, K, theta){
  
  x <- array(0, c(ntrials)) #choice for each trial 
  r <- array(0, c(ntrials)) #reward for each trial 
  EV <- array(0, c(ntrials,4)) #Expected valence for each trial and for each deck 
  EV_update <- array(0, c(ntrials,4)) #Update to EV for each trial and each deck 
  EF <- array(0, c(ntrials,4)) #Expected frequency for each trial and each deck 
  EF_chosen <- array(0, c(ntrials,4)) #update to EF for chosen decks, for each trial and each deck 
  EF_unchosen <- array(0, c(ntrials,4)) #update to EF for unchosen decks, for each trial and each deck 
  PS <- array(0, c(ntrials,4)) #Perseverance on each trial for each deck 
  V <- array(0, c(ntrials,4)) #Valence of each deck on each trial 
  exp_p <- array(0, c(ntrials,4)) #For calculating the softmax function 
  p <- array(0, c(ntrials,4)) #Probability of choosing each deck on each trial 
  
  #Set vlaues for trial 1 
  x[1] <- rcat(1,c(.25,.25,.25,.25)) #initializing the choice, in trial 1 we expect the agent to pick at random 
  r[1] <- payoff[1,x[1]] #initializing the choice, in trial 1 we expect the agent to pick a random card
  
  signX <- array(0, c(ntrials))
  
  K_new <- (3^K)-1
  
  for (t in 2:ntrials){
    
    signX[t] <-ifelse(r[t-1]==0, 0, ifelse(r[t-1]<0, -1, 1))
    
    for (k in 1:4){
      
      #Update Expected Value, EV
      EV_update[t,k] <- ifelse(r[t-1]>=0, 
                               EV[t-1,k]+Arew*(r[t-1] - EV[t-1,k]),
                               EV[t-1,k]+Apun*(r[t-1] - EV[t-1,k]))
      
      EV[t,k] <- ifelse(x[t-1]==k, EV_update[t,k], EV[t-1,k])
      
      #Update Expected Frequency (different for chosen and unchosen decks)
      EF_chosen[t,k] <- ifelse(r[t-1]>=0,
                          EF[t-1,k]+Arew*(signX[t]-EF[t-1,k]),
                          EF[t-1,k]+Apun*(signX[t]-EF[t-1,k]))
      
      EF_unchosen[t,k] <- ifelse(r[t-1]>=0,
                            EF[t-1,k]+Apun*((-signX[t]/3)-EF[t-1,k]),
                            EF[t-1,k]+Arew*((-signX[t]/3)-EF[t-1,k]))
      
      EF[t,k] <- ifelse(x[t-1]==k, EF_chosen[t,k], EF_unchosen[t,k])
      
      #Update Perseverancce, PS (different for chosen and unchosen decks)
      PS[t,k] <- ifelse(x[t-1]==k,
                        1/(1+K_new),          #chosen decks
                        PS[t-1,k]/(1+K_new))  #unchosen decks 
      
      #Update Valence, V, for each deck 
      V[t,k] <- EV[t,k] + EF[t,k]*betaF + PS[t,k]*betaP
      
      #Apply softmax function - turn Valence into probabiltiy of choice 
      exp_p[t,k] <- exp(theta*EV[t,k])#put it into softmax - turn it into a choice prob - Luce's choice rule
        
    }
    
    for (k in 1:4){
      p[t,k] <- exp_p[t,k]/sum(exp_p[t,]) #apply softmax function 
    }
    
    x[t] <- rcat(1,p[t,]) #choice is sample from categorical distribution of the four probability distributions (updated)
    r[t] <- payoff[t,x[t]]
    
  }
  
  output <- list(x=x, EV=EV, EF=EF, PS=PS, V=V, r=r)
  return(output)
  
}
