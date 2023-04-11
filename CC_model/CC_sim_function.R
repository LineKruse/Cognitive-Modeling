CC_function <- function(nagents, ntrials, vals, parameters){
  
  #Free parameters - what we can do inference on 
  Gb1 <- parameters$Gb1
  omega1 <- parameters$omega1
  lambda <- parameters$lambda
  gamma <- parameters$lambda
  p0 <- parameters$p0
  pbeta <- parameters$pbeta
  
  #Simulation arrays- to be filled 
  Ga <- array(0, c(ntrials)) #Observed contribution of others 
  Gb <- array(0, c(nagents, ntrials)) #Beliefs of others' contributions 
  p <- array(0, c(nagents, ntrials)) #preference for amount contributing (conditional)
  omega <- array(0, c(nagents, ntrials)) #weightingof beliefs about others contributions relative to own preferred contribution
  c <- array(0, c(nagents, ntrials)) #actual contribution 
  
  #Agents preferences - assumed to be a linear function of possible values 
  pvals <- array(0, c(nagents, length(vals)))
  #Vector of preferred conditional contributions 
  for(n in 1:nagents){
    pvals[n,] <- p0[n] + (pbeta[n]*vals)
  }
  
  #Set values for trial 1
  omega[,1] <- omega1
  Gb[,1] <- Gb1
  c[,1] <- Gb1
  Ga[1] <- mean(Gb1)
  
  #Run forward model simulation 
  for (t in 2:ntrials){
    
    for (n in 1:nagents){
      
      #Update beliefs about what others contribute 
      Gb[n,t] <- (gamma[n]*(Gb[n,t-1]))+((1-gamma[n])*(Ga[t-1]))
      
      #Determine what people predict or prefer to contribute given the group contribution 
      p[n,t] <- pvals[n,round(Gb[n,t])]
      
      #Update relative weighting of belifes about others contributions, using decay function
      omega[n,t] <- omega[n,t-1]*(1-lambda[n])
      
      #Contribution is a wegihted average of predicted (preferred) contribution and belief
      c[n,t] <- ceiling((omega[n,t])*Gb[n,t] + ((1-omega[n,t])*p[n,t]))
      
    }
    
    #Recode average contribution as observed belief about how much each agent contributed 
    Ga[t] <- sum(c[,t])/nagents
    
  }
  
  result <- list(c=c, Ga=Ga)
  return(result)
}
