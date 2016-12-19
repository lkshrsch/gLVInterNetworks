
solveLV_Sens_bio <- 
function(Parms, times, states){
  
  'Just like solveLV_new but adapted for sensitivity analysis (initial states inside) PROBLEM initial states are then global....'
  
  states = data$obs[1,-1]
  times = data$obs[,1]  ### PROBLEM , again, using variable not passed through the arguments!! 
  
  if(class(Parms)!="matrix"){Parms <- parameter_matrix(Parms, length(states))}
  
  
  derivsgLV_new<-function( Time,States,Parms){   #time needs to be an argument for the later fitting functions
    
    #derivates =States*( Parms %*% append(1, append(S(Time, cycle = cycle, c = c, continuous = continuous, lambda = lambda),States))) 
    
    derivates = States*( Parms %*% append(1,States)) 
    
    #logistic:
    #derivates =   ( Parms %*% append(1, append(S(Time, cycle = cycle, c = c, continuous = continuous, lambda = lambda),States)))* (1 - ( abs(States)/K)  )
    
    return(list(derivates))   
    
  }
  
  return(deSolve::ode(y=states, times = times, func = derivsgLV_new, parms = Parms, hmax=0.01, hmin=1e-7))
  
}