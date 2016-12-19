solveLV_bio <-
function(Parms, times , States ){
    
    #require(deSolve)
    
    if(class(Parms)!="matrix"){Parms <- parameter_matrix(Parms, length(States))}
    
    
    derivsgLV<-function( Time,States,Parms){   #time needs to be an argument for the later fitting functions
      
      States[States <= 0] = 0
      
      #derivates =States*( Parms %*% append(1, append(S(Time, cycle = cycle, c = c, continuous = continuous, lambda = lambda),States))) 
      
      derivates =States*( Parms %*% append(1,States)) 
      
      #logistic:
      #derivates =   ( Parms %*% append(1, append(S(Time, cycle = cycle, c = c, continuous = continuous, lambda = lambda),States)))* (1 - ( abs(States)/K)  )
      
      return(list(derivates))   
      
    }
    try(return(deSolve::ode(y=States, times = times, func = derivsgLV, parms = Parms, hmax=0.01, hmin=1e-7)))
    
  }