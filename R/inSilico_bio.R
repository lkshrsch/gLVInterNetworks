inSilico_bio <-
function(species, number_of_non_diagonal_coefficients, events, mode = 1, times=1:100){
  
  'requires: random_interaction_matrix_bio, solveLV'
  
  
  "mode:0 = linear ODE, 1 = normal, 2 = mixed feeding, 3 = events"
  n = number_of_non_diagonal_coefficients
  
  if(n>(species^2-species)){
    print(paste0("Error. For ", species," species, the number of interactions cant be greater than ",species^2-species))
    return()
  }
  
  
  res <- matrix(0,nrow=2)
  
  while(  class(res)[1]!="deSolve"){
    # j <- j+1
    #print(j)
    
    interactions <- random_interaction_matrix_bio(species, number_of_non_diagonal_coefficients)
    #m <- sample(x = c(0,runif(species+5,0.01,2)), size = species, prob = c(1-(1/(species+5)),rep(0.4/(species+5),species+5)), replace = T) 
    #Parms <- cbind( m,interactions)
    
    #Parms <- cbind(qV,interactions) 
    
    # If no substrate present, outflow must be weighted with linear growth function (they grow now independent of a substrate abundance)
    
    
    #growth <- rnorm(n = species,mean = 0.5,sd = 1)
    
    #x0 = c(runif(species,0.1,0.5))
    
    growth <- rep(0,nrow(interactions))
    var_means <- runif(n = species, 0.1,0.8)
    
    
    for(i in 1:nrow(interactions)){
      growth[i] <- -1*(interactions[i,]%*%var_means)
    }
    
    
    Parms <- cbind(growth, interactions)
    
    #colnames(Parms) = NULL
    #par(mfrow=c(1,1))
    options(warn = 2)
    
    #try to solve res, and continue to next simulation if warning emerges -integration fail- (warn=2, counted as an error)
    
    x0 <- var_means + rnorm(n = species,mean = var_means,sd = 0.01)
    
    # NEEDS TO BE ADAPTED TO NEW SUBSTRATE DYNAMICS
    if(mode==3){
      print("Events")
      names(x0) <- as.character(1:length(x0))
      res <- tryCatch(solveLV_event(events = events,times = times,  Parms, States = x0 , c=2*c, lambda= lambda, cycle=cycle),error=function(e) return("error") )
      s <- S(t = times, c=2*c, lambda = lambda, cycle )
    }
    
    if(mode==1){
      #print("Normal")
      res <- tryCatch(solveLV_bio(times = times,  Parms, States = x0),warning=function(w) return("error"),error=function(e) return("error") )
      
    }
    
    if(mode==0){
      print("linear ODE")
      res <- tryCatch(solveLV_bio_linear(Parms, States = x0, times = times))
    }
    
  }  
  
  options(warn = 1)
  
  return(list(res,Parms))
  
}
