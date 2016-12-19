
modfit_pars_estimate_MLE_sequential <- 
function(verbose=FALSE,parameter_vector,method, res, ptol, ftol, maxiter, background_Parms, lowerbound, upperbound){
  
  'MLE parameter estimation using FME modfit, written so that it can accept parameter subsets to estimate. General function for MLE'
  'internal function'
  
  #require(deSolve)
  #require(FME)
  #require(minpack.lm)  
  #background_Parms <- c(background_Parms)
  #names(background_Parms) <- as.character(1:length(background_Parms))
  
  ModelCost_linear <- function(P, parset = names(x)) {
    
    Parms <- background_Parms
    
    Parms[as.numeric(names(P))] <- P
    
    P <- parameter_matrix(Parms,length(res[1,-1]))
    
    if(verbose){print(P)}
    
    res_hat <- solveLV_bio(Parms = P,States = res[1,-1],times =  seq(0,res[nrow(res),1],by = 0.01))
    
    colnames(res_hat) <- colnames(res)
    
    return(FME::modCost(res_hat, res))  
  }
  
  # HERE I AM SETTING BOUNDS ON THE PARAMETERS (-20, 20), MAKE THIS MORE FLEXIBLE
  Fit <- FME::modFit(p = parameter_vector, method = method  , f = ModelCost_linear ,control = minpack.lm::nls.lm.control(ptol = ptol, ftol = ftol, maxiter = maxiter) , lower = lowerbound ,upper = upperbound)  
  
  return(Fit)
}

