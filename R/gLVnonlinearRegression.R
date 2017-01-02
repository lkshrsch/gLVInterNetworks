

gLVnonlinearRegression <- 
function(data, parms0 = NULL, ftol = 1e-8 , ptol = 1e-8, maxiter = 100, lowerbound = rep(-20,ncol(data$obs)*(ncol(data$obs)-1)), upperbound = rep(20,ncol(data$obs)*(ncol(data$obs)-1)), method = "Marq"){ 
  
  # Check that input is correct: 
  if(class(data)%in%c("matrix","table")){tmp <- data; data <- list(); data$obs <- tmp }
  
  if(class(data$obs)!="matrix"){print("data in wrong format?");return()}

  try(Parms <- data$Parms, silent = T)
  species <- ncol(data$obs[,-1])
  
  if(!is.null(parms0)){
    d <- dim(parameter_matrix(parms0,species))
    if(d[1]!=species || d[2]!= (species+1) ){
      cat("parms0 has incorrect dimensions for the input observations.", "\n", "input species: ",species, "\n", "input parms0 dimensions: ", d )
      return()
      }
    }
  
  try(k <- data$sparsity, silent = T)
  try(noise <- data$noise, silent = T)

  Result_nonlin <- list()
  obs <- obs2data_bio(data$obs)
  Y = obs[[1]]
  X = obs[[2]]
  condition_Number_XtX <- kappa(t(scale(t(X)[,-1],scale = F))%*%scale(t(X)[,-1],scale = F))
  condition_Number_X <- kappa(t(X[,-1]))
  data$kappa_XTX <- condition_Number_XtX
  data$kappa_X <- condition_Number_X

  ###################################################################################
  
  #              Nonlinear Regression: gradient descent 
  
  ###################################################################################

  # first try to get better simultaneous fit
  
  npar <- species*(species+1)
  
  background <- rep(0, npar)
  names(background) <- as.character(1:npar)
  
  
  if(is.null(parms0)){
    parameters_to_estimate <- background
    names(parameters_to_estimate) <- as.character(1:npar)
  }else{parameters_to_estimate <- parms0
  names(parameters_to_estimate) <- as.character(1:npar)}
  ################################################################
  #--------------------------------------------------------

  options(warn = -1)
  try(Fit <- modfit_pars_estimate_MLE_sequential(background_Parms = background, method = method , parameter_vector = parameters_to_estimate, res = data$obs, verbose = F, ftol= ftol , ptol = ptol, maxiter = maxiter,  lowerbound = lowerbound, upperbound = upperbound),silent = F)
  
  
  # failed solution, overshot
  
  if(length(Fit$residuals) != length(data$obs[,-1])){
    
    Result_nonlin$Parms <- Fit$par
    Result_nonlin$SSR <- Inf
    Result_nonlin$SE <- NULL
    Result_nonlin$residuals_t.test <- NULL
    Result_nonlin$message <- "Solution of model failed. Overshot"
    reshat <- solveLV_bio(Fit$par, times = seq(0,data$obs[nrow(data$obs),1],by = 0.01),States = data$obs[1,-1])
    Result_nonlin$Fit <- Fit
    
    Result_nonlin$obs <- reshat
    
    Result_nonlin$parms0 <- parms0
    
    class(Result_nonlin) <- "Inference_run"
    
    return(Result_nonlin)
    
  }
  
  if(!all(names(Fit$residuals) %in% as.character(1:ncol(data$obs[,-1])))){
    
    Result_nonlin$Parms <- Fit$par
    Result_nonlin$SSR <- Inf
    Result_nonlin$SE <- NULL
    Result_nonlin$residuals_t.test <- NULL
    Result_nonlin$message <- "Solution of model failed. Overshot"
    reshat <- solveLV_bio(Fit$par, times = seq(0,data$obs[nrow(data$obs),1],by = 0.01),States = data$obs[1,-1])
    Result_nonlin$Fit <- Fit
    Result_nonlin$parms0 <- parms0
    Result_nonlin$obs <- reshat
    
    class(Result_nonlin) <- "Inference_run"
    
    return(Result_nonlin)
    
    
  }
  
  
  options(warn = 2)  # catch warning from summary function. If error, then singularity encountered or some problem which impedes the computation of the inverse hessian
  mes <- tryCatch(sF <- summary(Fit),error=function(e) return("error"))
  
  options(warn = -1) # return to normal error handling
  
  
  #  failed
  if(class(mes)[1]!="summary.modFit"){
    
    #parms_nonlin <- Fit$par
    SSR_nonlin <- Inf
    SE_nonlin <- NULL
    mean_zero_of_errors_nonlin <- NULL
    
    Result_nonlin$Parms <- Fit$par
    Result_nonlin$SSR <- Fit$ssr
    Result_nonlin$SE <- NULL
    Result_nonlin$residual_SD <- sd(Fit$residuals)

    Result_nonlin$residuals_t.test <- NULL
    Result_nonlin$message <- "Failure on computing summary statistics. Solution converged to optimal point estimates"
    reshat <- solveLV_bio(Fit$par, times = seq(0,data$obs[nrow(data$obs),1],by = 0.01),States = data$obs[1,-1])
    Result_nonlin$Fit <- Fit
    Result_nonlin$parms0 <- parms0
    Result_nonlin$obs <- reshat
    
    class(Result_nonlin) <- "Inference_run"
    
    return(Result_nonlin)
    
  } else{  #simultaneous fit worked: 
    
    
    
    nonlin_start <- parms0
    
    SSR_nonlin <- Fit$ssr
    
    parms_nonlin <- Fit$par
    
    SE_nonlin <- sqrt(diag(sF$cov.scaled))
    
    reshat <- solveLV_bio(parms_nonlin, times = seq(0,data$obs[nrow(data$obs),1],by = 0.01),States = data$obs[1,-1])
    
    test <- stats::t.test(Fit$residuals)
    
    mean_zero_of_errors_nonlin <- test$conf.int[1] <= 0 & 0  <= test$conf.int[2]
    
    Result_nonlin$Parms <- parameter_matrix(parms_nonlin, species)
    Result_nonlin$SSR <- SSR_nonlin
    Result_nonlin$residual_SD <- sd(Fit$residuals)
    Result_nonlin$SE <- parameter_matrix(SE_nonlin, species)
    Result_nonlin$residuals_t.test <- mean_zero_of_errors_nonlin
    Result_nonlin$message <- Fit$message
    Result_nonlin$obs <- reshat
    Result_nonlin$parms0 <- parms0
    Result_nonlin$Fit <- Fit
    
    Result_nonlin$df <- ncol(data$obs[,-1])*nrow(data$obs[,-1]) - ncol(data$obs[,-1])*(ncol(data$obs[,-1])+1) # in inference function. Its weird like this because starting point is given, thus one row of df less...
    
    if(!is.null(data$testData)){
    try(Result_nonlin <- prediction(Result_nonlin, data),silent = T)
    try(Result_nonlin <- assessment(Result_nonlin, Parms), silent = T)
    }
    
    inference <- Result_nonlin#, "subsets"=Result_subsets)
    
    run <- inference
    class(run) <- "Inference_run"
    
    
    return(run)
    
}}

