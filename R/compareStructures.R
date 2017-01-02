
compareStructures <- function(data, structures, verbose=FALSE, iterations = 1){
  
  if(class(data)=="matrix"){
    obs <- data
    data <- list("obs"=obs, "noise"="?")
  }
  
  # error check that data is in right format. Convert to right format
  
  result <- list() 
  for(i in 1:nrow(structures$lb)){
    
    best <- list()
    ssrs <- c()
    for(j in 1:iterations){
      nlr <- list()
      while(class(nlr)!="Inference_run"){
        try(nlr <- gLVnonlinearRegression(data = data ,method = "Marq",parms0 = runif(n = ncol(data$obs[,-1])*(ncol(data$obs[,-1])+1), min = structures$lb[i,], max = structures$ub[i,]),  lowerbound = structures$lb[i,], upperbound = structures$ub[i,]),silent = T)
      }
      best[[j]] <- nlr
      ssrs[j] <- nlr$SSR
    }
    
    ssrs <- round(ssrs,3)
    differentSolutions <- length(unique(ssrs))
    numberOptimalSolution <- sum(ssrs == min(ssrs))
    
    nlr <- best[[min(which(ssrs == min(ssrs)))]]
    
    nlr$iterations <- iterations
    nlr$differentSolutions <- differentSolutions
    nlr$numberOptimalSolutions <- numberOptimalSolution
    
    if(nlr$message != "Solution of model failed. Overshot"){
      result[[i]] <- nlr
    }
    if(verbose){print(i)}
  }
  #--------------------------------------------
  if(!is.null(data$Parms)){
    # if in silico data, convert to structure
    structure <- c(sign(data$Parms))
    structure[structure %in% 1] <- "+"
    structure[structure %in% -1] <- "-"
    structure <- paste0(structure, collapse = "")
  }else{
    # else:
    structure = "Unknown"
  }
  
  ssr <- matrix(nrow = (length(result)+1), ncol = 4)
  
  for(i in 1:(nrow(ssr)-1)){
    ssr[i,1] <- i
    try(ssr[i,2] <- round(result[[i]]$SSR,3), silent = T)
    ssr[i,3] <- paste0(structures$bounds[i,],collapse = "")
    ssr[i,4] <- round(sd(result[[i]]$Fit$residuals),3)
  }
  ssr[nrow(ssr),1:4] <- c("Original", 0 , structure, data$noise)
  
  result_final <- as.data.frame(ssr[order(as.numeric(ssr[,2])),])
  
  colnames(result_final) <- c("Index", "SSR","Network","SD")
  
  return(list("networks"=result_final,"runs"=result))
  
}