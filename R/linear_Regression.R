
linear_Regression <- 
function(data, regularization = FALSE, alpha = 0){
  
  'input: data as list of lists'
  
  #-----------------------------------------
  # Check that input is correct:
  if(class(data)%in%c("matrix","table")){tmp <- data; data <- list(); data$obs <- tmp }
  
  if(class(data$obs)!="matrix"){print("data in wrong format?");return()}
  if(alpha < 0 || alpha > 1){print("alpha in wrong interval. Must be in [0,1] ");return()}
  
  #----------------------------------------
  
  try(k <- data$sparsity, silent = T)   # Only possible for simulated data! 
  try(noise <- data$noise, silent = T)   # Only possible for simulated data! 
  try(Parms <- data$Parms, silent = T)   # Only possible for simulated data!
  species <- ncol(data$obs[,-1])
  
  Result_LR <- list()
  obs <- obs2data_bio(data$obs)
  Y = obs[[1]]
  X = obs[[2]]
  condition_Number_XtX <- kappa(t(scale(t(X)[,-1],scale = F))%*%scale(t(X)[,-1],scale = F))
  condition_Number_X <- kappa(t(X[,-1]))
  data$kappa_XTX <- condition_Number_XtX
  data$kappa_X <- condition_Number_X
  
  #------------- Linear Regression ------------------------  
  #here LR improvement from linmod possible ! (QR decomposition to avoid matrix inverse, although here i implement stats::lm , but maybe I can just write the linmod version)

  if(!regularization){
    
    # Ordinary linear regression:
    parmsLR <- matrix(0,nrow = length(data$obs[1,-1]), ncol = length(data$obs[1,-1])+1)
    SE_LR <- parmsLR
    
    for(i in 1:nrow(Y)){
      Fit <- stats::lm(Y[i,]~t(X)[,-1])
      sF <- summary(Fit)
      parmsLR[i,] <- sF$coefficients[,"Estimate"]
      SE_LR[i,] <- sF$coefficients[,"Std. Error"]  
    }
    
    timescale <- data$obs[2,1]
    parmsLR <- parmsLR/timescale
    SE_LR <- SE_LR/timescale
    
    try(reshat <- solveLV_bio(parmsLR,times = seq(0,data$obs[nrow(data$obs),1],by = 0.01),States = data$obs[1,-1]), silent = TRUE)
    
    
    
    if(reshat[nrow(reshat),1]!=data$obs[nrow(data$obs),1]){   # If LR fails, regularize
      
      regularization <- TRUE
      
      
      
    }else{
      
      cost <- FME::modCost(reshat, data$obs)
      SSR_LR <- cost$model#/length(cost$residuals[,"res"])
      
      # check if errors are really normally distributed with mean zero:
      test <- stats::t.test(cost$residuals[,"res"])
      mean_zero_of_errors_LR <- test$conf.int[1] <= 0 & 0  <= test$conf.int[2]
      
      
      
    }
    
  }
  
  #------------- Regularization Alternatives -------
  
  # Elastic net
  
  #  result_regularized = tryCatch(regularized_linear_regression(X,Y, alpha = 0.6,cvfolds = cvfolds),error=function(e) return(c("error","error")))
  #  parmsEL = result_regularized[[1]]
  #  reg_mseEL = result_regularized[[2]]
  #  options(warn = 1)
  
  # LASSO
  
  #  result_regularized = tryCatch(regularized_linear_regression(X,Y, alpha = 1,cvfolds = cvfolds),error=function(e) return(c("error","error")))
  #  parmsLASSO = result_regularized[[1]]
  #  reg_mseLASSO = result_regularized[[2]]
  
  # Ridge resgression
  if(regularization){
    
    cvfolds <- max(3,round(length(data$obs[,1])/10))
    
    try(result_regularized <- regularized_linear_regression(X,Y, alpha = alpha ,cvfolds = cvfolds),silent = F)
    
    parmsLR <- result_regularized[[1]]
    timescale <- data$obs[2,1]
    parmsLR <- parmsLR/timescale
    
    
    SE_LR <- matrix(0, species, species+1)
    
    reshat <- solveLV_bio(parmsLR,times = seq(0,data$obs[nrow(data$obs),1],by = 0.01),States = data$obs[1,-1])
    
    
    
    if(reshat[nrow(reshat),1]!=data$obs[nrow(data$obs),1]){
      
      SSR_LR <- Inf
      SE_LR <- NULL
      mean_zero_of_errors_LR <- NULL
      
      
    }else{
      
      cost <- FME::modCost(reshat, data$obs)
      
      SSR_LR <- cost$model
      
      for(i in 1:species){
        
        SE_LR[i,] <- sqrt(diag(cost$var[i,"SSR"]/length(parmsLR[i,])*MASS::ginv(rbind(1,t((data$obs[,-1])))%*%cbind(1,(data$obs[,-1])))))
        
      }
      
      SE_LR <- SE_LR/timescale
      # check if errors are really normally distributed with mean zero:
      test <- stats::t.test(cost$residuals[,"res"])
      mean_zero_of_errors_LR <- test$conf.int[1] <= 0 & 0  <= test$conf.int[2]
      
      
      
    }
    
    
  }
  
  df <- nrow(data$obs[,-1])-ncol(data$obs[,-1])  # in inference function
  
  
  if(regularization){
    if(alpha == 0){regression <- "Ridge"}
    if(alpha == 1){regression <- "LASSO"} 
    if(alpha < 1 && alpha > 0){regression <- paste0("Elastic Net. Alpha = ", alpha )}
  } else {regression <- "Vanilla Linear Regression"}
  
  Result_LR <- list( "obs" = reshat, "Parms"=parmsLR,"SE"=SE_LR, "df" = df , "residuals_t.test"=mean_zero_of_errors_LR, "SSR"= SSR_LR, "Regression"=regression)
  
  #-------------- ASSESSMENT -----------------------
  if(!is.null(data$testData)){
  try(Result_LR <- prediction(Result_LR, data),silent = T)
  
  try(Result_LR <- assessment(Result_LR, Parms), silent = T)
  }
  
  inference <- Result_LR
  
  #run <- list("data"=data,"inference"=inference)
  
  run <- inference
  
  class(run) <- "Inference_run"
  
  return(run)
  
}

