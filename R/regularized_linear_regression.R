
regularized_linear_regression <- 
function(X,Y, alpha, cvfolds){
  
  
  #require(glmnet)
  
  sol = matrix(0, nrow = nrow(Y), ncol = nrow(X))
  
  # X <- X[-1,]  # Get rid of intercept , it is automatically placed by glmnet
  
  
  mse = 0
  
  
  
  for(i in 1:nrow(Y)){
    
    
    cv <- glmnet::cv.glmnet(t(X)[,-1],Y[i,], alpha=alpha ,nfolds = cvfolds,standardize=T)
    
    small.lambda.index <- which(cv$lambda == cv$lambda.1se)
    small.lambda.betas <- cv$glmnet.fit$beta[,small.lambda.index]
    small.lambda.betas <- c(cv$glmnet.fit$a0[small.lambda.index],small.lambda.betas)
    sol[i,] = small.lambda.betas
    mse[i] = cv$cvm[small.lambda.index]
    #print(cv$lambda.1se)
  }
  
  mse = mean(mse)
  
  
  
  
  return(list(sol,mse))
  
}
