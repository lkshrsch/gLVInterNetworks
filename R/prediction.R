prediction <-
function(result, data){
  #of test data
  x0 <- data$testData[1,-1]
  
  pred <- solveLV_bio(result$Parms, data$testData[1:nrow(data$testData),1], x0)
  
  Coef_of_determination <- R_squared(pred, data$testData)
  
  result$R_squared_test_Data <- Coef_of_determination
  
  r <- c()
  for(i in 1:10){
    
    
    x0 <- runif(n = data$species, 0, 1)
    
    new <- solveLV_bio(data$Parms, times = 0:50, States = x0)
    
    #new <- addNoise_res(new, data$noise)
    
    pred2 <- solveLV_bio(result$Parms, new[,1], x0)
    
    r[i] <- R_squared(pred2, new)
  }
  
  result$R_squared_new_Data <- mean(r)
  
  return(result)
  
}
