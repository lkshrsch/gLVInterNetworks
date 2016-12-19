addNoise_res <-
function(res,sd){
  
  noise <- matrix(rnorm(0,sd,n = length(res[,-1]) ),nrow = nrow(res), ncol = ncol(res[,-1]))
  
  
  
  res[,-1] <- res[,-1] + noise
  return(res)
}
