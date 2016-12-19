
generate_data <- 
function(species, number_of_non_diagonal_coefficients, timepoints, noise, testData){
  
  'requires inSilico_bio, discrete, addNoise_res'
  
  raw_data <- inSilico_bio(species,number_of_non_diagonal_coefficients)
  Parms <- raw_data[[2]]
  
  res <- raw_data[[1]]
  
  # 2) Data selection:
  
  threshold = 3
  
  #first time point to be stable
  
  if(any(which(round(diff(abs(rowMeans(res[,-1])),2),threshold )==0))){
    
    end <- min(which(round(diff(abs(rowMeans(res[,-1])),2),threshold )==0))
  }else{ end <- nrow(res) 
  }
  
  # 2.1 data simulation in time interval
  
  res <- solveLV_bio(Parms, seq(0,end,by=0.01), res[1,-1])
  
  # 2.2 Discretization
  
  discretization <- nrow(res)/timepoints 
  
  res <- discrete(res, discretization)
  
  
  # Separation in test data and observations
  
  t <- round(nrow(res)*(testData*0.01))
  
  test_data <- res[(nrow(res)-t):nrow(res),]
  
  obs <- res[1:(nrow(res)-t),] 
  
  '----  can change to add other kinds of noise?? ----'
  # Add gaussian noise
  
  obs <- addNoise_res(obs, noise)
  
  '-------------------------------------------------'
  
  # add Noise (important for probabilistic modelling and meaningful likelihood)
  #matplot(res[,-1],type="p",pch=20)
  
  # END DATA EDITING
  
  # Data characteristics:
  
  timepoints <- nrow(obs)
  dimensions <- ncol(obs[,-1])
  
  k <- sum(any(Parms==0))/length(Parms)
  
  
  
  data <- list("species"=species,"timepoints"=timepoints, "Parms"=Parms,"noise"=noise, "sparsity"=k, "obs"=obs,"testData"=test_data)
  
  class(data) <- "Sim_data"
  
  return(data)
}
