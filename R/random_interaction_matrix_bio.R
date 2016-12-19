random_interaction_matrix_bio <-
function(species,number_of_non_diagonal_coefficients, times=1:100, plot=FALSE){
  'Generate random parameters for simulation studies on multispecies gLV'
  'For lotka volterra function without substrate, so diagonal entries less negative'
  
  n = number_of_non_diagonal_coefficients
  
  if(n>(species^2-species)){
    print(paste0("Error. For ", species," species, the number of interactions cant be greater than ",species^2-species))
    return()
  }
  #diagonal negative -> logistic growth
  
  interaction <- diag(sample(x = runif(n = species, min = -2,max = -1)),nrow = species)
  
  
  
  # random interaction coefficients outside diagonal (replace=FALSE)
  
  if(abs(n)>0){
    
    coordinates <-   which(interaction == 0, arr.ind = T) 
    
    rows = sample(x = nrow(coordinates), size = n, replace = FALSE)  
    
    
    
    for ( i in 1:length(rows)){
      
      #interaction[coordinates[rows[i],1],coordinates[rows[i],2]] <- runif(1,min = -2, max = 2)
      interaction[coordinates[rows[i],1],coordinates[rows[i],2]] <- rnorm(1,0,4)
      
      
    }
  }
  
  
  
  #pars <- cbind(linear,interaction)
  
  colnames(interaction)=NULL
  
  
  
  return((interaction))
}
