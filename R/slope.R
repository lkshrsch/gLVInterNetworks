slope <-
function(abundances){
  'Get log slopes of measured time points. For discrete LV linear algebraic system'
  'Input: observations without time points == Abundances'
  #means = matrix(0,nrow = nrow(observations)/2,ncol = ncol(observations))
  slopes = matrix(0,nrow = nrow(abundances)-1,ncol = ncol(abundances))
  for (column in 1:ncol(abundances)){   
    for (row in 1:(nrow(abundances)-1)){
      
      
      try(slopes[row,column] <- log_slope_luk(abundances[row,column],abundances[row+1,column]))
      
      
      #means[(row/2),column] <- mean_luk(observations[row-1,column],observations[row,column],n = 2)
      #slopes[(row/2),column] <- slope_luk(observations[row-1,column],observations[row,column],n = 4)
    }
  }
  return(slopes)
}
