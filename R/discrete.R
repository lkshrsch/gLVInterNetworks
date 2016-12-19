discrete <-
function(observations, n){
  'make realistic observations by discretization of simulated data.'
  result <- observations[seq(1,nrow(observations),by = n),]
  return(result)
}
