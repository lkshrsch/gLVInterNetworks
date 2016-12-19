parameter_matrix <-
function(pars, numberSpecies ){
  'set parameters as matrix format as required by solveLV function'
  par_matrix = matrix(data = pars, nrow = numberSpecies)
  return(par_matrix)
}
