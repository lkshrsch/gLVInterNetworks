compare_matrix <-
function(a,b){
  
  av = c(a)
  bv = c(b)
  
  for(i in 1:length(av)){
    av[i] = interaction_compare(av[i],bv[i])
  }
  return(av)
}
