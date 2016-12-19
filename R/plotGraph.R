plotGraph <- 
function(x, vsize = 0.1, main = NULL, verbose = FALSE, keepNames = FALSE, ...){
  
  if(class(x) == "real_data"){return("Can't draw an interaction network of real data..! Provide inference results")}
  
  draw_interaction_network(generate_interaction_network(x$Parms, observations = x$obs, vsize = vsize, verbose = verbose, keepNames = keepNames ), main = main)
  
}