plotGraph <- 
function(x, vsize = 25, main = NULL, relativeVSize=FALSE, relativeEdgeSize=FALSE, edgeSize=1, verbose = FALSE, keepNames = FALSE, ...){
  
  if(class(x) == "real_data"){return("Can't draw an interaction network of real data..! Provide inference results")}
  
  draw_interaction_network(generate_interaction_network(x$Parms, relativeVSize , relativeEdgeSize, edgeSize ,observations = x$obs, vsize = vsize, verbose = verbose, keepNames = keepNames ), main = main)
  
}