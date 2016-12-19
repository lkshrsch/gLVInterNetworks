
print.summary.Inference_run <- 
function(x, ...){
  
  printCoefmat(x , P.values = TRUE, has.Pvalue = TRUE)
  
}
