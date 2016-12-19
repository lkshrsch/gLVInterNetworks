
summary.Inference_run <- 
function(object, ...){
  
  tval <- c(object$Parms/object$SE)
  TAB <- cbind(Estimate = c(object$Parms), StdErr = c(object$SE), t.value = tval, p.value = 
2*pt(-abs(tval), 
df = object$df))
  class(TAB) <- 'summary.Inference_run'
  return(TAB)
}
