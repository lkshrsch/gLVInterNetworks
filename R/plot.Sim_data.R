plot.Sim_data <-
function(x, legend = FALSE, ...){
  
  if(legend){
    matplot(x = x$obs[,1] ,y = x$obs[,-1], xlab = "Time", ylab = "Observations", type="l",...)
  legend("topright", legend = colnames(x$obs[,-1]),lty = 1, col=c(1:length(x$obs[,-1])))
  }
  else{matplot(x = x$obs[,1] ,y = x$obs[,-1], xlab = "Time", ylab = "Observations", ...)}  
}
