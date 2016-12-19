plot.Inference_run <-
function(x, ...){
    
    matplot(x = x$obs[,1],y = x$obs[,-1], xlab = "Time", ylab="Model solution" , ... )  
}