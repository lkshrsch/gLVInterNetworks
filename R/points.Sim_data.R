points.Sim_data <- 
function(x, index=NULL , ...){

  if(!is.null(index)){
    points(x = x$obs[,1], y= x$obs[,index+1], col=index)
  }else{
  for(i in 1:ncol(x$obs[,-1])){
    points(x = x$obs[,1], y= x$obs[,i+1], col=i)
  }
  }
}