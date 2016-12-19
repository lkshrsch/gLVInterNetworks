obs2data_bio <-
function(res){
  obs = t(res[,-1])  # get rid of time
  X = rbind(1,obs)
  X = X[,1:ncol(X)-1]  # remove last time measurement
  #F = Slopes
  
  Y <- slope(res[,-1])
  
  Y=t(Y)
  
  Y[is.nan(Y)]<-0
  Y[is.infinite(Y)]<-0
  
  return(list(Y,X))
  
}
