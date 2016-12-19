R_squared <-
function(newData, prediction){
  if(dim(newData)[1] != dim(prediction)[1]){return(0)}
  else{
  SStot = sum((newData[,-1] - colMeans(prediction[,-1]))^2)
  SSres = sum((newData - prediction)^2)
  
  return(Coef_of_determination = max(0,(1 - (SSres/SStot))))
  }
}
