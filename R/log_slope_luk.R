log_slope_luk <-
function(a,b){
  
  return(tryCatch(log(abs(b))-log(abs(a)),error = function(e) {print(paste0("Tried slope with: ",a ," ",b));return("log slope error")} ))
  
}
