interaction_compare <-
function(a,b){
  
  
  if(a>0 && b>0){
    return(TRUE)
  }
  else
    if(a==0 && b==0){
      return(TRUE)
    }
  else
    if(a<0 && b<0){
      return(TRUE)
    }
  else {return (FALSE)}
}
