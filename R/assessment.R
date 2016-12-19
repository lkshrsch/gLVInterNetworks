assessment <-
function(result, Parms){
  
  "still need to check how many zeros are in Parms and correct for qualitative..."
  
  quantitative <- correctly_intervalized_estimates(c(result$Parms) ,c(result$SE),c(Parms)) # significant hits (t test if its different from original parameters)
  quantitative <- sum(quantitative)/length(quantitative)
  
  qualitative <- compare_matrix(c(result$Parms), c(Parms)) # qualitative comparison. Network edges.
  qualitative <- sum(qualitative)/length(qualitative)
  
  
  #qualitative2 <- correctly_intervalized_estimates(c(result$Parms[!Parms %in% 0]) ,c(result$SE[!Parms %in% 0]),rep(0,length(c(result$Parms[!Parms %in% 0]))) ) # qualitative comparison. Network edges.
  #qualitative2 <- 1-sum(qualitative2)/length(qualitative2)
  
  is_not_significant <- correctly_intervalized_estimates(c(result$Parms) ,c(result$SE),rep(0,length(Parms)))  # qualitative comparison. Network edges.
  
  is_zero <- c(Parms) == 0
  
  qualitative2 <- is_not_significant == is_zero
  
  qualitative2 <- sum(qualitative2)/length(qualitative2)
  
  result$quantitative = quantitative 
  result$qualitative1 = qualitative
  result$qualitative2 = qualitative2
  
  
  return(result)
  
}
