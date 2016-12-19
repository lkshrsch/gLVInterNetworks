
sensitivityAnalysis <- 
function(Parms){
  
  sF <- FME::sensFun(func = solveLV_Sens_bio, parms = Parms)
  
  coll <- FME::collin(sF)
  
  return(list("sens" = sF, "coll" = coll))
  
}


