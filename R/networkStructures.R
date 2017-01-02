
networkStructures <- function(n, upperBound = 10, lowerBound = -10, testStructures = NULL){
  
  if(is.null(testStructures)){
  l <- gtools::permutations(n = n, r = n*(n+1), v = c(-1,1), repeats.allowed = TRUE)
  
  
  lb <- l
  lb[lb %in% 1] <- 0
  lb[lb %in% -1] <- lowerBound
  
  ub <- l
  ub[ub %in% 1] <- upperBound
  ub[ub %in% -1] <- 0
  bounds <- lb
  bounds[bounds %in% c(0)] <- "+"
  bounds[bounds %in% lowerBound] <- "-"
  return(list("ub"=ub, "lb"=lb, "bounds"=bounds))
  }
  if(!is.null(testStructures)){
    
    a <- strsplit(testStructures, split = NULL)
    lb <- matrix(nrow = length(a), ncol = length(a[[1]]))
    for(i in 1:length(a)){
      lb[i,1:length(a[[i]])] <- a[[i]]
    }
    
    
    
    for(j in 1:length(lb)){
      for(i in 1:length(lb[[j]])){
        if(lb[[j]][i] =="-"){lb[[j]][i] <- lowerBound}else{lb[[j]][i] <- 0}
      }
    }
    class(lb) <- "numeric"
    
    
    
    a <- strsplit(testStructures, split = NULL)
    ub <- matrix(nrow = length(a), ncol = length(a[[1]]))
    for(i in 1:length(a)){
      ub[i,1:length(a[[i]])] <- a[[i]]
    }
    
    for(j in 1:length(ub)){
      for(i in 1:length(ub[[j]])){
        if(ub[[j]][i] =="+"){ub[[j]][i] <- upperBound}else{ub[[j]][i] <- 0}
      }
    }
    class(ub) <- "numeric"
    bounds <- as.matrix(testStructures)
    return(list("ub"=ub, "lb"=lb, "bounds"=bounds))
  }
}





