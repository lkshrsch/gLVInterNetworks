
generate_interaction_network <- 
function(parms_matrix, observations, relativeVSize = FALSE, relativeEdgeSize = FALSE, edgeSize ,  substrate=FALSE, vsize=0.1, verbose=FALSE, keepNames=FALSE ){
  
  #require(igraph)
  
  rownames(parms_matrix)<-as.character(1:nrow(parms_matrix))
  
  n <- ncol(observations[,-1])
  
  interaction_pars <- parms_matrix[,-1]
  linear_pars <- parms_matrix[,1]  
  
  #transpose interaction matrix! Arrow points in the direction a specie actually influence another!
  interaction_pars <- round(interaction_pars,3)  # round parameters for sparse representation 
  
  network <- igraph::graph.adjacency(adjmatrix = t(interaction_pars),mode = "directed",weighted = TRUE, add.colnames = TRUE)
  
  
  #edge attributes:
  
  #make list of parameters to match list of edges (removing all 0 coefficients = not existing edges)
  interaction_pars <- interaction_pars[ ! interaction_pars %in% 0] 
  
  for ( i in 1:length(igraph::E(network))){  
    
    'if(c(interaction_pars)[i]!=0){
    E(network)[i]$label = c(interaction_pars)[i]
  }'
    
    if (c(interaction_pars)[i]<0){   
      igraph::E(network)[i]$lty=2
    }
    
    if ( c(interaction_pars)[i]>0){
      igraph::E(network)[i]$lty=1
    }
    
    
    
    if(  abs(c(interaction_pars)[i]) > 0){
      if(relativeEdgeSize){
      igraph::E(network)[i]$width <- 4* abs( interaction_pars[i]/min(interaction_pars)  )
      }else{igraph::E(network)[i]$width = edgeSize}
    }
    
    if(verbose){
      igraph::E(network)[i]$label <- round(c(interaction_pars)[i],2)
    }
    
}
  
  #vertex attributes:
  
  relative_abundances = colMeans(observations)[-1]
  
  if(!keepNames){
  igraph::V(network)$label = as.character(1:(length(igraph::V(network))))
  }
  
  if(substrate){
    #substrate
    igraph::V(network)[1]$color="white"
    igraph::V(network)[1]$label="S"
    igraph::V(network)[-1]$label = as.character(1:(length(igraph::V(network))-1))
    igraph::V(network)[1]$size = mean(relative_abundances)/vsize
    
    #label
    ' for(i in 2:ncol(parms_matrix)){
    V(network)[i]$label = as.character(i )
  }'
  
    }
  
  for (i in 1:length(igraph::V(network))){
    
    if(keepNames){
    igraph::V(network)[i]$label = colnames(observations[,-1])[i]
    }
    if (linear_pars[i]<0){   
      igraph::V(network)[i]$color="red"
      
      if(relativeVSize){
      
      igraph::V(network)[i]$size = (relative_abundances[i]/min(relative_abundances))*vsize # normalize to avoid negative values .  Vertex size should represent relative abundance of species... not linear parameter.. linear parameter maybe as extra line coming from nowhere (ecosystem)
      }else{igraph::V(network)[i]$size = vsize}
    }
    if (linear_pars[i]>0){igraph::V(network)[i]$color="blue"}
    if(relativeVSize){
    igraph::V(network)[i]$size = (relative_abundances[i]/min(relative_abundances))*vsize # normalize to avoid negative values .  Vertex size should represent relative abundance of species... not linear parameter.. linear parameter maybe as extra line coming from nowhere (ecosystem)
    }else{igraph::V(network)[i]$size = vsize}
  }
  
  return(network)
  
  }
