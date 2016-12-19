
#-------------- draw_interaction_network igraph ---------------------------

draw_interaction_network <-
function(network, main=NULL){
  
  igraph::plot.igraph(network,layout = igraph::layout.fruchterman.reingold , vertex.label.color="black",edge.color="black", edge.curved=TRUE, main=main)
  
}