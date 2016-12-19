correctly_intervalized_estimates <-
function(background, SE, Parms){
    CI95 <- SE * 2
    up_estimates <- background + CI95
    down_estimates <- background - CI95
    return(down_estimates < c(Parms) & c(Parms) < up_estimates)
  }
