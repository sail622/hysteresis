fundamental.ellipsesummary <- function(g){
  if (g$method=="harmonic2")
  rbind(g$coefficients,g$derived.values)[c("cx","cy","ampx","ampy","rotated.angle"),]
  
}