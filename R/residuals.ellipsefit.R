residuals.ellipsefit <- function(g){
  resid.x <-g$pred.x-g$x
  resid.y <-g$pred.y-g$y
  resid.geometric <- sqrt(resid.x^2+resid.y^2)
  resid.algebraic <- g$residuals
  return(list("input"=resid.x,"output"=resid.y,"geometric"=resid.geometric,"algebraic"=resid.algebraic))
}