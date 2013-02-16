fitstatistics <- function (x,y,newx,newy,n,method,period){
if (method == "harmonic2") 
  d.f. <- 6
else d.f. <- 5
MSE.full <- sum((y - newy)^2 + (x - newx)^2)/(n * 2 - d.f.)
MSE.y <- sum((y - newy)^2)/(n - d.f.)
bia <- (n - 1)/n
aic.y <- log(MSE.y * bia) + 1 + d.f. * 2/n
aic.full <- log(MSE.y * bia) + 1 + d.f./n
residual.correlation <- cor(y - newy, x - newx)
return(c("MSE.full" = MSE.full, "MSE.y" = MSE.y, "bias" = bia, 
         "AIC.full" = aic.full, "AIC.y" = aic.y, "Residual.Correlation" = residual.correlation,"n"=n,
         "period"=period,"d.f."=n-d.f.))
}