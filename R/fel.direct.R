fel.direct <-
function(x,y,ti,period,pred.method){
n <- length(x)
results <- direct(x,y)   
##direct performs the inner computations and defines cx,cy,lambda1,lambda2, and theta globally.
inti <- internal.1(lambda1,lambda2,theta)
der <- derived.1(lambda1,lambda2,theta,inti[1],inti[2],inti[3],period)
amps <- derived.amps(inti[1],inti[2],inti[3]) 

preds <- geometric_distance("x"=x,"y"=y,"cx"=cx,"cy"=cy,"lambda1"=lambda1,"lambda2"=lambda2,"theta"=theta,ti,pred.method
                            ,"ampx"=amps[1],"ampy"=amps[2],"lag"=der[2])
   
der.summ <- fitstatistics(x,y,preds$pred.x,preds$pred.y,n,method="lm",period)
    
z <- c("cx"=cx,"cy"=cy,"theta"=theta,"lambda1"=lambda1,"lambda2"=lambda2,"rotated.angle"=rotated.angle)
 z2 <- c("area"=der[1],"lag"=der[2],"coercion"=der[3], 
"b.x"=inti[1],"b.y"=inti[2],"retention"=inti[3], "p.a."=inti[4],"ampx"=amps[1],"ampy"=amps[2],"split.angle"=amps[3],"n"=n)

   res <- list("method"="direct","x"=x,"y"=y,"pred.x"=preds$pred.x,"pred.y"=preds$pred.y,"period.time"=preds$period.time,"coefficients"=z,"derived.values"=z2,
               "fit.statistics"=der.summ,"residuals"=results$residuals,"fit"=results$fit)
   res
}



