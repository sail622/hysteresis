fel.lm <-
function(x,y,ti,period,pred.method){
n <- length(x)
x2 <- -x^2
y2 <- y^2
xy <- x*y
int <- rep(1,n)

model <- lm(terms(x2~0+xy+y2+x+y+int,keep.order=TRUE))
a <- as.vector(c(1,coef(model)))

theta = atan2(a[2],a[1]-a[3])/2
while(theta<0){theta<-pi/2+theta}


cx <- -(2*a[3]*a[4]-a[2]*a[5])/(4*a[1]*a[3]-a[2]*a[2])
cy <- -(2*a[1]*a[5]-a[2]*a[4])/(4*a[1]*a[3]-a[2]*a[2])

major <- 1/sqrt((a[1]*cos(theta)*cos(theta) + a[2]*cos(theta)*sin(theta) + a[3]*sin(theta)*sin(theta)) / (a[1]*cx*cx + a[2]*cx*cy + a[3]*cy*cy - a[6]))
minor <- 1/sqrt((a[1]*sin(theta)*sin(theta) - a[2]*cos(theta)*sin(theta) + a[3]*cos(theta)*cos(theta)) / (a[1]*cx*cx + a[2]*cx*cy + a[3]*cy*cy - a[6]))


lambda1<- major 
lambda2<-minor
if (lambda2 > lambda1){
  lambda2 <- lambda1; lambda1 <- minor;theta <- theta +pi/2;
}
rotated.angle <- theta*180/pi

inti <- internal.1(lambda1,lambda2,theta)
der <- derived.1(lambda1,lambda2,theta,inti[1],inti[2],inti[3],period)
amps <- derived.amps(inti[1],inti[2],inti[3])   

preds <- geometric_distance("x"=x,"y"=y,"cx"=cx,"cy"=cy,"lambda1"=lambda1,"lambda2"=lambda2,"theta"=theta,ti,
                            "pred.method"=pred.method,"ampx"=amps[1],"ampy"=amps[2],"lag"=der[2])

der.summ <- fitstatistics(x,y,preds$pred.x,preds$pred.y,n,method="lm",period)
    
z <- c("cx"=cx,"cy"=cy,"theta"=theta,"lambda1"=lambda1,"lambda2"=lambda2,"rotated.angle"=rotated.angle)
 z2 <- c("area"=der[1],"lag"=der[2],"coercion"=der[3],
"b.x"=inti[1],"b.y"=inti[2],"retention"=inti[3], "p.a."=inti[4],"ampx"=amps[1],"ampy"=amps[2],"split.angle"=amps[3], "n"=n)

   res <- list("method"="lm","x"=x,"y"=y,"pred.x"=preds$pred.x,"pred.y"=preds$pred.y,"period.time"=preds$period.time,"coefficients"=z,"derived.values"=z2,
               "fit.statistics"=der.summ,"residuals"=resid(model),"fit"=model)
   res
}



