fel.nls <-
function (x,y,ti,control,period,pred.method) {
n <- length(x)
results <- direct(x,y)   
##direct performs the inner computations and defines cx,cy,lambda1,lambda2, and theta globally.
z <- rep(1,n)   
nls.polar.fit<-nls(z~
     ( cos(theta)*(x-cx)+sin(theta)*(y-cy))^2/(((ampx)^2+(ampy)^2+((ampx)^2-(ampy)^2)/((cos(theta))^2-(sin(theta))^2))/2)      + (-sin(theta)*(x-cx)+cos(theta)*(y-cy))^2/((ampx^2+ampy^2-(ampx^2-ampy^2)/((cos(theta))^2-(sin(theta))^2))/2),
     start=list(
     ampx=sqrt((lambda1*cos(theta))^2+(lambda2*sin(theta))^2),
     ampy=sqrt((lambda1*sin(theta))^2+(lambda2*cos(theta))^2),
     cx=cx,cy=cy,theta=theta),control=control,trace=F
    )

    #summary(nls.polar.fit)
    ampx<-as.vector(coef(nls.polar.fit)[1])
    ampy<-as.vector(coef(nls.polar.fit)[2])

    theta<-as.vector(coef(nls.polar.fit)[5])
    rotated.angle <- theta*180/pi
    
    cx <- as.vector(coef(nls.polar.fit)[3])
    cy <- as.vector(coef(nls.polar.fit)[4])
                                                                  
    lambda1<-sqrt((((ampx)^2+(ampy)^2+((ampx)^2-(ampy)^2)/((cos(theta))^2-(sin(theta))^2))/2)  )
    lambda2<-sqrt((ampx^2+ampy^2-(ampx^2-ampy^2)/((cos(theta))^2-(sin(theta))^2))/2)


if (lambda2 > lambda1){
  lam <- lambda2; lambda2 <- lambda1; lambda1 <- lam;theta <- theta +pi/2;rotated.angle <- theta/180*pi;
}
    MSE<-(summary(nls.polar.fit)$sigma)^2
    
    
inti <- internal.1(lambda1,lambda2,theta)
der <- derived.1(lambda1,lambda2,theta,inti[1],inti[2],inti[3],period)
split.angle = atan2(sqrt(ampy^2-inti[3]^2),ampx)
split.angle = split.angle*180/pi
preds <- geometric_distance("x"=x,"y"=y,"cx"=cx,"cy"=cy,"lambda1"=lambda1,"lambda2"=lambda2,"theta"=theta,ti,"pred.method"=pred.method,"ampx"=ampx,"ampy"=ampy,"lag"=der[2])    
der.summ <- fitstatistics(x,y,preds$pred.x,preds$pred.y,n,method="nls",period)
    
z <- c("cx"=cx,"cy"=cy,"theta"=theta,"lambda1"=lambda1,"lambda2"=lambda2,"rotated.angle"=rotated.angle)
z2 <- c("area"=der[1],"lag"=der[2],"coercion"=der[3], 
        "b.x"=inti[1],"b.y"=inti[2],"retention"=inti[3], "p.a."=inti[4],"ampx"=ampx,"ampy"=ampy,"split.angle"=split.angle, "n"=n)
   res<-list("method"="nls","x"=x,"y"=y,"pred.x"=preds$pred.x,"pred.y"=preds$pred.y,"period.time"=preds$period.time,
             "coefficients"=z,"derived.values"=z2,"fit.statistics"=c("MSE.nls"=MSE,der.summ),
             "residuals"=resid(nls.polar.fit),"fit"=nls.polar.fit)
   res
   }
