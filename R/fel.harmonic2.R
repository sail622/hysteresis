fel.harmonic2 <-
function(x,y,ti,period,pred.method){


  Ta.lm<-lm.fit(cbind(rep(1,length(x)),sin(ti),cos(ti)),x)            
  b.x<-sqrt(coef(Ta.lm)[[2]]^2+coef(Ta.lm)[[3]]^2)
  phase.angle<- atan2(coef(Ta.lm)[[3]],coef(Ta.lm)[[2]])-pi/2
  rad<-ti+phase.angle
  cx<-coef(Ta.lm)[[1]]
  Tb.lm<-lm.fit(cbind(rep(1,length(y)),sin(rad),cos(rad)),y)

  b.y<-coef(Tb.lm)[[3]]
  retention<- coef(Tb.lm)[[2]]
  cy<-coef(Tb.lm)[[1]]
inti <- internal.2(b.x,b.y,retention,phase.angle)
  n <- length(x)
 der <- derived.2(b.x,b.y,retention,period)
    
     newx<-b.x*cos(rad)+cx
 
newy<-b.y*cos(rad)+retention*sin(rad)+cy

der.summ <- fitstatistics(x,y,newx,newy,n,method="harmonic2",period)
  residual <- sqrt((x-newx)^2+(y-newy)^2)
amps <- derived.amps(b.x,b.y,retention)  
  
   z <- c("cx"=cx,"cy"=cy, "b.x"=b.x,"b.y"=b.y,"phase.angle"=phase.angle, "retention"=retention)
  z2 <- c("area"=der[1], "lag"=der[2], "coercion"=der[3],"theta"=inti[1],
          "rotated.angle"=inti[2],"lambda1"=inti[3],"lambda2"=inti[4],"p.a."=inti[5],"ampx"=amps[1],"ampy"=amps[2],"split.angle"=amps[3] )
   res<-list("method"="harmonic2","x"=x,"y"=y,"pred.x"=newx,"pred.y"=newy,"period.time"=ti,
             "coefficients"=z,"derived.values"=z2,"fit"=list(Ta.lm,Tb.lm),"fit.statistics"=der.summ,"residuals"=residual)
   res
    }
