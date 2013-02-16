mloop <- function(cx=0,cy=0,retention=0.2,b.x=0.6,b.y=0.8,m=1,n=1,sd.x=0,sd.y=0,phase.angle=0,n.points=24,period=24,classical=FALSE) {
 if (classical==FALSE) {
  x<-cx+b.x*cos((1:n.points)/period*2*pi+phase.angle)+rnorm(n.points,0,sd.x)
  y<-cy+retention*sin((1:n.points)/period*2*pi+phase.angle)^m+b.y*cos((1:n.points)/period*2*pi+phase.angle)^n+rnorm(n.points,0,sd.y)
 }
  else {
   direc<-sign(cos((1:n.points)/period*2*pi+phase.angle))
   x<-cx+b.x*cos((1:n.points)/period*2*pi+phase.angle)+rnorm(n.points,0,sd.x)
   y<-cy+retention*sin((1:n.points)/period*2*pi+phase.angle)^m+direc*(b.y*abs(cos((1:n.points)/period*2*pi+phase.angle))^n)+rnorm(n.points,0,sd.y)
 }
 if (n==1) beta.split.angle<-atan2(b.y,b.x) 
 else if (n==2 | n==3) beta.split.angle <- pi/2
  else beta.split.angle<-NA
  hysteresis <- 1/sqrt(1+(b.y/retention)^(2/m))
  area <- (0.5/(beta((m+3)/2,(m+3)/2)*(m+2))+1/beta((m+1)/2,(m+1)/2)-1/beta((m+3)/2,(m-1)/2))/(2^m)*pi*abs(retention*b.x)
  if ((n%%2)!=1 | (m%%2)!=1) warning("May not be an actual hysteresis loop if m and n are not both odd, check plot.")
  ans <- list("values"=c("cx"=cx,"cy"=cy, "b.x"=b.x,"b.y"=b.y,"m"=m,"n"=n,"phase.angle"=phase.angle,"retention"=retention,
                  "beta.split.angle"=beta.split.angle,"hysteresis"=hysteresis,"area"=area),"x"=x,"y"=y)
class(ans) <- "hysteresisloop"
  ans
}

