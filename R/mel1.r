mel1 <- function(cx=32,cy=39,rotated.angle=2,lambda1=7,lambda2=0.23,phase.angle=0,n.points=24,period=24,sd.x=0,sd.y=0) {
theta <- rotated.angle/180*pi
 t <-(0:(n.points-1))/period*2*pi
inti <- internal.1(lambda1,lambda2,abs(theta))
der <- derived.1(lambda1,lambda2,abs(theta),inti[1],inti[2],inti[3],period)
wrx <- rnorm(n.points,0,sd.x)
wry <- rnorm(n.points,0,sd.y)
x <- lambda1*cos(theta)*cos(t)-lambda2*sin(theta)*sin(t)+cx+wrx
y <- lambda1*sin(theta)*cos(t)+lambda2*cos(theta)*sin(t)+cy+wry
ans <- list("values"=c("cx"=cx,"cy"=cy,"rotated.angle"=rotated.angle,"lambda1"=lambda1,"lambda2"=lambda2,"p.a."=inti[4],"b.x"=inti[1],"b.y"=inti[2],"phase.angle"=phase.angle,
"n.points"=n.points,"period"=period,"area"=der[1],"lag"=der[2],"retention"=inti[3],"coercion"=der[3]),"method"=1,"x"=x,"y"=y)
class(ans) <- "ellipsemake"
ans
}

