geometric_distance <- function(x,y,cx,cy,lambda1,lambda2,theta,ti,pred.method,ampx,ampy,lag) {
if (pred.method=="find.times") {
  ti<-numeric(length(x))
for (i in 1:length(x)) {
  x0<-x[i]
  y0<-y[i]
  zmin1<-optimize(ellipsespot,c(0,pi),"x0"=x0,"y0"=y0,"cx"=cx,"cy"=cy,"lambda1"=lambda1,"lambda2"=lambda2,"theta"=theta)
  zmin2<-optimize(ellipsespot,c(pi,2*pi),"x0"=x0,"y0"=y0,"cx"=cx,"cy"=cy,"lambda1"=lambda1,"lambda2"=lambda2,"theta"=theta)
  ti[i]<-ifelse(zmin1$objective < zmin2$objective, zmin1, zmin2)[[1]]
  }
  pred.x<-cx +lambda1*cos(theta)*cos(ti)-lambda2*sin(theta)*sin(ti)
  pred.y<-cy +lambda1*sin(theta)*cos(ti)+lambda2*cos(theta)*sin(ti)
}
else {
  srad<-sin(ti)
  crad <- cos(ti)
  ZZ <- cbind(srad,crad,rep(1,length(srad)))
  phase.Ta.nls.polar <- atan2((- c(1,0,0)%*% solve(t(ZZ)%*%ZZ)%*%t(ZZ)%*%x),(c(0,1,0)%*% solve(t(ZZ)%*%ZZ)%*%t(ZZ)%*%x))
  pred.x <- ampx*cos(ti+phase.Ta.nls.polar)+cx
  pred.y <- ampy*cos(ti+phase.Ta.nls.polar-lag*pi/12)+cy
}

return(list("pred.x"=pred.x,"pred.y"=pred.y,"period.time"=ti))
}