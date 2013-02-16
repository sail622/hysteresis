ellipsespot <- function (z,x0,y0,cx,cy,lambda1,lambda2,theta) { 
  xt<-cx +lambda1*cos(theta)*cos(z)-lambda2*sin(theta)*sin(z)
  yt<-cy +lambda1*sin(theta)*cos(z)+lambda2*cos(theta)*sin(z)
  dist<-sqrt((xt-x0)^2+(yt-y0)^2)
  return("dist"=dist)}