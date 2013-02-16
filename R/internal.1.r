internal.1 <- function(lambda1,lambda2,theta)
{
b.x <- sqrt((lambda1*cos(theta))^2+(lambda2*sin(theta))^2)
p.a. <- -asin(lambda1*cos(theta)/b.x)+pi/2
retention <- -lambda1*sin(theta)*cos(pi/2+p.a.)+lambda2*cos(theta)*cos(p.a.)
b.y <- (lambda1*sin(theta)-retention*sin(p.a.))/cos(p.a.)
return(c(b.x,b.y,retention,p.a.))
}