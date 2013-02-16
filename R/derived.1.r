derived.1 <- function(lambda1,lambda2,theta,b.x,b.y,retention,period) {
area <- lambda1*lambda2*pi
coercion <- b.x/(sqrt(1+(b.y/retention)^2))
lag<-abs(atan2(retention,b.y))*period/(pi*2)
z <- c(area,lag,coercion)
z
}