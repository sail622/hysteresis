floop <- function(x,y=NULL,m=1,n=1,times="even",period=24,cycles=NULL, subset=NULL,na.action=getOption("na.action"),classical=FALSE) {
  floopcall <- match.call()
  if (ncol(matrix(x)) > 2)
    times <- x[,3]
  dat <- xy.coords(x,y)
  if (!is.null(subset)) {
    dat$x<-dat$x[subset]; dat$y<-dat$y[subset];
    if (!is.null(cycles))
      cycles<-cycles[subset]
    if (is.numeric(times))
      times<-times[subset]}
  if (!is.null(cycles)) {
    dat <- cbind("x"=dat$x,"y"=dat$y)
    if (is.numeric(times))
      ans <- by(cbind(dat,times),cycles,floop,m=m,n=n,period=period,na.action=na.action)
    else
      ans <- by(dat,cycles,floop,m=m,n=n,period=period,times=times,na.action=na.action)
    class(ans) <- "fittedlooplist" 
    attr(ans,"call") <- floopcall
    names(ans) <- levels(factor(cycles))
    return(ans)
  }
  
  if (is.null(period))
    period <- length(dat$x)
 suppressWarnings(if (times=="even")
  t <- (0:(length(x)-1))/period*pi*2
 else t <- times)
 matx <- cbind(rep(1,length(x)),sin(t),cos(t))
 
 xfit <- lm.fit(matx,x)
 cx <- as.vector(coef(xfit)[1])
 b.x <- as.vector(sqrt(coef(xfit)[2]^2+coef(xfit)[3]^2))
 phase.angle<- atan2(coef(xfit)[3],coef(xfit)[2])-pi/2
 costp <- cos(t+phase.angle)
 if (classical==FALSE) maty <- cbind(rep(1,length(x)),sin(t+phase.angle)^m,costp^n)
 if (classical==TRUE) {
   direc <- sign(costp)
   maty <- cbind(rep(1,length(x)),sin(t+phase.angle)^m,direc*abs(costp)^n)
 }
  yfit <- lm.fit(maty,y)
 cy <- as.vector(coef(yfit)[1])
 retention <- as.vector(coef(yfit)[2])
 b.y <- as.vector(coef(yfit)[3])
 pred.x<-cx+b.x*cos(t+phase.angle)
  if (classical==FALSE) pred.y<-cy+retention*sin(t+phase.angle)^m+b.y*costp^n
 if (classical==TRUE)  pred.y<-cy+retention*sin(t+phase.angle)^m+direc*(b.y*abs(costp)^n)
  residuals <- sqrt((x-pred.x)^2+(y-pred.y)^2)
  if (n==1) beta.split.angle<-atan2(b.y,b.x) 
    else if (n==2 | n==3) beta.split.angle <- pi/2
    else beta.split.angle<-NA
  hysteresis <- 1/sqrt(1+(b.y/retention)^(2/m))
  area <- (0.5/(beta((m+3)/2,(m+3)/2)*(m+2))+1/beta((m+1)/2,(m+1)/2)-1/beta((m+3)/2,(m-1)/2))/(2^m)*pi*abs(retention*b.x)
  ans <- list("values"=c("cx"=cx,"cy"=cy, "b.x"=b.x,"b.y"=b.y,"m"=m,"n"=n,"phase.angle"=phase.angle,"retention"=retention,
                  "beta.split.angle"=beta.split.angle,"hysteresis"=hysteresis,"area"=area),"fit"=list(xfit,yfit),
              "x"=x,"y"=y,"pred.x"=pred.x,"pred.y"=pred.y,"period.time"=t+phase.angle,"residuals"=residuals)
class(ans) <- "fittedloop"
  ans
}

