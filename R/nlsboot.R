nlsboot <-
function(j=NULL,wr1,wr2,x.pred,y.pred,n,cbb){
  if (is.numeric(cbb)==TRUE) {
    xresid2 <- c(wr1,wr1)
    yresid2 <- c(wr2,wr2)
    k <- n/cbb
    xblocks <- sample(1:n,k,replace=TRUE)
    yblocks <- sample(1:n,k,replace=TRUE)
    xressamp <- c(t(outer(xblocks,0:(cbb-1),FUN="+")))
    yressamp <- c(t(outer(yblocks,0:(cbb-1),FUN="+")))
    y.boot<-yresid2[yressamp]+y.pred
    x.boot<-xresid2[xressamp]+x.pred
  }
  else {
    rx <- sample(wr1,n,replace=T)
    ry <- sample(wr2,n,replace=T) 
    x.boot<-rx +  x.pred
    y.boot<-ry +  y.pred
  }
          results <- direct(x.boot,y.boot)   
          ##direct performs the inner computations and defines cx,cy,lambda1,lambda2, and theta globally.
          z <- rep(1,n)   
          nls.polar.fit<-nls(z~
            ( cos(theta)*(x.boot-cx)+sin(theta)*(y.boot-cy))^2/(((ampx)^2+(ampy)^2+((ampx)^2-(ampy)^2)/((cos(theta))^2-(sin(theta))^2))/2)      + (-sin(theta)*(x.boot-cx)+cos(theta)*(y.boot-cy))^2/((ampx^2+ampy^2-(ampx^2-ampy^2)/((cos(theta))^2-(sin(theta))^2))/2),
                             start=list(
                               ampx=sqrt((lambda1*cos(theta))^2+(lambda2*sin(theta))^2),
                               ampy=sqrt((lambda1*sin(theta))^2+(lambda2*cos(theta))^2),
                               cx=cx,cy=cy,theta=theta),trace=F
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
          z <- c("cx"=cx,"cy"=cy,"theta"=theta,"lambda1"=lambda1,"lambda2"=lambda2,"rotated.angle"=rotated.angle)
          z
      }
