lmboot <-
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

          x2 <- -x.boot^2
          y2 <- y.boot^2
          xy <- x.boot*y.boot
          int <- rep(1,n)
          
          model <- lm(terms(x2~0+xy+y2+x.boot+y.boot+int,keep.order=TRUE))
          a <- as.vector(c(1,coef(model)))
          
          theta = atan2(a[2],a[1]-a[3])/2
          rotated.angle<- 180*theta/pi  # rotated angle in degree
          while(rotated.angle<0){rotated.angle<-90+rotated.angle}
          theta <- rotated.angle/180*pi
          
          cx <- -(2*a[3]*a[4]-a[2]*a[5])/(4*a[1]*a[3]-a[2]*a[2])
          cy <- -(2*a[1]*a[5]-a[2]*a[4])/(4*a[1]*a[3]-a[2]*a[2])
          
          major <- 1/sqrt((a[1]*cos(theta)*cos(theta) + a[2]*cos(theta)*sin(theta) + a[3]*sin(theta)*sin(theta)) / (a[1]*cx*cx + a[2]*cx*cy + a[3]*cy*cy - a[6]))
          minor <- 1/sqrt((a[1]*sin(theta)*sin(theta) - a[2]*cos(theta)*sin(theta) + a[3]*cos(theta)*cos(theta)) / (a[1]*cx*cx + a[2]*cx*cy + a[3]*cy*cy - a[6]))
          
          
          lambda1 <- abs(major) 
          lambda2 <-abs(minor)
          if (lambda2 > lambda1){
            lambda2 <- lambda1; lambda1 <- abs(minor);theta <- theta +pi/2;rotated.angle <- theta*180/pi;
          }

          z <- c("cx"=cx,"cy"=cy,"theta"=theta,"lambda1"=lambda1,"lambda2"=lambda2,"rotated.angle"=rotated.angle)
          z
      }
