directboot <-
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
          z <- c("cx"=cx,"cy"=cy,"theta"=theta,"lambda1"=lambda1,"lambda2"=lambda2,"rotated.angle"=rotated.angle)
          z
      }
