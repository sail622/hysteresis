harmonic2boot <-
  function(j=NULL,pred.x,pred.y,xresid,yresid,ti,n,cbb){
    index <- sample(1:(n+3),3,replace=FALSE)
    if (is.numeric(cbb)==TRUE) {
      xresid2 <- c(xresid,xresid)
      yresid2 <- c(xresid,xresid)
      k <- n/cbb
      xblocks <- sample(1:(n+3),k,replace=TRUE)
      yblocks <- sample(1:(n+3),k,replace=TRUE)
      xressamp <- c(t(outer(xblocks,0:(cbb-1),FUN="+")))
      yressamp <- c(t(outer(yblocks,0:(cbb-1),FUN="+")))
      y<-yresid2[yressamp]+pred.y[-index]
      x<-xresid2[xressamp]+pred.x[-index]
    }
    else {  
    y<-sample(yresid,n,replace=T)+pred.y[-index]
    x<-sample(xresid,n,replace=T)+pred.x[-index]}
    
    Ta.lm<-lm.fit(cbind(rep(1,n),sin(ti[-index]),cos(ti[-index])),x)            
    b.x<-sqrt(coef(Ta.lm)[[2]]^2+coef(Ta.lm)[[3]]^2)
    phase.angle<- atan2(coef(Ta.lm)[[3]],coef(Ta.lm)[[2]])-pi/2
    rad<-ti[-index]+phase.angle
    cx<-coef(Ta.lm)[[1]]
    Tb.lm<-lm.fit(cbind(rep(1,n),sin(rad),cos(rad)),y)
    
    b.y<-coef(Tb.lm)[[3]]
    retention<- coef(Tb.lm)[[2]]
    cy<-coef(Tb.lm)[[1]]

    z <- c("cx"=cx,"cy"=cy, "b.x"=b.x,"b.y"=b.y,"phase.angle"=phase.angle, "retention"=retention)
z
  }
