plot.ellipsefitlist<-function(a,mfrow,startTime=0,putNumber=F,putTime=F,xlab="Input",ylab="Output",main="Main",newPred=T,show=NULL,...){
  par(mfrow=mfrow,oma=c(1.5,2,2,1))
    mapply(plot.ellipsefit,a,main=names(a),MoreArgs=list(startTime=startTime,putNumber=putNumber,putTime=putTime,xlab=xlab,ylab=ylab,newPred=newPred,show=show))
  mtext(paste(main),side=3,outer=T)
  }
