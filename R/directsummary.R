directsummary <-
function (g,N,mc,studentize,center,cbb=cbb) {
    wr1<- g$x-g$pred.x
    wr2<- g$y-g$pred.y
    if (center==TRUE) {
      wr1 <- wr1 - mean(wr1) 
      wr2 <- wr2 - mean(wr2)
    }
    n <- g$fit.statistics["n"]
    if (is.numeric(cbb)==TRUE){
      k <- n/cbb
      if (abs(k-round(k)) > 0.00001 || cbb <= 0 || abs(cbb-round(cbb)) > 0.00001) stop("invalid value for cbb.")}
    
    if (studentize==TRUE) {
      Xmat <- cbind(rep(1,n),sin(g$period.time),cos(g$period.time))
      h <- Xmat%*%solve(crossprod(Xmat))%*%t(Xmat)
      r.Ta <- wr1/sqrt(1-h)
      r.Tb <- wr2/sqrt(1-h)     
      wr1 <- r.Ta-mean(r.Ta)
      wr2 <- r.Tb-mean(r.Tb) 
    }
if (mc > 1) {
  cl <- makeCluster(mc,methods=FALSE)
  bootdat <- clusterMap(cl,directboot, j=1:N, MoreArgs=list(wr1=wr1,wr2=wr2,x.pred=g$pred.x,y.pred=g$pred.y,n=n,cbb=cbb),SIMPLIFY=TRUE)
  stopCluster(cl)
  }
  else  bootdat <- mapply(directboot, j=1:N, MoreArgs=list(wr1=wr1,wr2=wr2,x.pred=g$pred.x,y.pred=g$pred.y,n=n,cbb=cbb))
    bootint2 <- matrix(internal.1(bootdat[4,],bootdat[5,],bootdat[3,]),nrow=N,ncol=4)
    bootderived <- matrix(derived.1(bootdat[4,],bootdat[5,],bootdat[3,],bootint2[,1],bootint2[,2],bootint2[,3],rep(g$fit.statistics["period"],N)),nrow=N,ncol=3)
    bootamps <- matrix(derived.amps(bootint2[,1],bootint2[,2],bootint2[,3]),nrow=N,ncol=3)    
    bootdat <- data.frame(t(bootdat),bootderived,bootint2,bootamps,rep(g$fit.statistics["n"],N))
    colnames(bootdat) <- names(c(g$coefficients,g$derived.values))
    
    if (diff(range(bootdat[,"rotated.angle"])) > 170)
      warning("Bootstrapped rotated angle values on both sides of 0, 180 degrees.")    
    
error<-apply(bootdat,2,sd,na.rm=T)
themean<-apply(bootdat,2,mean,na.rm=T)
ranges<-apply(bootdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T)
full <- data.frame(c(g$coefficients,g$derived.values),t(ranges),error,themean)
colnames(full) <- c("measured.value","q0.025","q0.25","q0.5","q0.75","q0.975","Std. Error","Mean")
    
full$Bias <- full$Mean-full$measured.value
full$Estimate <- full$measured.value-full$Bias   
    full[,c("q0.025","q0.25","q0.5","q0.75","q0.975")]<-full[,c("q0.025","q0.25","q0.5","q0.75","q0.975")]-
      full$Bias
    
coefficients <- full[1:length(g$coefficients),]
derived.values <- full[(1+length(g$coefficients)):length(themean),]
    
rad<-g$period.time+full["Ta.phase","Estimate"]
pred.x<-full["b.x","Estimate"]*cos(rad)+full["cx","Estimate"] 
pred.y<-full["b.y","Estimate"]*cos(rad)+full["retention","Estimate"]*sin(rad)+full["cy","Estimate"]    
    
full2<-list("coefficients"=coefficients,"derived.values"=derived.values,
"fit.statistics"=g$fit.statistics,"pred.x"=g$pred.x,"pred.y"=g$pred.y, "x"=g$x,"y"=g$y,"call"=g$call,"method"=g$method,"boot.data"=bootdat)
invisible(full2)

}
