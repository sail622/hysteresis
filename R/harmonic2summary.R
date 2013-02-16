harmonic2summary <-
function (g,N,mc,studentize,cbb) {
xresid<-g$pred.x-g$x
yresid<-g$pred.y-g$y
n <- length(xresid)-3
if (is.numeric(cbb)==TRUE){
k <- n/cbb
if (abs(k-round(k)) > 0.00001) stop("number of observations - 3 divided by cbb needs to be an integer.")}
if (studentize==TRUE) {
  h.Ta <- lm.influence(g$fit[[1]])$hat
  h.Tb <- lm.influence(g$fit[[2]])$hat
  r.Ta <- xresid/sqrt(1-h.Ta)
  r.Tb <- yresid/sqrt(1-h.Tb)     
  xresid <- r.Ta-mean(r.Ta)
  yresid <- r.Tb-mean(r.Tb) 
}
if (mc > 1) {
  cl <- makeCluster(mc,methods=FALSE)
  bootdat <- clusterMap(cl,harmonic2boot,j=1:N,MoreArgs=list(pred.x=g$pred.x,pred.y=g$pred.y,xresid=xresid,yresid=yresid,ti=g$period.time,n=n,cbb=cbb),SIMPLIFY=TRUE)
  bootdat <- simplify2array(bootdat)
  stopCluster(cl)
}
else bootdat<-mapply(harmonic2boot,j=1:N,MoreArgs=list(pred.x=g$pred.x,pred.y=g$pred.y,xresid=xresid,yresid=yresid,ti=g$period.time,n=n,cbb=cbb))
bootdat<-t(bootdat)
bootderived <- matrix(derived.2(bootdat[,3],bootdat[,4],bootdat[,6],rep(g$fit.statistics["period"],N)),nrow=N,ncol=3)
bootinternal1 <- matrix(internal.2(bootdat[,3],bootdat[,4],bootdat[,6],bootdat[,5]),nrow=N,ncol=5)
bootamps <- matrix(derived.amps(bootdat[,3],bootdat[,4],bootdat[,6]),nrow=N,ncol=3)                        
bootdat <- data.frame(bootdat,bootderived,bootinternal1,bootamps)  
colnames(bootdat) <- names(c(g$coefficients,g$derived.values))

if (diff(range(bootdat[,"rotated.angle"])) > 170)
  warning("Bootstrapped rotated angle values on both sides of 0, 180 degrees.")

error<-apply(bootdat,2,sd,na.rm=T)
ranges<-apply(bootdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975),na.rm=T)
themean<-apply(bootdat,2,mean,na.rm=T)
full <- data.frame(c(g$coefficients,g$derived.values),t(ranges),error, themean)
colnames(full) <- c("measured.value","q0.025","q0.25","q0.5","q0.75","q0.975","Std. Error","Mean")

full$Bias <- full$Mean-full$measured.value
full$Estimate <- full$measured.value-full$Bias
full[,c("q0.025","q0.25","q0.5","q0.75","q0.975")]<-full[,c("q0.025","q0.25","q0.5","q0.75","q0.975")]-
  full$Bias

rad<-g$period.time+full["phase.angle","Estimate"]
pred.x<-full["b.x","Estimate"]*cos(rad)+full["cx","Estimate"] 
pred.y<-full["b.y","Estimate"]*cos(rad)+full["retention","Estimate"]*sin(rad)+full["cy","Estimate"]
coefficients <- full[1:length(g$coefficients),]
derived.values <- full[(1+length(g$coefficients)):length(themean),]
full2<-list("coefficients"=coefficients,"derived.values"=derived.values,
"fit.statistics"=g$fit.statistics, "pred.x"=pred.x,"pred.y"=pred.y, "x"=g$x,"y"=g$y,"call"=g$call,"method"=g$method,"boot.data"=bootdat)
invisible(full2)
}
