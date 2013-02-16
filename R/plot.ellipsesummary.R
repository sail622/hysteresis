plot.ellipsesummary<-function(a,startTime=0,putNumber=FALSE,putTime=FALSE,values=TRUE,xlab="Input",ylab="Output",main="Main",xlim=NULL,ylim=NULL,newPred=TRUE,show=NULL,...){
a$values <- rbind(a$coefficients,a$derived.values)
  if (newPred==TRUE)
  {
ti <- (1:101)*pi/50
newX <- a$values["b.x","Estimate"]*cos(ti)+a$values["cx","Estimate"]
newY <- a$values["b.y","Estimate"]*cos(ti)+a$values["retention","Estimate"]*sin(ti)+a$values["cy","Estimate"]
}
else {
newY <- a$pred.y
newY[length(newY)+1] <- newY[1]
newX <- a$pred.x
newX[length(newX)+1] <- newX[1] }
if (is.null(xlim)) xlim <-c(min(c(a$x,newX)),max(c(a$x,newX)))
if (is.null(ylim)) ylim <- c(min(c(a$y,newY)),max(c(a$y,newY)))  
plot(newY~newX,type="l",ylim=ylim,xlim=xlim
    ,xlab=xlab,ylab=ylab
    )
if (values==FALSE)
title( paste(main),cex=2)

if (values==TRUE) {
  title(line=3, paste(main),cex=1.2)
mtext(bquote(paste(
     "Lag"==.(format(a$values["lag","Estimate"],digits=4)))),side=3,line=1.85,cex=0.75)
mtext(bquote(paste("Ellipse Area"==.(format(a$values["area","Estimate"],digits=3)))),side=3,line=0.95,cex=0.75)
mtext(paste("Retention",format(a$values["retention","Estimate"],digits=4),"Coercion=",format(a$values["coercion","Estimate"],digits=3)),side=3,line=0.0,cex=0.75)
}
points(a$y~a$x,pch=1,cex=0.85)

if (any(show=="Lambdas")) {segments(a$values["cx","Estimate"],a$values["cy","Estimate"],a$values["cx","Estimate"]+a$values["lambda1","Estimate"]*cos(a$values["theta","Estimate"]),a$values["cy","Estimate"]+a$values["lambda1","Estimate"]*sin(a$values["theta","Estimate"]),col="red")
segments(a$values["cx","Estimate"],a$values["cy","Estimate"],a$values["cx","Estimate"]+a$values["lambda2","Estimate"]*cos(a$values["theta","Estimate"]+pi/2),a$values["cy","Estimate"]+a$values["lambda2","Estimate"]*sin(a$values["theta","Estimate"]+pi/2),col="red")}

if (any(show=="Bs")) segments(a$values["cx","Estimate"],a$values["cy","Estimate"],a$values["cx","Estimate"]+a$values["b.x","Estimate"],a$values["cy","Estimate"]+a$values["b.y","Estimate"],col="blue")

if (any(show=="Retention")) segments(a$values["cx","Estimate"],a$values["cy","Estimate"],a$values["cx","Estimate"],a$values["cy","Estimate"]+a$values["retention","Estimate"],col="purple")

if (any(show=="Coercion")) segments(a$values["cx","Estimate"],a$values["cy","Estimate"],a$values["cx","Estimate"]+a$values["coercion","Estimate"],a$values["cy","Estimate"],col="yellow")
  
  # add points value
if(putNumber==TRUE){
  text(a$x,a$y,as.character(format(a$y,digits=4)))
}
  hour<-NA
 if(startTime!=0){
 hour<-c(startTime:length(a$x),0:(startTime-1))
 }
 else{
 hour<-c(0:(length(a$x)-1))
 }


if(putTime==TRUE){
  text(a$x,a$y,as.character(hour),cex=1.1)
}

}

