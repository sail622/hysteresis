plot.ellipsefit<-function(a,startTime=0,putNumber=FALSE,putTime=FALSE,values=TRUE,xlim=NULL,ylim=NULL,xlab="Input",ylab="Output",main="Main",newPred=TRUE,show=NULL,...){
  a$values <- c(a$coefficients,a$derived.values)
  if (newPred==TRUE)
  {
ti <- (1:101)*pi/50
newX <- a$values["b.x"]*cos(ti)+a$values["cx"]
newY <- a$values["b.y"]*cos(ti)+a$values["retention"]*sin(ti)+a$values["cy"]
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
     "Lag"==.(format(a$values["lag"],digits=4)))),side=3,line=1.85,cex=0.75)
mtext(bquote(paste("Ellipse Area"==.(format(a$values["area"],digits=3)))),side=3,line=0.95,cex=0.75)
mtext(paste("Retention",format(a$values["retention"],digits=4),"Coercion=",format(a$values["coercion"],digits=3)),side=3,line=0.0,cex=0.75)
}
  
points(a$y~a$x,pch=1,cex=0.85)

if (any(show=="Lambdas")) {segments(a$values["cx"],a$values["cy"],a$values["cx"]+a$values["lambda1"]*cos(a$values["theta"]),a$values["cy"]+a$values["lambda1"]*sin(a$values["theta"]),col="red")
segments(a$values["cx"],a$values["cy"],a$values["cx"]+a$values["lambda2"]*cos(a$values["theta"]+pi/2),a$values["cy"]+a$values["lambda2"]*sin(a$values["theta"]+pi/2),col="red")}

if (any(show=="Bs")) segments(a$values["cx"],a$values["cy"],a$values["cx"]+a$values["b.x"],a$values["cy"]+a$values["b.y"],col="blue")

 if (any(show=="Retention")) segments(a$values["cx"],a$values["cy"],a$values["cx"],a$values["cy"]+a$values["retention"],col="purple")
  
  if (any(show=="Coercion")) segments(a$values["cx"],a$values["cy"],a$values["cx"]+a$values["coercion"],a$values["cy"],col="yellow")
  
  # add points value
if(putNumber==TRUE) text(a$x,a$y,as.character(format(a$y,digits=4)))

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

