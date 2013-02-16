fel <-
function(x,y=NULL,method="harmonic2",period=NULL,cycles=NULL,times="unknown",subset=NULL,na.action=getOption("na.action"),control=nls.control()) {
  felcall <- match.call()
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
    ans <- by(cbind(dat,times),cycles,fel,method=method,period=period,na.action=na.action,control=control)
    else
      ans <- by(dat,cycles,fel,method=method,period=period,times=times,na.action=na.action,control=control)
    class(ans) <- "ellipsefitlist" 
      attr(ans,"call") <- felcall
    names(ans) <- levels(factor(cycles))
    return(ans)
  }
  if (is.null(period))
    period <- length(dat$x)
 suppressWarnings(if (times=="unknown") {
    pred.method <- "find.times"
    dat <- data.frame(do.call(na.action,list(cbind("x"=dat$x,"y"=dat$y))))
    dat$times <-2*(0:(length(dat$x)-1))/period*pi }
  else if (is.numeric(times)){
    pred.method <- "times"
  dat <- data.frame(do.call(na.action,list(cbind("x"=dat$x,"y"=dat$y,times))))
    dat$times <-2*dat$times/period*pi}
  else {
    pred.method <- "times"
    dat <- data.frame(do.call(na.action,list(cbind("x"=dat$x,"y"=dat$y))))
    dat$times <-2*(0:(length(dat$x)-1))/period*pi })
 
  if (method=="harmonic2")
    ans <- fel.harmonic2(dat$x,dat$y,dat$times,period,pred.method)
    else if (method=="nls")
      ans <- fel.nls(dat$x,dat$y,dat$times,control,period,pred.method)
        else if (method=="direct")
        ans <- fel.direct(dat$x,dat$y,dat$times,period,pred.method)
          else if (method=="lm")
            ans <- fel.lm(dat$x,dat$y,dat$times,period,pred.method)
        else stop("method must be 'harmonic2', 'direct', 'lm' or 'nls'") 
  ans$call <- felcall
        class(ans) <- "ellipsefit"
        ans}
