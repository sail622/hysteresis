\name{plot.ellipsefit}
\alias{plot.ellipsefit}
 \alias{plot.ellipsefitlist}
\title{
Plot a fitted ellipse.
}
\description{
A scatterplot of x and y fitted with an ellipse. Uses a \code{fitellipse} object created by \code{\link{fel}}. Can also plot an \code{ellipsefitlist} object that contains multiple ellipses,
in this case an \code{mfrow} argument must also be provided. See \code{\link{par}}.
}
\usage{
plot.ellipsefit(a, startTime = 0, putNumber = F, putTime = F, values = TRUE, 
    xlab = "Input", ylab = "Output", xlim=NULL,ylim=NULL,main = "Main", newPred = T, 
    show = NULL, ...)
plot.ellipsefitlist(a,mfrow, startTime = 0, putNumber = F, putTime = F, values = TRUE, 
    xlab = "Input", ylab = "Output", main = "Main", newPred = T, 
    show = NULL, ...)  
}
\arguments{
  \item{a}{
\code{ellipsefit} object created by the function \code{\link{fel}}.
}
  \item{startTime}{
optional number that represents when period starts.
}
  \item{putNumber}{
optional logical that numbers points from first to last. 
}
  \item{putTime}{
optional logical that prints point times on plot.
}
  \item{xlab}{
a title for the x axis.
}
  \item{ylab}{
a title for the y axis.
}
  \item{xlim}{
limits for x axis.
}
  \item{ylim}{
limits for y axis.
}
  \item{main}{
an overall title for the plot.
}
  \item{newPred}{
draw an ellipse with 100 points. If \code{FALSE} use predicted ellipse from \code{ellipsefit} object which will result in a rougher shape.
}
  \item{show}{
a character vector of parameters to be shown in the plot. Possible values are "Retention", "Coercion", "Lambdas" and "Bs".
}
  \item{\dots}{
Arguments to be passed to methods.
}
}

\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}

\seealso{
\code{\link{fel}} for fitting points that form an ellipse and creating an \code{ellipsefit} object, \code{\link{summary.ellipsefit}} for bootstrapping and summarizing an ellipsefit object, and \code{\link{plot.ellipsesummary}}for plotting an ellipsesummary object.
}
\examples{
ellipse1 <- mel(sd.x=0.2,sd.y=0.04)
ellipse1.fit <- fel(ellipse1$x,ellipse1$y)
plot(ellipse1.fit,xlab="Input",ylab="Output",main="Simulated Ellipse", putTime=TRUE)
boot.ellipse1.fit <- summary(ellipse1.fit)
plot(boot.ellipse1.fit,xlab="Input",ylab="Output",main="Bootstrapped Ellipse", putTime=TRUE)
}

\keyword{ hplot }
