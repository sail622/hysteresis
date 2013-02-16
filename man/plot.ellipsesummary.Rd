\name{plot.ellipsesummary}
\alias{plot.ellipsesummary}
\title{
Plot a fitted and bootstrapped ellipse.
}
\description{
A scatterplot of x and y fitted with an ellipse. Uses an ellipsesummary object created by \code{\link{fel}} and \code{\link{summary.ellipsefit}}. Can also plot an \code{ellipsesummarylist} object that contains multiple ellipses,
in this case an \code{mfrow} argument must also be provided. See \code{\link{par}}.
}
\usage{
plot.ellipsesummary(a, startTime = 0, putNumber = F, putTime = F, xlab = "Input", ylab = "Output", main = "Main", xlim = Null, ylim = Null,show=NULL)
}

\arguments{
  \item{a}{
\code{ellipsesummary} object created by the functions \code{\link{fel}} and \code{\link{summary.ellipsefit}}.
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
  \item{xlim}{
x axis limits.
}
  \item{ylim}{
y axis limits.
}
  \item{newPred}{
draw an ellipse with 100 points. If FALSE use predicted ellipse from ellipsesummary object.
}
  \item{show}{
a character vector of parameters to be shown in the plot. Possible values are "Retention", "Coercion", "Lambdas" and "Bs".
}

  \item{\dots}{
Arguments to be passed to methods.
}
}
\details{
\code{ellipsesummary} ellipses have less biased parameter estimates.
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}
\seealso{
\code{\link{fel}}, \code{\link{plot.ellipsefit}} and \code{\link{summary.ellipsefit}}. 
}
\examples{
ellipse1 <- mel(sd.x=0.2,sd.y=0.04)
ellipse1.fit <- fel(ellipse1$x,ellipse1$y)
plot(ellipse1.fit,xlab="Input",ylab="Output",main="Simulated Ellipse", putTime=TRUE)
boot.ellipse1.fit <- summary(ellipse1.fit)
plot(boot.ellipse1.fit,xlab="Input",ylab="Output",main="Bootstrapped Ellipse", putTime=TRUE)
}
\keyword{ hplot }

