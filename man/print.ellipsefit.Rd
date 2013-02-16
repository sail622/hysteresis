\name{print.ellipsefit}
\alias{print.ellipsefit}
\title{
Print ellipsefit Estimates
}
\description{
print methods for classes \code{ellipsefit}, \code{ellipsesummary}, \code{ellipsefitlist} or \code{ellipsesummarylist} created by the functions \code{\link{fel}} and \code{\link{summary.ellipsefit}}.
}
\usage{
print.ellipsefit(g, ...)
print.ellipsesummary(g, ...)
print.ellipsefitlist(g, ...)
}
\arguments{
  \item{g}{
\code{ellipsefit}, \code{ellipsesummary}, \code{ellipsefitlist} or \code{ellipsesummarylist} object.
}
  \item{\dots}{
passed to methods.
}
}
\details{
See \code{\link{ellipse.parameters}} for descriptions of parameters returned. \code{\link{summary.ellipsefit}} produces more printout than \code{\link{fel}}.
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}
\seealso{
\code{\link{fel}} for fitting points that form an ellipse and creating an \code{ellipsefit} object, \code{\link{plot.ellipsefit}}for plotting an ellipsefit object.
\code{\link{summary.ellipsefit}} for bootstrapping and summarizing an \code{ellipsefit} object, and \code{\link{plot.ellipsesummary}}for plotting an \code{ellipsesummary} object.
}
\examples{
ellipse1 <- mel(sd.x=0.2,sd.y=0.04)
ellipse1.fit <- fel(ellipse1$x,ellipse1$y)
plot(ellipse1.fit,xlab="Input",ylab="Output",main="Simulated Ellipse", putTime=TRUE)
boot.ellipse1.fit <- summary(ellipse1.fit)
plot(boot.ellipse1.fit,xlab="Input",ylab="Output",main="Bootstrapped Ellipse", putTime=TRUE)
}

