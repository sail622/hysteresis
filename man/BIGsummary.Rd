\name{BIGsummary}
\alias{BIGsummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An Alternate summary method for class \code{ellipsefit}.
}
\description{
Prints out Delta Method standard errors and Wald confidence intervals in addition to the usual \code{\link{summary.ellipsefit}} results.
}
\usage{
BIGsummary(g)
}
\arguments{
  \item{g}{
an \code{ellipsefit} object.
}
}
\details{
Calls \code{\link{summary.ellipsefit}} and \code{\link{delta.error}} as parts of its operations. Is little more than an alternative way of printing summary and delta.error results for ellipsefit objects.
}
\value{
  \item{summary}{an \code{ellipsesummary} object. See \code{\link{summary.ellipsefit}}.}
  \item{error}{\code{\link{deltaMethod}} standard errors. See \code{\link{delta.error}}.}
  \item{C.I.boot}{Wald 95 percent confidence intervals based on bootstrap standard errors.}
  \item{C.I.delta}{Wald 95 percent confidence intervals based on \code{\link{deltaMethod}} errors.}
}
\references{
Yang, F. and A. Parkhurst, Estimating Elliptical Hysteresis: A Comparison of Analytic Methods. (submitted)
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}
\seealso{
\code{\link{fel}} fits points that form an ellipse and creates an ellipsefit object using one of the four methods available, \code{\link{summary.ellipsefit}} summarizes and produces bootstrap estimates for an 
ellipsefit object, and \code{\link{plot.ellipsefit}} plots an ellipsefit object. 
}
\examples{
ellipse1 <- mel(sd.x=0.2,sd.y=0.04)
ellipse1.fit <- fel(ellipse1$x,ellipse1$y,method="lm")
BIGsummary(ellipse1.fit)
}
\keyword{ models }

