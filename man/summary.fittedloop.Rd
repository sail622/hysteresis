\name{summary.fittedloop}
\alias{summary.fittedloop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard Errors for \code{ellipsefit} Parameters
}
\description{
Obtain delta method standard errors for parameters from an \code{fittedloop} object produced by the function \code{\link{floop}}.
}
\usage{
delta.error(g)
}
\arguments{
  \item{g}{
an \code{fittedloop} object produced by \code{\link{floop}}.
}
}
\details{
Internal computations are performed using the function \code{\link{deltamethod}} from the package msm.
}
\value{
  \item{SE}{a numeric vector of standard errors for ellipse parameters}
}
\references{
Yang, F. and A. Parkhurst, Estimating Elliptical Hysteresis: A Comparison of Analytic Methods. (submitted)
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}
\seealso{
\code{\link{floop}} fits points that form a hysteresis loop and creates an \code{fittedloop} object. Alternatively \code{\link{fel}} and \code{\link{summary.ellipsefit}} can be used to fit and bootstrap an ellipse.
}
\examples{
###Simulate and fit a hysteresis loop.
loop1 <- mloop(sd.x=0.5,sd.y=0.5,m=3,n=5)
model <- floop(loop1$x,loop1$y,m=3,n=5)
summary(model)
}
\keyword{ models }

