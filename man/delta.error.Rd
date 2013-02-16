\name{delta.error}
\alias{delta.error}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard Errors for \code{ellipsefit} Parameters
}
\description{
Obtain delta method standard errors for parameters from an \code{ellipsefit} object with \code{method="lm"}, \code{method="harmonic2"} or \code{method="nls"}. Bootstrap standard errors obtained through \code{\link{summary.ellipsefit}} are generally superior however.
}
\usage{
delta.error(g)
}
\arguments{
  \item{g}{
an \code{ellipsefit} object.
}
}
\details{
Internal computations are performed using the function \code{\link{deltaMethod}} from the package car.
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
\code{\link{fel}} fits points that form an ellipse and creates an \code{ellipsefit} object using one of the four methods available, \code{\link{summary.ellipsefit}} summarizes and produces bootstrap estimates for an 
\code{ellipsefit} object, and \code{\link{plot.ellipsefit}} plots an \code{ellipsefit} object. 
}
\examples{
ellipse1 <- mel(sd.x=0.2,sd.y=0.04)
ellipse1.fit <- fel(ellipse1$x,ellipse1$y,method="nls")
delta.error(ellipse1.fit)
}
\keyword{ models }

