\name{residuals.ellipsefit}
\alias{residuals.ellipsesummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract \code{ellipsefit} or \code{ellipsesummary} Model Residuals.
}
\description{
Four types of residuals can be extracted from an \code{ellipsefit} or \code{ellipsesummary} object; input, output, algebraic and geometric.
}
\usage{
residuals(g)
}
\arguments{
  \item{g}{
an \code{ellipsefit} or \code{ellipsesummary} object.
}
}
\details{
Geometric residuals are based on the straight line distance between predicted and true values along an x,y cartesian plane, and algebraic residuals are based on the method used to calculate the ellipsefit object. If method="harmonic2" then there are no algebraic residuals, and residuals.ellipsefit replaces these with the geometric residuals
}
\value{
  \item{input}{a numeric vector. See details.}
  \item{output}{a numeric vector. See details.}
  \item{geometric}{a numeric vector. See details.}
  \item{algebraic}{a numeric vector. See details.}
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
ellipse1.fit <- fel(ellipse1$x,ellipse1$y)
plot(ellipse1.fit,main="Simulated Ellipse", putTime=TRUE)
boot.ellipse1.fit <- summary(ellipse1.fit)
plot(boot.ellipse1.fit,xlab="Input",ylab="Output",main="Bootstrapped Ellipse", putTime=TRUE)
}
\keyword{ models }

