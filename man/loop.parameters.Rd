\name{loop.parameters}
\alias{loop.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inherent and Derived Parameter Definitions for Hysteresis Loops
}
\description{
\code{\link{floop}} returns a \code{fittedloop} object and calculates a variety of hysteresis loop parameters.This is a list of definitions for these parameters.
}

\value{
Coefficients and derived parameters returned by using the function \code{\link{floop}} to fit hysteretic data.
  \item{cx}{
centroid of input x.
}
  \item{cy}{
centroid of output y.
}
  \item{b.x}{
saturation point x coordinate. The maximum value of the input.
}
  \item{b.y}{
saturation point y coordinate. The value of the output when the input is at its maximum.
}
  \item{phase.angle}{
defines the starting point of the loop.
}

  \item{area}{
the area of the hysteresis loop.
}
  \item{retention}{
half of the distance between the two possible values of the output when the input is at its central value.
}
  \item{hysteresis}{
hysteresis along the y axis.
}
}
\references{
Lapshin, R. (1995) Analytical model for the approximation of hysteresis loop and its application to the scanning tunneling microscope.
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}
\seealso{
\code{\link{mloop}} for simulating a hysteresis loop and \code{\link{floop}} for fitting a hysteresis loop.   
}
\examples{
theloop<-mloop(sd.x=0.05,sd.y=0.05,m=3,n=5)
loopmodel<-floop(theloop$x,theloop$y,m=3,n=5)
loopmodel$values
}
\keyword{ models }
