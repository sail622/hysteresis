\name{ellipse.parameters}
\alias{ellipse.parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inherent and Derived Parameter Definitions for \code{ellipsefit} Models
}
\description{
\code{\link{fel}} returns an \code{ellipsefit} object and calculates a variety of ellipse parameters.This is a list of definitions of parameters.
}

\value{
Coefficients and Derived Parameters returned by using the function \code{\link{fel}} to fit elliptical data.
  \item{cx}{
centroid of input x.
}
  \item{cy}{
centroid of output y.
}
   \item{theta}{
angle of rotation. The angle in radians between the x-axis and the major axis \code{lambda1} of the ellipse. A coefficient for all methods except 'harmonic2'.
}
\item{rotated.angle}{
angle of rotation. The angle in degrees between the x-axis and the major axis \code{lambda1} of the ellipse. A coefficient for every method except 'harmonic2'.
}
  \item{lambda1}{
length of major (longer) axis. A coefficient for every model except 'harmonic2'.The longer red line found in the demo output below.
}
  \item{lambda2}{
length of minor (shorter) axis. A coefficient for every method except 'harmonic2'.The shorter red line found in the demo output below.
}
   \item{p.a.}{
used in internal calculations to transfer between different sets of parameters. The distance in radians between the saturation point and the intersection of the major axis.
}
  \item{b.x}{
saturation point x coordinate. The maximum value of the input. A coefficient for \code{method='harmonic2'}. Represented by the blue line in the demo output below.
}
  \item{b.y}{
saturation point y coordinate. The value of the output when the input is at its maximum. A coefficient for \code{method='harmonic2'}. Represented by the blue line in the demo output below.
}
  \item{phase.angle}{
defines the starting point of the ellipse. Only calculated using the 'harmonic2' method.
}
  \item{ampx}{
The range of the ellipse input values divided by 2. Used in some of the calculations performed for the 'nls' method.
}
  \item{ampy}{
The range of the ellipse output values divided by 2. Used in some of the calculations performed for the 'nls' method.
}
  \item{area}{
The area of the ellipse.
}
  \item{lag}{
The number of points between the location where the input reaches its maximum value and where the output reaches its maximum value. Lag is therefore dependent on the user given value for period, which has as a default the number of points used to fit the ellipse. 
}
  \item{retention}{
Half of the distance between the two possible values of the output when the input is at its central value. The purple line in the demo below.
}
  \item{coercion}{
Half of the distance between the two possible values of the input when the output is at its central value. The yellow line in the demo below.
}
}
\references{
Yang, F. and A. Parkhurst, Estimating Elliptical Hysteresis: A Comparison of Analytic Methods. (submitted)
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}
\seealso{
\code{\link{fel}} for fitting points that form an ellipse and creating an \code{ellipsefit} object, \code{\link{plot.ellipsefit}}for plotting an \code{ellipsefit} object.
\code{\link{summary.ellipsefit}} for summarizing an \code{ellipsefit} object, and \code{\link{plot.ellipsesummary}}for plotting an \code{ellipsesummary} object.  
}
\examples{
ellipse1 <- mel(lambda1=7,lambda2=4,rotated.angle=30)
ellipse1.fit <- fel(ellipse1$x,ellipse1$y)
plot(ellipse1.fit,show=c("Retention","Coercion","Lambdas","Bs"),main="Ellipse Parameters")
ellipse1.fit$coefficients
ellipse1.fit$derived.values
}
\keyword{ models }
