\name{mloop}
\alias{mloop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate a Hysteresis Loop
}
\description{
Simulate a hysteresis loop with a variety of possible parameters.
}
\usage{
mloop(cx = 0, cy = 0, retention = 1, b.x = 1, b.y = 1, m = 1, 
    n = 1, sd.x = 0, sd.y = 0, phase.angle = 0, n.points = 24, 
    period = 24)  
}

\arguments{
  \item{cx}{
centroid of input x.
}
  \item{cy}{
centroid of output y.
}
  \item{retention}{
half of the distance between the two possible values of the output when the input is at its central value.
}
  \item{b.x}{
saturation point x coordinate. The maximum value of the input.
}
  \item{b.y}{
saturation point y coordinate. The value of the output when the input is at its maximum.
}
  \item{m}{
a parameter of the hysteresis loop. Default is 1, which makes loop an ellipse when n is also equal to 1. See details.
}
  \item{n}{
a parameter of the hysteresis loop. Default is 1, which makes loop an ellipse when m is also equal to 1. See details.
}
  \item{sd.x}{
standard deviation of the normally distributed variation in the input vector x. }
  \item{sd.y}{
standard deviation of the normally distributed variation in the output vector y. }
  \item{phase.angle}{
defines the starting point of the loop.
} 
  \item{n.points}{
number of points on loop.
} 
  \item{period}{
number of equally spaced points required to make a full loop.
}
  \item{classical}{
fit a classical hysteresis loop. Use \deqn{y_t=sign(cos(t))*(a*sin(t)^m+b.y*abs(cos(t))^n)+e_{y,t}} instead of \deqn{y_t=a*sin(t)^m+b.y*cos(t)^n+e_{y,t}}. Allows the user to fit classical loops with any n>1 instead of just odd numbered n. Default is false.
}
}
\details{
Simulates input and output variables x and y that form a hysteresis loop of the form \deqn{x_t=b.x*cos(t)+e_{x,t}} \deqn{y_t=a*sin(t)^m+b.y*cos(t)^n+e_{y,t}} where \deqn{t=(1:n.points)/period*pi*2+phase.angle} and the error terms e are normally distributed. Also produces a vector of derived values.}
\value{
mloop returns an object of class \code{hysteresisloop}.
  \item{values}{estimated values of various inherent and derived parameters of the hysteresis loop. See \code{\link{loop.parameters}}}
  \item{x}{the input x.}
  \item{y}{the output y.}
}
\references{
Lapshin, R. (1995) Analytical model for the approximation of hysteresis loop and its application to the scanning tunneling microscope.
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}
\seealso{
Fit a hysteresis loop with the function \code{\link{floop}}.
}
\examples{
loop1 <- mloop(sd.x=0.5,sd.y=0.5,m=3,n=5)
floop(loop1$x,loop1$y,m=3,n=5)
}
\keyword{ models }

