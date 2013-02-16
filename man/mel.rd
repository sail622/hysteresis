\name{mel}
\alias{mel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make an Ellipse
}
\description{
Produces an ellipse based on 1 of 4 possible formulations.
}
\usage{
mel(cx=32,cy=39,rotated.angle=2,lambda1=7,lambda2=0.23,n.points=24,period=24,sd.x=0,sd.y=0)
mel(method=2,cx=32,cy=39,b.x=6.99,b.y=0.244,retention=0.23,phase.angle=1.57,n.points=24,period=24,sd.x=0,sd.y=0)
mel(method=3,cx=32,cy=39,ampx=6.99,ampy=0.335,lag=2.888,n.points=24,period=24,sd.x=0,sd.y=0)
mel(x2=0.002293,xy=-.06960,y2=0.9976,x=2.567,y=-75.58,int=1432.7,
                 phase.angle=0,n.points=24,period=24,sd.x=0,sd.y=0)
}

\arguments{
  \item{method}{
One of 1,2,3 or 4. If \code{method=1}; \code{lambda1}, \code{lambda2} and \code{rotated.angle} are the elliptical parameters used. If \code{method=2} \code{b.x}, \code{b.y} and \code{retention} are used instead.
 The third option \code{method=3} uses the easiest to understand parameters in \code{ampx}, \code{ampy}, and \code{lag}. If \code{method=4} coefficients to the variables \code{x2} x^2, \code{xy} x*y, \code{y2} y^2, \code{x}, \code{y} and a vector of 1's \code{int} are used. 
 Default is \code{method=1}.
}
  \item{cx}{
centroid of input x.
}
  \item{cy}{
centroid of output y.
}
  \item{phase.angle}{
defines the starting point of the ellipse. Does not change ellipse shape.
}
  \item{rotated.angle}{
angle of rotation. In degrees. Only used if \code{method=1}.
}
  \item{lambda1}{
length of major axis. Only used if \code{method=1}.
}
  \item{lambda2}{
length of minor axis. Only used if \code{method=1}.
}
  \item{b.x}{
saturation point x coordinate. Only used if \code{method=2}.
}
  \item{b.y}{
saturation point y coordinate. Only used if \code{method=2}.
}
  \item{retention}{
another ellipse parameter used if \code{method=2}. Half of the distance between the two possible values of the output when the input is at its central value.
}
  \item{ampx}{
The range of the ellipse input values divided by 2. Only used if \code{method=3}.
}
  \item{ampy}{
The range of the ellipse output values divided by 2. Only used if \code{method=3}.
}
  \item{lag}{
The number of points between the location where the input reaches its maximum value and where the output reaches its maximum value.
 Lag is therefore dependent on the value chosen for period. Only used if \code{method=3}. 
}
  \item{n.points}{
number of points on ellipse. Equally spaced by circumference of ellipse/period.
}
  \item{period}{
number of points required to make a full loop around the ellipse.
}
  \item{sd.x}{
optional number specifying a normally distributed standard deviation for x.
}
  \item{sd.y}{
optional number specifying a normally distributed standard deviation for y.
}
}
\details{
All of the three methods can be used to specify a series of points that make up an ellipse. The function \code{mel}
uses parameters to form an ellipse and find derived variables such as area, lag, retention, and coercion. 
Optionally, normally distributed random variation can be introduced in both the x and y directions. The first method is useful alongside the nls, lm and direct fitting methods,
while the second is comparable to the harmonic2 ellipse fitting method. The third method for mel is included even though there is no similar ellipse fitting method because it is the easiest to use due to the easy interpretability of its parameters.}
\value{
mel returns an object of class \code{ellipsemake}.
  \item{values}{the nine fundamental parameters (cx,cy,rotated.angle,lambda1,lambda2,b.x,b.y,a,phase.angle) of which only four or five are used 
  along with the four derived parameters (area, lag, retention, coercion).}
  \item{method}{the method used.}
  \item{x}{the input x.}
  \item{y}{the output y.}
}
\references{
Yang, F. and A. Parkhurst, Estimating Elliptical Hysteresis: A Comparison of Analytic Methods. (submitted)
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}
\seealso{
\code{\link{fel}} for fitting observations that form an ellipse and creating an \code{ellipsefit} object, \code{\link{plot.ellipsefit}}for plotting an \code{ellipsefit} object.
\code{\link{summary.ellipsefit}} for summarizing an \code{ellipsefit} object, and \code{\link{plot.ellipsesummary}} for plotting an \code{ellipsesummary} object.  
}
\examples{
ellipse1 <- mel(sd.x=0.2,sd.y=0.04)
ellipse1.fit <- fel(ellipse1$x,ellipse1$y)
plot(ellipse1.fit,xlab="Input",ylab="Output",main="Simulated Ellipse", putTime=TRUE)
boot.ellipse1.fit <- summary(ellipse1.fit)
plot(boot.ellipse1.fit,xlab="Input",ylab="Output",main="Bootstrapped Ellipse", putTime=TRUE)
}
\keyword{ models }

