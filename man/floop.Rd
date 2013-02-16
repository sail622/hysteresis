\name{floop}
\alias{floop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit a Hysteresis Loop
}
\description{
Fits a hysteresis loop given values of m and n chosen by the user.
}
\usage{
floop(x, y, m = 1, n = 1, period = 24,cycles=NULL,times="even",subset=NULL,na.action= getOption("na.action"),classical=FALSE) 
}

\arguments{
  \item{x}{
numeric input vector.
}
  \item{y}{
numeric output vector.
}
  \item{m}{
a parameter of the hysteresis loop. Default is 1, which makes loop an ellipse when n is also equal to 1. See details.
}
  \item{n}{
a parameter of the hysteresis loop. Default is 1, which makes loop an ellipse when m is also equal to 1. See details.
}
  \item{period}{
number of equally spaced points required to make a full loop.Used if times="equal".
}
  \item{cycles}{
an optional factor or list of factors, each of length \code{nrow(x)}. Use to identify several different ellipses to fit at once, in which case \code{fel} returns an object of class \code{ellipsefitlist} instead of \code{ellipsefit}.
}
  \item{times}{
either a numeric vector of length nrow(x) or the default "equal". If the times at which ellipse observations are taken are known a numeric vector can be used to give those times. If the default "equal" is used instead time points are assumed to be equally spaced.
}
  \item{subset}{
an optional vector specifying a subset of observations to be used in the fitting process.
}
  \item{na.action}{
a function which indicates what should happen when the data contain NAs. The default is set by the \code{na.action} setting of \code{\link{options}}, the ‘factory-fresh’ default 
is \code{\link{na.omit}}. Value \code{link{na.exclude}} can be useful.
}
  \item{classical}{
fit a classical hysteresis loop. Use \deqn{y_t=sign(cos(t))*(a*sin(t)^m+b.y*abs(cos(t))^n)+e_{y,t}} instead of \deqn{y_t=a*sin(t)^m+b.y*cos(t)^n+e_{y,t}}. Allows the user to fit classical loops with any n>1 instead of just odd numbered n. Default is false.
}
}
\details{
Fits input and output variables x and y that form a hysteresis loop of the form \deqn{x_t=b.x*cos(t)+e_{x,t}} \deqn{y_t=a*sin(t)^m+b.y*cos(t)^n+e_{y,t}} where \deqn{t=(1:n.points)/period*pi*2+phase.angle} and the error terms e are normally distributed. Also produces a vector of derived values.}
\value{
floop returns an object of class \code{fittedloop}.
  \item{values}{estimated values of various inherent and derived parameters of the hysteresis loop. See \code{\link{loop.parameters}}}
  \item{x}{the input x.}
  \item{y}{the output y.}
  \item{pred.x}{fitted x values.}
  \item{pred.y}{fitted y values.}
  \item{period.time}{time vector used to fit x and y.}
  \item{residuals}{residuals measured by euclidean distance.}
}
\references{
Lapshin, R. (1995) Analytical model for the approximation of hysteresis loop and its application to the scanning tunneling microscope.
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}
\seealso{
Simulate a hysteresis loop with the function \code{\link{mloop}}. Alternatively see \code{\link{fel}} for fitting an ellipse (a hysteresis loop with parameters m=1, n=1) using a variety of methods. If a loop is an ellipse, use of floop is strongly discouraged in favor of fel. 
}
\examples{
loop1 <- mloop(sd.x=0.5,sd.y=0.5,m=3,n=5)
model <- floop(loop1$x,loop1$y,m=3,n=5)
summary(model)
}
\keyword{ models }

