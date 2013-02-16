\name{fel}
\alias{fel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fitting Ellipses
}
\description{
Fit a sinusoidal hysteretic (elliptical) process between an input and an output over the course of one period.
}
\usage{
fel(x, y, method = "harmonic2", period = NULL, cycles = NULL,
  times="unknown",subset = NULL, control=nls.control(),na.action= getOption("na.action"))
}
\arguments{
  \item{x}{
input
}
  \item{y}{
output
}
  \item{method}{
the method to be used for fitting; one of either the default \code{method="harmonic2"}, \code{method="nls"}, \code{method="direct"}, or \code{method="lm"}.
}
  \item{period}{
an optional number that defines the length of the \code{period}.
}
  \item{cycles}{
an optional factor or list of factors, each of length \code{nrow(x)}. Use to identify several different ellipses to fit at once, in which case \code{fel} returns an object of class \code{ellipsefitlist} instead of \code{ellipsefit}.
}
  \item{times}{
either a numeric vector of length \code{nrow(x)} or one of the two options "equal" or the default "unknown". If the times at which ellipse observations are taken are known a numeric vector
can be used to give those times. If not, predicted values are found by minimizing geometric distances from the fitted ellipse to the observations. If "equal" time points are assumed to be equally spaced. Do not use the "harmonic2" method unless times are either known or are known to be equal. Bootstrapping results are also more accurate if correct times are used.}
  \item{subset}{
an optional vector specifying a subset of observations to be used in the fitting process.
}
  \item{control}{
optional and only used if \code{method="nls"}. See \code{\link{nls.control}}.
}
  \item{na.action}{
a function which indicates what should happen when the data contain NAs. The default is set by the \code{na.action} setting of \code{\link{options}}, the ‘factory-fresh’ default 
is \code{\link{na.omit}}. Value \code{link{na.exclude}} can be useful.
}
}
\details{
Where y is a sinusoidal process with an element of randomness that lags x, which is also a stochastic sinusoidal process, an ellipse can be used to fit the relationship between
x and y.

The values of derived parameters such as area, lag, retention and coercion are estimated from this ellipse.

The harmonic2 method is a two stage harmonic least squares modeling technique first described by Rostislav Lapshin.  It generally provides the least biased estimates 
for derived parameters and as such is used as the default. Direct specific least squares (\code{method="direct"}) based on the work of Radim Halir and Jan Flusser is another available methods. The use of \code{method="lm"} or 
ellipse specific non-linear least squares (\code{method="nls"}) is discouraged as both methods are included mainly for comparison and testing purposes only and internally use the R functions after which they are named.

If x and y contain more than 1 ellipse that needs to be fit the argument \code{cycles} can be used to seperate ellipses for fitting.

Bootstrapped estimates for parameter values are provided with \code{\link{summary.ellipsefit}} if \code{method="harmonic2"} or \code{method="nls"}.  These bootstrapped estimates are generally less biased than those provided by \code{fel} in isolation.
}
\value{
\code{fel} returns an object of class \code{ellipsefit} or \code{ellipsefitlist}.
  \item{method}{the \code{method} used.}
  \item{x}{the input x used.}
  \item{y}{the output y used.}
  \item{pred.x}{the fitted values for x.}
  \item{pred.y}{the fitted values for y.}
  \item{period.time}{a vector that contains times converted to radians for observations, either estimated after the ellipse has been fitted or given beforehand by \code{times}.}
  \item{coefficients}{a named vector of parameter estimates. The five or six parameters most applicable to the \code{method} used. See \code{\link{ellipse.parameters}}.}
  \item{derived.values}{a named vector of parameter estimates derived from the coefficients. See \code{\link{ellipse.parameters}}.}
  \item{fit.statistics}{a named vector of summary and fit statistics such as the AIC.}
  \item{residuals}{algebraic residuals from the model. The function \code{\link{residuals.ellipsefit}} can produce other types of residuals from an \code{ellipsefit} object.}
}
\references{
Yang, F. and A. Parkhurst, Estimating Elliptical Hysteresis: A Comparison of Analytic Methods. (submitted)
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}
\seealso{
\code{\link{plot.ellipsefit}} for plotting and \code{\link{summary.ellipsefit}} for summarizing and bootstrapping an \code{ellipsefit} object.  
}
\examples{
ellipse1 <- mel(sd.x=0.2,sd.y=0.04)
ellipse1.fit <- fel(ellipse1$x,ellipse1$y)
plot(ellipse1.fit,main="Simulated Ellipse", putTime=TRUE)
boot.ellipse1.fit <- summary(ellipse1.fit)
plot(boot.ellipse1.fit,xlab="Input",ylab="Output",main="Bootstrapped Ellipse", putTime=TRUE)
}
\keyword{ models }

