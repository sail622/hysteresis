\name{hysteresis-package}
\alias{hysteresis-package}
\alias{hysteresis}
\docType{package}
\title{
Modeling Rate-Dependent Hysteretic Processes
}
\description{
Fit, summarize and plot sinusoidal hysteretic processes using two stage harmonic least squares. If the process is elliptical, other methods such as Halir and Flusser's direct specific least squares, ordinary least squares,
and ellipse-specific non-linear least squares are also available.
}
\details{
\tabular{ll}{
Package: \tab hysteresis\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-2-22\cr
License: \tab gpl (>= 1)\cr
}
Fits input and output variables x and y that form a hysteresis loop of the form \deqn{x_t=b.x*cos(t)+e_{x,t}} \deqn{y_t=a*sin(t)^m+b.y*cos(t)^n+e_{y,t}} where \deqn{t=(1:n.points)/period*pi*2+phase.angle}. The functions \code{\link{mloop}}, \code{\link{floop}} and the function method \code{\link{summary.fittedloop}} can be used to simulate, fit, obtain derived parameter estimates(see \code{\link{loop.parameters}}), and delta method standard errors for hysteresis loops with independent and normally distributed errors. If m=1 and n=1 then the hysteresis loop will form an ellipse which can be simulated with \code{\link{mel}}, fitted using 4 different available methods with \code{\link{fel}}, and bootstrapped using the function method \code{\link{summary.ellipsefit}}. Derived ellipse parameters (see \code{\link{ellipse.parameters}}) can therefore be obtained along with standard errors from models under any type of error structure that can be handled by bootstrapping.
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.

Maintainer: Spencer Maynes <smaynes89@gmail.com>
}
\references{
Yang, F. and A. Parkhurst, Estimating Elliptical Hysteresis: A Comparison of Analytic Methods. (submitted)
}
\keyword{ package }
\seealso{
Check out the vignette browseURL(system.file('doc/index.html',package='hysteresis')) \cr
For simulating hysteresis loops, \code{\link{mloop}} and \code{\link{mel}}. \cr
For fitting hysteresis loops, \code{\link{floop}} and \code{\link{fel}}. \cr
For summarizing hysteresis loops, \code{\link{summary.fittedloop}} and \code{\link{summary.ellipsefit}}. \cr
Miscellaneous \code{\link{BIGsummary}}, \code{\link{delta.error}}, \code{\link{plot.ellipsefit}}, \code{\link{plot.ellipsefitlist}}, \code{\link{plot.ellipsesummary}}, \code{\link{print.ellipsefit}}, \code{\link{residuals.ellipsesummary}}.
}
\examples{
###Take a look at the vignette.
browseURL(system.file('doc/index.html',package='hysteresis'))

###Simulate and fit a hysteresis loop.
loop1 <- mloop(sd.x=0.5,sd.y=0.5,m=3,n=5)
model <- floop(loop1$x,loop1$y,m=3,n=5)
summary(model)

###Simulate and fit an ellipse.
ellipse1 <- mel(sd.x=0.2,sd.y=0.04)
ellipse1.fit <- fel(ellipse1$x,ellipse1$y)
plot(ellipse1.fit,xlab="Input",ylab="Output",main="Simulated Ellipse", putTime=TRUE)
boot.ellipse1.fit <- summary(ellipse1.fit)
plot(boot.ellipse1.fit,xlab="Input",ylab="Output",main="Bootstrapped Ellipse", putTime=TRUE)
}
