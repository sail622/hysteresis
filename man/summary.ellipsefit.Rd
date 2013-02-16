\name{summary.ellipsefit}
\alias{summary.ellipsefit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizing Fitted Ellipses
}
\description{
summary method for class \code{ellipsefit} created by the function \code{\link{fel}}. Can bootstrap \code{\link{fel}} results to produce parameter estimates with reduced bias and standard errors. 
}
\usage{
## S3 method for class 'ellipsefit'
summary.ellipsefit(g, N = 1000, ...)
}
\arguments{
  \item{g}{
an object of class \code{ellipsefit} usually a result of a call to \code{\link{fel}}.
}
  \item{N}{
optional number of bootstrap replicates. Default of 1000.
}
  \item{studentize}{
studentize the residuals to improve performance. Default is true.
}
  \item{center}{
center x and y residuals around zero. Default is false. Irrelevant for "harmonic2" method.
}
  \item{cbb}{
allows for circular block bootstrapping. The default is NULL in which case circular block bootstrapping is not performed. If cbb is an integer greater than 1 which is a divisor of either the number of observations for methods "nls", "lm", or "nls" or the number of observations minus 3 for method=harmonic2 then it is used as the block size for circular block bootstrapping.  
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{
Bootstrap ellipsefit objects created by fitting elliptical data with the function \code{\link{fel}} and produce statistical summaries. Bootstrapping reduces the bias on estimates and also gives standard errors. Bootstrap estimates are created by subtracting original estimates from bootstrap means to get a bias estimate, and then subtracting this bias from the original estimate.

Residuals are studentized as if they were produced using the harmonic2 method, regardless of which method was actually used to produce them. However, unpublished simulation studies show that these studentized residuals provide better 95 percent coverages for all methods despite this.

If residuals are serially correlated than the argument cbb may be used to sample blocks of length cbb instead of individual residuals. Circular block bootstrapping is used, which means that all residuals are equally likely to be included and blocks can be made up of the last points on the ellipse together with the first.

When using the 'nls' or 'lm' methods individual bootstrap replications may occasionally fail to converge, when this occurs an extra replication will take the place of the one that failed to converge and a warning message will be produced.
}
\value{
\item{boot.data}{parameter estimates from individual bootstrap replications.}
\item{call}{function call.}
  \item{coefficients }{matrix containing parameter and standard error estimates, bootstrap quantiles, and bootstrapped parameter estimates for a variety of parameters that depend on method used. See \code{\link{ellipse.parameters}}.}
    \item{derived.values }{matrix containing parameter and standard error estimates, bootstrap quantiles, and bootstrapped parameter estimates for a variety of parameters that depend on method used. See \code{\link{ellipse.parameters}}.}
  \item{fit.statistics}{fit statistics from the original fit.}
  \item{method}{fitting method used.}
  \item{pred.x }{the bootstrap fitted values for x.}
  \item{pred.y }{the bootstrap fitted values for y.}
  \item{x }{the input x.}
  \item{y}{the input y.}
}
\references{
Yang, F. and A. Parkhurst, Estimating Elliptical Hysteresis: A Comparison of Analytic Methods. (submitted)
}
\author{
Spencer Maynes, Fan Yang, and Anne Parkhurst.
}
\seealso{
\code{\link{fel}} for fitting points that form an ellipse and creating an ellipsefit object and \code{\link{plot.ellipsesummary}}for plotting an ellipsesummary object.  
}
\examples{
ellipse1 <- mel(sd.x=0.2,sd.y=0.04)
ellipse1.fit <- fel(ellipse1$x,ellipse1$y)
plot(ellipse1.fit,xlab="Input",ylab="Output",main="Simulated Ellipse", putTime=TRUE)
boot.ellipse1.fit <- summary(ellipse1.fit)
plot(boot.ellipse1.fit,xlab="Input",ylab="Output",main="Bootstrapped Ellipse", putTime=TRUE)
}
\keyword{ models }

