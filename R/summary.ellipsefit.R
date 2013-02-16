summary.ellipsefit <-
function(g,N=1000,mc=1,studentize=TRUE,center=FALSE,cbb=NULL,...) {
if (g$method=="harmonic2")
  {res <- harmonic2summary(g,N=N,mc=mc,studentize=studentize,cbb=cbb)}
else if (g$method=="lm")
{res <- lmsummary(g,N=N,mc=mc,studentize=studentize,center=center,cbb=cbb)}
else if (g$method=="direct")
{res <- directsummary(g,N=N,mc=mc,studentize=studentize,center=center,cbb=cbb)}
  else {res <- nlssummary(g,N=N,mc=mc,studentize=studentize,center=center,cbb=cbb,...)}
  class(res) <- "ellipsesummary"
  res
  }
