summary.ellipsefitlist <-
function(g,N=1000,...) {
res <- mapply(summary.ellipsefit,g,MoreArgs=list(N=N,...),SIMPLIFY=FALSE)
  class(res) <- "ellipsesummarylist"
  res
  }
