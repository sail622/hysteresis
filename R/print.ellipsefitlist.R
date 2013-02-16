print.ellipsefitlist <- function(g,...) {
  cat("Call:\n")
  print(attr(g,"call"))
  n <- names(g)
    printcoef <- function(g,n) {
      cat("\nCycle ")
      print(n)
  cat("\nCoefficients:\n")
print(g$coefficients,digits=4)}
mapply(printcoef,g,n)
invisible(g)}
