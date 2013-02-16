print.ellipsefit <- function(g,...) {
  cat("Call:\n")
  print(g$call)
  cat("\nCoefficients:\n")
print(g$coefficients,digits=4)
invisible(g)}
