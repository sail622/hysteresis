print.ellipsesummary <-
function(g,...) {
  cat("Call:\n")
  print(g$call)
  cat("Ellipse Fitting Method:\n")
  print(g$method)
  cat("\nBootstrapped Coefficient Estimates:\n")
  print(g$coefficients[,c("Estimate","Bias","Std. Error","q0.25","q0.75")],digits=4)
  cat("\nBootstrapped Derived Value Estimates:\n")
  if (g$method=="harmonic2")
  print(g$derived.values[c("ampx","ampy","area","lag","coercion"),c("Estimate","Bias","Std. Error","q0.25","q0.75")],digits=4)
  else
    print(g$derived.values[c("ampx","ampy","area","lag","retention","coercion"),c("Estimate","Bias","Std. Error","q0.25","q0.75")],digits=4)
  
  cat("\nFit Statistics:\n")
  print(g$fit.statistics,digits=4)
invisible(g)}

