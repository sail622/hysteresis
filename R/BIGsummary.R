BIGsummary <- function(g,...) {
  summ <- summary.ellipsefit(g,...)
  cat("Call:\n")
  print(summ$call)
  cat("Ellipse Fitting Method:\n")
  print(summ$method)
  cat("\nBootstrapped Coefficient Estimates:\n")
  print(summ$coefficients[,c("mean","Std. Error","q0.25","q0.75")],digits=4)
  cat("\nBootstrapped Derived Value Estimates:\n")
  print(summ$derived.values[,c("mean","Std. Error","q0.25","q0.75")],digits=4)
  cat("\nWald 95% Confidence Intervals:\n")
  Td <- qt(0.975,summ$fit.statistics["d.f."]) 
  low <- c(summ$coefficients[,"mean"],summ$derived.values[,"mean"])-Td*c(summ$coefficients[,"Std. Error"],summ$derived.values[,"Std. Error"])
  high <- c(summ$coefficients[,"mean"],summ$derived.values[,"mean"])+Td*c(summ$coefficients[,"Std. Error"],summ$derived.values[,"Std. Error"])
  names(low) <- c(row.names(summ$coefficients),row.names(summ$derived.values))
  print(cbind(low,high),digits=4)
  cat("\nDelta Method Standard Errors and 95% C.I.'s:\n")
  error <- delta.error(g)
  low2 <- c(g$coefficients,g$derived.values)-error*Td
  high2 <- c(g$coefficients,g$derived.values)+error*Td
  print(cbind("measured.values"=c(g$coefficients,
                g$derived.values),
              "S.E."=error,"low"=low2,"high"=high2),digits=4)
  cat("\nFit Statistics:\n")
  print(summ$fit.statistics,digits=4)
 invisible(list("summary"=summ,"delta.errors"=error,"C.I.boot"=cbind(low,high),"C.I.delta"=cbind(names(los),low2,high2))) 
}