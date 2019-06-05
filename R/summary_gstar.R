summary.gstar <- function(model, digits = max(3L, getOption("digits") - 3L), ...) {
  dn <- c("Estimate", "Std.Err")
  coef.table <- cbind(model$B, model$std_err, model$t_value, model$p_value)
  dimnames(coef.table) <- list(rownames(model$B), c(dn,
                                                    "t value", "Pr(>|t|)"))
  cat("\nCoefficients: \n")

  printCoefmat(coef.table, digits = digits)

  cat("\n\nAIC:", format(signif(model$AIC, digits)))
  cat("\n")
}


