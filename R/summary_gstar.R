#' Summarizing Generalized Space-Time Autoregressive Fits
#' @description This function are simmilar with summary to "lm" or "glm" object.
#' \itemize{
#' \item coefficients - a named vector of coefficients.
#' \item AIC - A version of Akaike's An Information Criterion.
#' }
#' @export summary.gstar

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


