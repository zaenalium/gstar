#' Summarizing Generalized Space-Time Autoregressive Fits
#' @description This function are simmilar to summary of "lm" or "glm" object.
#' @param object an object of class "gstar".
#' @param ... further arguments passed to or from other methods.
#' \itemize{
#' \item coefficients - a named vector of coefficients.
#' \item AIC - A version of Akaike's An Information Criterion.
#' }
#' @export

summary.gstar <- function(object, ...) {
  digits = max(3L, getOption("digits") - 3L)
  dn <- c("Estimate", "Std.Err")
  coef.table <- cbind(object$B, object$std_err, object$t_value, object$p_value)
  dimnames(coef.table) <- list(rownames(object$B), c(dn,
                                                    "t value", "Pr(>|t|)"))
  cat("\nCoefficients: \n")

  printCoefmat(coef.table, digits = digits)

  cat("\n\nAIC:", format(signif(object$AIC, digits)))
  cat("\n")
}


