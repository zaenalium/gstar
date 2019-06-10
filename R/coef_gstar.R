#' @export

coef.gstar <- function(object, digits = max(3L, getOption("digits") - 3L), ...) {
  if (length(object$B) ){
    cat("Coefficients:\n")
    #colnames(x$B) <- "value"
    cname <- rownames(object$B)
    B <- c(object$B); names(B) <- cname
    print.default(format(B, digits = digits), print.gap = 2L,
                  quote = FALSE)
  } else cat("No coefficients\n")

}

