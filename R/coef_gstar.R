coef.gstar <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  if (length(x$B) ){
    cat("Coefficients:\n")
    #colnames(x$B) <- "value"
    cname <- rownames(x$B)
    B <- c(x$B); names(B) <- cname
    print.default(format(B, digits = digits), print.gap = 2L,
                  quote = FALSE)
  } else cat("No coefficients\n")

}

