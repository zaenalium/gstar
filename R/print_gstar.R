print.gstar <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat(paste0("Model : GSTAR(",x$p,", ", x$q,")\n\n"))
  if (length(x$B) ){
    cat("Coefficients:\n")
    colnames(x$B) <- "value"
    print.default(format(x$B, digits = digits), print.gap = 2L,
                  quote = FALSE)
  } else cat("No coefficients\n")
  cat("\nMSE for all data = ", x$MSE_total)
  cat("\nMAPE for all data = ", x$MAPE_total)
  cat("\nMSE for each location = ", x$MSE_each)
  cat("\nMAPE for each location = ", x$MAPE_each)
  cat("\n")
  invisible(x)
}

