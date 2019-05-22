 print.gstar <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat(paste0("Model : GSTAR(",1,", ", x$p,", diff = ", x$d, ")\n\n"))
  if (length(x$B) ){
    cat("Coefficients:\n")
    #colnames(x$B) <- "value"
    cname <- rownames(x$B)
    B <- c(x$B); names(B) <- cname
    print.default(format(B, digits = digits), print.gap = 2L,
                  quote = FALSE)
  } else cat("No coefficients\n")

    cat("\nAIC = ", x$AIC)




#   MAPE_each = data.frame(x$MAPE_each)
#  cat("\nMSE for all data = ", x$MSE_total)
#  cat("\nMAPE for all data = ", x$MAPE_total)
#  cat("\n\nMSE for each location : \n\n")
#  print(x$MSE_each)
#  cat("\n\nMAPE for each location : \n\n")
#  print(x$MAPE_each)
  cat("\n")
  invisible(x)
}

