performance <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("\nMSE for all data = ", x$MSE_total)
  cat("\nMSE for each location : \n")
  print(x$MSE_each)
  cat("\nMAPE for all data = ", x$MAPE_total)
  cat("\nMAPE for each location : \n")
  print(x$MAPE_each)
  cat("\n")
}
