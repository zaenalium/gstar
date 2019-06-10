#' Calculate performance of prediction or forecasting
#' @param object an object of class "gstar".
#' @param digits how many digit of the number output
#' @param testing a dataframe or matrix or xts object that contain testing data (outsample data). \strong{Please be noted, if you use differencing in the model estimation, you do not need difference your data because we already cover that in this function}
#' @param ... Other arguments
#' @return
#' \itemize{
#' \item MSE fol all data - Mean Square Error for all the data combined
#' \item MSE fol each location - Mean Square Error for each spatial location
#' \item MAPE fol all data - Mean Absolute Percentage Error for all the data combined
#' \item MAPE fol each location - Mean Absolute Percentage Error for each spatial location }
#' @export


performance <- function(object, digits = max(3L, getOption("digits") - 3L), testing = NULL ,...) {
  if(is.null(testing)){
    cat('----------Performance training------------\n')
     cat("\nMSE for all data = ", object$MSE_total)
    cat("\nMSE for each location : \n")
    print(object$MSE_each)
    cat("\nMAPE for all data = ", object$MAPE_total)
    cat("\nMAPE for each location : \n")
    print(object$MAPE_each)
    cat("\n")
  } else {
    if(ncol(testing) != ncol(object$data)) {
      "Number column testing and training data are not equal.\nPlease insert appropriate testing data!!"
    }

    cat('----------Performance training------------\n')
    cat("\nMSE for all data = ", object$MSE_total)
    cat("\nMSE for each location : \n")
    print(MSE_each)
    cat("\nMAPE for all data = ", object$MAPE_total)
    cat("\nMAPE for each location : \n")
    print(object$MAPE_each)
    cat("\n")



    fitted_values <- predict(object, nrow(testing))
    z_mat <-  as.matrix(testing)
    MSE_total <- mean((z_mat - fitted_values)^2)
    MAPE_total <- mean(abs((c(as.matrix(z_mat - fitted_values)))/
                             c(as.matrix(z_mat)))) * 100
    MSE_each <- apply(z_mat - fitted_values, 2, function(x) mean(x^2))
    MAPE_each <- apply(abs((z_mat - fitted_values) / z_mat),
                       2, function(x) mean(100*x))

    cat('----------Performance testing------------\n')
    cat("\nMSE for all data = ", MSE_total)
    cat("\nMSE for each location : \n")
    print(MSE_each)
    cat("\nMAPE for all data = ", MAPE_total)
    cat("\nMAPE for each location : \n")
    print(MAPE_each)
  }
}
