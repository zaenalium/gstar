#' Predicting the gstar object

#' @description Predicted values based on gstar object object
#' @param object an object of class "gstar".
#' @param n The number of steps ahead for which prediction is required.
#' @param ... further arguments passed to or from other methods.

#' @rdname predict.gstar
#' @export

predict.gstar <- function(object, n = NULL, ...){
  fitted_values <- as.data.frame(object$fitted_values)

  if(is.null(n)) {
    return(object$fitted_values)
  } else if(object$d == 0) {

    nr_ft_init <- nrow(fitted_values)
    nr_ft <- nr_ft_init
    for(i in 1:n){

      zt_pr <-  pad_zero(as.matrix(rbind((fitted_values[(nr_ft - object$p + 1)
                                                        :nr_ft , ]), 0)),
                         object$p)
      vt_pr <- pad_zero(as.matrix(rbind((fitted_values[(nr_ft - object$p + 1)
                                                       :nr_ft , ]), 0)) %*% as.matrix(object$W),
                        object$p)
      pr <- cbind(zt_pr, vt_pr) %*% object$B
      pr_mat <- matrix(pr, nrow = 1)
      fitted_values <- rbind(as.matrix(fitted_values), pr_mat)
      nr_ft <- nr_ft + 1

    }
    if(is.null(object$periodicity)){
      forecast <- fitted_values[(nr_ft_init + 1):nrow(fitted_values), ]
    } else  if (object$periodicity %in% c('minute','hourly') ) {
              stop("Currently gstar only support time series with date attributes 'daily','weekly', 'monthly','quarterly', and 'yearly'. \nPlease convert it first or remove the date attributes")
    } else {
      periodicity <- ifelse(object$periodicity %in% c('weekly', 'monthly','quarterly', 'year'),
                        gsub("ly$", "", object$periodicity),
                            ifelse(object$periodicity == 'daily', 'day', NULL))
      forecast <- fitted_values[(nr_ft_init + 1):nrow(fitted_values), ]
      Date <- seq(as.Date(object$Date[length(object$Date)]), by = periodicity, length.out = n + 1)
      forecast <- xts::xts(forecast, order.by = Date[-1])
    }

  } else {

    fitted_values_diff <- diff(as.matrix(fitted_values), differences = object$d)
    nr_ft_init <- nrow(fitted_values_diff)
    nr_ft <- nr_ft_init

    for(i in 1:n){
      # nc_ft <- ncol(fitted_values)

      zt_pr <-  pad_zero(as.matrix(rbind((fitted_values_diff[(nr_ft - object$p + 1)
                                                        :nr_ft , ]), 0)),
                         object$p)
      vt_pr <- pad_zero(as.matrix(rbind((fitted_values_diff[(nr_ft - object$p + 1)
                                                       :nr_ft , ]), 0)) %*% as.matrix(object$W),
                        object$p)
      pr <- cbind(zt_pr, vt_pr) %*% object$B
      pr_mat <- matrix(pr, nrow = 1) + fitted_values[nrow(fitted_values), ]
      fitted_values <- rbind(fitted_values, pr_mat)
      fitted_values_diff <-   diff(as.matrix(fitted_values), differences = object$d)
      nr_ft <- nr_ft + 1
    }

    if(is.null(object$periodicity)){
      forecast <- fitted_values[(nr_ft_init + 2):nrow(fitted_values), ]
    } else  if (object$periodicity %in% c('minute','hourly') ) {
      stop("Currently gstar only support time series with date attributes 'daily','weekly', 'monthly','quarterly', and 'yearly'. \nPlease use convert it first or remove the date attributes")
    } else {
      periodicity <- ifelse(object$periodicity %in% c('weekly', 'monthly','quarterly', 'year'),
                            gsub("ly$", "", object$periodicity),
                            ifelse(object$periodicity == 'daily', 'day', NULL))
      forecast <- fitted_values[(nr_ft_init + 2):nrow(fitted_values), ]
      Date <- seq(as.Date(object$Date[length(object$Date)]), by = periodicity, length.out = n + 1)
      forecast <- xts::xts(forecast, order.by = Date[-1])
    }
  }

#class(forecast) <- "predict.gstar"

forecast

}
