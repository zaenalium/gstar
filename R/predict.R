predict.gstar <- function(model, n = NULL){
  fitted_values <- as.data.frame(model$fitted_values)
 # fitted_values$Date <- NULL
  #fitted_values <- as.matrix(fitted_values)
  if(is.null(n)) {
    return(model$fitted_values)
  } else if(model$d == 0) {

    nr_ft_init <- nrow(fitted_values)
    nr_ft <- nr_ft_init
    for(i in 1:n){
     # nc_ft <- ncol(fitted_values)

      zt_pr <-  pad_zero(as.matrix(rbind((fitted_values[(nr_ft - model$p + 1)
                                                        :nr_ft , ]), 0)),
                         model$p)
      vt_pr <- pad_zero(as.matrix(rbind((fitted_values[(nr_ft - model$p + 1)
                                                       :nr_ft , ]), 0)) %*% as.matrix(model$W),
                        model$p)
      pr <- cbind(zt_pr, vt_pr) %*% model$B
      pr_mat <- matrix(pr, nrow = 1)
      fitted_values <- rbind(as.matrix(fitted_values), pr_mat)
      nr_ft <- nr_ft + 1

    }
    if(is.null(model$periodicity)){
      forecast <- fitted_values[(nr_ft_init + 1):nrow(fitted_values), ]
    } else  if (model$periodicity %in% c('minute','hourly') ) {
              stop("Currently gstar only support time series with date attributes ‘daily’,‘weekly’, ‘monthly’,‘quarterly’, and ‘yearly’. \nPlease use convert it first or remove the date attributes")
    } else {
      periodicity <- ifelse(model$periodicity %in% c('weekly', 'monthly','quarterly', 'year'),
                        gsub("ly$", "", model$periodicity),
                            ifelse(model$periodicity == 'daily', 'day', NULL))
      forecast <- fitted_values[(nr_ft_init + 1):nrow(fitted_values), ]
      Date <- seq(as.Date(model$Date[length(model$Date)]), by = periodicity, length.out = n + 1)
      forecast <- xts::xts(forecast, order.by = Date[-1])
    }

  } else {

    fitted_values_diff <- diff(as.matrix(fitted_values), differences = model$d)
    nr_ft_init <- nrow(fitted_values_diff)
    nr_ft <- nr_ft_init

    for(i in 1:n){
      # nc_ft <- ncol(fitted_values)

      zt_pr <-  pad_zero(as.matrix(rbind((fitted_values_diff[(nr_ft - model$p + 1)
                                                        :nr_ft , ]), 0)),
                         model$p)
      vt_pr <- pad_zero(as.matrix(rbind((fitted_values_diff[(nr_ft - model$p + 1)
                                                       :nr_ft , ]), 0)) %*% as.matrix(model$W),
                        model$p)
      pr <- cbind(zt_pr, vt_pr) %*% model$B
      pr_mat <- matrix(pr, nrow = 1) + fitted_values[nrow(fitted_values), ]
      fitted_values <- rbind(fitted_values, pr_mat)
      fitted_values_diff <-   diff(as.matrix(fitted_values), differences = model$d)
      nr_ft <- nr_ft + 1
    }

    if(is.null(model$periodicity)){
      forecast <- fitted_values[(nr_ft_init + 2):nrow(fitted_values), ]
    } else  if (model$periodicity %in% c('minute','hourly') ) {
      stop("Currently gstar only support time series with date attributes ‘daily’,‘weekly’, ‘monthly’,‘quarterly’, and ‘yearly’. \nPlease use convert it first or remove the date attributes")
    } else {
      periodicity <- ifelse(model$periodicity %in% c('weekly', 'monthly','quarterly', 'year'),
                            gsub("ly$", "", model$periodicity),
                            ifelse(model$periodicity == 'daily', 'day', NULL))
      forecast <- fitted_values[(nr_ft_init + 2):nrow(fitted_values), ]
      Date <- seq(as.Date(model$Date[length(model$Date)]), by = periodicity, length.out = n + 1)
      forecast <- xts::xts(forecast, order.by = Date[-1])
    }

      #} else {
    #  if (periodicity %in% c('minute','hourly') )
    #    stop("Currently gstar only support time series with date attributes ‘daily’,‘weekly’, ‘monthly’,‘quarterly’, and ‘yearly’. \nPlease use convert it first or remove the date attributes")

    #  periodicity <- ifelse(model$periodicity %in% c('weekly', 'monthly','quarterly', 'year'),
    #                        ifelse(model$periodicity == 'daily', 'day', NULL))
    #  forecast <- fitted_values[(nr_ft_init + 2):nrow(fitted_values), ]
    #  Date <- seq(model$Date[length(model$Date)], periodicity, length.out = n)
    #  forecast <- cbind(Date, forecast)
    #}
    #forecast <- fitted_values_diff[(nr_ft_init + 1):nrow(fitted_values_diff), ]
    #forecast[1, ] <- forecast[1, ] + fitted_values[nrow(fitted_values), ]
    #forecast <- apply(forecast, 2, cumsum) %>% as.data.frame()

    #forecast <- fitted_values[(nr_ft_init + 2):nrow(fitted_values), ]
    #forecast$Date <- dt[length(dt)] + 1:n
    #forecast <- select(forecast, Date, everything())
  }

forecast

}
