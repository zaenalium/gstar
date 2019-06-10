#' Plotting the gstar object
#'
#' @description plotting the gstar object
#' @param x an object of class "gstar".
#' @param n_predict The number of steps ahead for which prediction is required.
#' @param testing The outsample or testing data to be plotted.
#' @param ... Other arguments
#' @import ggplot2
#' @rdname plot.gstar
#' @export

plot.gstar <- function(x, n_predict = NULL, testing  = NULL, ...) {
      if(is.null(testing)) {
        if(is.null(n_predict)){
          all_df <-  rbind(as.data.frame(x$data),
                           as.data.frame(x$fitted_values))
          if(!is.null(x$Date)) {
          all_df$Date <- as.Date(row.names(all_df))
          } else {
            all_df$Date <- c(1:nrow(x$data), 1:nrow(x$fitted_values))
          }
          all_df$source <- c(rep("data", nrow(x$data)),
                             rep("fitted_values", nrow(x$fitted_values)))

          all_df <- reshape2::melt(all_df, id.vars = c("Date", "source"))

          ggplot(all_df, aes_string(x = "Date", y = "value", color = "source")) +
            geom_line(aes_string(linetype = "source")) + facet_wrap(.~variable)
        } else {
          forecast <- predict(x, n_predict)
          all_df <-  rbind(as.data.frame(x$data),
                               as.data.frame(x$fitted_values), as.data.frame(forecast))
          if(!is.null(x$Date)) {
            all_df$Date <- as.Date(row.names(all_df))
          } else {
          all_df$Date <- c(1:nrow(x$data), 1:nrow(x$fitted_values),
                                            (nrow(x$fitted_values) + 1):(nrow(x$fitted_values)+nrow(forecast)))
          }
          all_df$source <- c(rep("data", nrow(x$data)),
                             rep("fitted_values", nrow(x$fitted_values)),
                             rep("forecast", n_predict))
          all_df <- reshape2::melt(all_df, id.vars = c("Date", "source"))

          ggplot(all_df, aes_string(x = "Date", y = "value", color = "source")) +
              geom_line(aes_string(linetype = "source")) + facet_wrap(.~variable)
        }
      } else {
        if(ncol(testing) != ncol(x$data)) {
          stop("Number column testing and training data are not equal.\nPlease insert appropriate testing data!!")
        }
          forecast <- predict(x, nrow(testing))
          all_df <-  rbind(as.data.frame(x$data),
                           as.data.frame(x$fitted_values), as.data.frame(forecast),
                           as.data.frame(testing))
          all_df$source <- c(rep("data", nrow(x$data)),
                             rep("fitted_values", nrow(x$fitted_values)),
                             rep("forecast", nrow(forecast)),
                             rep("testing", nrow(testing)))
          if(!is.null(x$Date)) {
            all_df$Date <- as.Date(row.names(all_df))
          } else {
            all_df$Date <- c(1:nrow(x$data), 1:nrow(x$fitted_values),
                             (nrow(x$fitted_values) + 1):(nrow(x$fitted_values)+nrow(forecast)),
                             (nrow(x$fitted_values) + 1):(nrow(x$fitted_values)+nrow(testing)))
          }
          all_df <- reshape2::melt(all_df, id.vars = c("Date", "source"))

          ggplot(all_df, aes_string(x = "Date", y = "value", color = "source")) +
            geom_line(aes_string(linetype = "source")) + facet_wrap(.~variable)
        }
    }
