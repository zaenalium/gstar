#' @export plot.gstar

plot.gstar <- function(model, n_predict = NULL, testing  = NULL) {
      if(is.null(testing)) {
        if(is.null(n_predict)){
          all_df <-  rbind(as.data.frame(model$data),
                           as.data.frame(model$fitted_values))
          if(!is.null(model$Date)) {
          all_df$Date <- as.Date(row.names(all_df))
          } else {
            all_df$Date <- c(1:nrow(model$data), 1:nrow(model$fitted_values))
          }
          all_df$source <- c(rep("data", nrow(model$data)),
                             rep("fitted_values", nrow(model$fitted_values)))

          all_df <- reshape2::melt(all_df, id.vars = c("Date", "source"))

          ggplot(all_df, aes(x = Date, y = value, color = source)) +
            geom_line(aes(linetype = source)) + facet_wrap(.~variable)
        } else {
          forecast <- predict(model, n_predict)
          all_df <-  rbind(as.data.frame(model$data),
                               as.data.frame(model$fitted_values), as.data.frame(forecast))
          if(!is.null(model$Date)) {
            all_df$Date <- as.Date(row.names(all_df))
          } else {
          all_df$Date <- c(1:nrow(model$data), 1:nrow(model$fitted_values),
                                            (nrow(model$fitted_values) + 1):(nrow(model$fitted_values)+nrow(forecast)))
          }
          all_df$source <- c(rep("data", nrow(model$data)),
                             rep("fitted_values", nrow(model$fitted_values)),
                             rep("forecast", n_predict))
          all_df <- reshape2::melt(all_df, id.vars = c("Date", "source"))

          ggplot(all_df, aes(x = Date, y = value, color = source)) +
              geom_line(aes(linetype = source)) + facet_wrap(.~variable)
        }
      } else {
        if(ncol(testing) != ncol(model$data)) {
          stop("Number column testing and training data are not equal.\nPlease insert appropriate testing data!!")
        }
          forecast <- predict(model, nrow(testing))
          all_df <-  rbind(as.data.frame(model$data),
                           as.data.frame(model$fitted_values), as.data.frame(forecast),
                           as.data.frame(testing))
          all_df$source <- c(rep("data", nrow(model$data)),
                             rep("fitted_values", nrow(model$fitted_values)),
                             rep("forecast", nrow(forecast)),
                             rep("testing", nrow(testing)))
          if(!is.null(model$Date)) {
            all_df$Date <- as.Date(row.names(all_df))
          } else {
            all_df$Date <- c(1:nrow(model$data), 1:nrow(model$fitted_values),
                             (nrow(model$fitted_values) + 1):(nrow(model$fitted_values)+nrow(forecast)),
                             (nrow(model$fitted_values) + 1):(nrow(model$fitted_values)+nrow(testing)))
          }
          all_df <- reshape2::melt(all_df, id.vars = c("Date", "source"))

          ggplot(all_df, aes(x = Date, y = value, color = source)) +
            geom_line(aes(linetype = source)) + facet_wrap(.~variable)
        }
    }
