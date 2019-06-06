gstar_est <- function(x, W, p,d, dt, freq){

  x_base <- as.data.frame(x)

  if(d > 0) {
    x <- diff(x, lag = d)
    #dt <- dt[-seq(d)]
  }

  sp_loc <- colnames(x)


  xt <- pad_zero(x, p)
  w_xt <- pad_zero(x %*% W, p)
  z <- matrix(x[-seq(p),], ncol = 1)
  Xv <- cbind(xt, w_xt)
  B <- solve(t(Xv) %*% Xv) %*% t(Xv) %*% z

  z_hat <- Xv %*% B

  if(d == 0) {
    fitted_values <- matrix(z_hat, ncol = ncol(x) )
    z_mat <-  matrix(z, ncol = ncol(x) )
    MSE_total <- mean((z - z_hat)^2)
    MAPE_total <- mean(abs((z - z_hat)/z)) * 100
    MSE_each <- apply(z_mat - fitted_values, 2, function(x) mean(x^2))
    MAPE_each <- apply(abs((z_mat - fitted_values) / z_mat),
                      2, function(x) mean(100*x))
  } else {
    fitted_values <- matrix(z_hat, ncol = ncol(x_base) ) + as.matrix(x_base[(p + d + 1):(nrow(x_base)),])
    z_mat <-  matrix(z, ncol = ncol(x_base) )  + as.matrix(x_base[(p + d + 1):(nrow(x_base)),])
    MSE_total <- mean((fitted_values- z_mat)^2)
    MAPE_total <- mean(abs((c(z_mat) - c(fitted_values))/c(z_mat))) * 100
    MSE_each <- apply(z_mat- fitted_values, 2, function(x) mean(x^2))
    MAPE_each <- apply(abs((z_mat - fitted_values) / z_mat),
                      2, function(x) mean(100*x))
  }


  sse <- sum((z - z_hat)^2)#/ (nrow(z)- ncol(B) - 1)
  std_err <- sse * diag(solve(t(Xv) %*% Xv))
  t_result <- B/std_err
  p_value <-  2 * pt(abs(t_result), (nrow(z)- ncol(B) - 1),
                     lower.tail = FALSE)
  AIC <- nrow(z) * log(sse / nrow(z)) + 2 * nrow(B)
  row.names(B) <- paste0(paste0(rep(paste0("psi", 1:p), each = ncol(x)), rep(0:1, each = p * ncol(x)))
                         , paste0("(", rep(sp_loc, p), ")") )

  names(MSE_each) = sp_loc
  names(MAPE_each) = sp_loc

  res <- z - z_hat
  #p <- m0$rank
  N <- length(res)
  w <- rep.int(1, N)
  loglike.calc = .5* (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) +log(sum(w*res^2))))
  aic = -2*as.numeric(loglike.calc)+2*(length(B)+1)


  fitted_values <- as.data.frame(fitted_values)
  colnames(fitted_values) <- sp_loc

  if(d == 0  & !is.null(dt)) {
    fitted_values <- xts::xts(fitted_values, order.by = as.Date(dt[-seq(p)]))
    #fitted_values <- dplyr::select(fitted_values, c(Date, dplyr::everything()))
    #x_base$Date <- as.Date(dt)
    #x_base <- dplyr::select(x_base, Date, c(Date, dplyr::everything()))
    x_base <- xts::xts(x_base, order.by = as.Date(dt))
   } else if (d > 0  & !is.null(dt)){
    fitted_values$Date <-as.Date(dt[-seq(d + p)])
    fitted_values <-  xts::xts(fitted_values, order.by = as.Date(dt[-seq(p)]))
    #x_base$Date <- as.Date(dt)
    #x_base <- dplyr::select(x_base, Date, c(Date, dplyr::everything()))
    x_base <- xts::xts(x_base, order.by = as.Date(dt))
   }


out <- list(B = B, fitted_values = fitted_values,
            std_err = std_err,t_value = t_result, p_value = p_value, p = p, d = d,
            MSE_total = MSE_total
            ,MAPE_total = MAPE_total
            ,MSE_each = MSE_each
            ,MAPE_each  = MAPE_each, AIC = aic, W = W, data = x_base, periodicity = freq, Date = dt)

#0.5 * (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) +
#                            log(sum(w * res^2))))

class(out) <- "gstar"
out

}
