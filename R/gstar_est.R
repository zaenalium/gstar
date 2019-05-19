gstar_est <- function(z, xt, w_xt, method){
  Xv <- cbind(xt, w_xt)
  B <- solve(t(Xv) %*% Xv) %*% t(Xv) %*% z
  z_hat <- Xv %*% B
  sse <- sum((z - z_hat)^2)/ (nrow(z)- ncol(B) - 1)
  std_err <- sse * diag(solve(t(Xv) %*% Xv))
  t_result <- B/std_err
  p_value <-  2 * pt(abs(t_result), (nrow(z)- ncol(B) - 1),
                      lower.tail = FALSE)
  row.names(B) <- paste0(paste0("psi", 1:ncol(xt)), rep(0:(nrow(B) / ncol(xt) - 1),
                                                        each = ncol(xt)))
  fitted_values = matrix(z_hat, ncol = ncol(xt) )
  z_mat <-  matrix(z, ncol = ncol(xt) )

out <- list(z = z,z_hat = z_hat, B = B, fitted_values =
              matrix(z_hat, ncol = ncol(xt), byrow = T),
            std_err = std_err, p_value = p_value, p = p, q = q,
            MSE_total = mean((z- z_hat)^2),
            MAPE_total = mean(abs((z- z_hat)/z)) * 100,
            MSE_each = apply(z_mat- fitted_values, 2, function(x) mean(x^2)),
            MAPE_each = apply(abs((z_mat- fitted_values) / z_mat),
                              2, function(x) mean(100*x^2))
            )

class(out) <- "gstar"
out

}
