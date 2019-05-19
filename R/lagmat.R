lagmat <- function(x, k) {
  if (!is.vector(x))
    stop('x must be a vector')
  if (!is.numeric(x))
    stop('x must be numeric')
  if (!is.numeric(k))
    stop('k must be numeric')
  if (1 != length(k))
    stop('k must be a single number')
  c(rep(NA, k), x)[1 : length(x)]
}



#xt <- list_lag(x, p)



#xt <- pad_zero(xt)

