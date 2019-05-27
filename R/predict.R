predict.gstar <- function(out, newdata = NULL){
  if(is.null(newdata)) {
    return(out$fitted_values)
  } else if(d == 0) {
    pr <- out$B %*%  matrix(newdata, ncol = 1)
  }

}
