gstar <- function(x, weight = "uniform",
                  p = 1, d = 0, est = "OLS", dmat = NULL, date_column = NULL) {

  WEIGHT <- c("uniform", "binary", "crosscorrelation", "inverse")
  wg <- pmatch(weight, WEIGHT)
  if (is.na(weight))
    stop("invalid weight")
  if (wg == -1)
    stop("ambiguous weight, please insert correct weight")
  if(weight %in% c("binary", "inverse") & is.null(dmat))
    stop("if you use either 'binary' or 'inverse', please provide me distance matrix")
  if(weight == "binary" & (sum(!(dmat == 0 | dmat == 1))) != 0)
    stop("if you use binary weight, please provide me distance matrix that contain 1 or 0")
  EST <- c("OLS")
  match_est <- pmatch(est, EST)
  if (match_est == -1) stop("please insert correct estimation method")

  var_num <- sapply(x, is.numeric)
  if(sum(var_num) < 2 ) {
     stop("The data 'x' should contain at least two numeric variables. For univariate analysis consider ar() and arima() in package stats.\n")
  }
  if(sum(!var_num) >= 1 ) {
    stop("The data 'x' should contain numeric data, if you need to the Date please convert it to 'xts' object or 'ts' object")
  }
  if(any(c(p,d) < 0 )){
    stop("The GSTAR order must be positive")
  }

  if(any(class(x) %in% c("zoo", "xts", "ts"))){
    x <- xts::as.xts(x)
    dt <- zoo::index(x)
    freq <- xts::periodicity(x)$scale
  } else {
    dt <- NULL
    freq <- NULL
  }

  x <- as.matrix(x)

 fit <-  gstar_est(x , p, d, dt, freq)
 class(fit) <- "gstar"

 fit

}
