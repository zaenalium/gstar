#' Fit Generalized Space-Time Autoregressive Model
#'
#' @description gstar function return the paramter estimatation of Generalized Space-Time Autoregressive Model.
#' @usage gstar(x, weight, p = 1, d = 0, est = "OLS")
#' @param x a dataframe, matrix or xts/ts object that contain time series data.
#' @param weight a spatial weight ncol(x) * ncol(x) with diagonal = 0.
#' @param p an autoregressive order, value must be greater than 0.
#' @param d a lag differencing order, value must be greater than 0.
#' @param est estimation method, currently only OLS available, another estimation will be added later.
#' @return gstar returns output like lm, use function \code{\link{summary}}to obtain the summary coefficients and others. The detail are shown in the following list :
#' \itemize{
#' \item coefficients - a named vector of coefficients.
#' \item AIC - A version of Akaike's An Information Criterion (the calculation similiar with aic in \emph{lm} method )}
#' @references Budi Nurani Ruchjana, Svetlana A. Borovkova and H. P. Lopuhaa (2012), \emph{Least Squares Estimation of Generalized Space Time AutoRegressive (GSTAR) Model and Its Properties}, The 5th International Conference on Research and Education in Mathematics AIP Conf. Proc. 1450, 61-64.
#' @seealso \code{\link{summary}} for summarises the model that has been built. Also use \code{\link{predict}} to predict model to testing or new data.
#' @examples library(gstar)
#' library(xts)
#' data("LocationCPI")
#'
#' x = xts(LocationCPI[, -1], order.by = as.Date(LocationCPI[, 1]))
#' weight = matrix(c(0, 1, 1, 1,
#'                 1, 0, 1, 1,
#'                 1, 1, 0, 1,
#'                 1, 1, 1, 0), ncol = 4, nrow = 4)
#'
#' #the sum of weight is equal to 1 every row.
#' weight = weight/(ncol(x) - 1)
#'
#' fit <-  gstar(x, weight = weight, p = 1, d = 0, est = "OLS")
#' summary(fit)
#'
#' performance(fit)
#'
#' predict(fit)
#'
#' plot(fit)
#' @export gstar


gstar <- function(x, weight,
                  p = 1, d = 0, est = "OLS") {
  #WEIGHT <- c("uniform", "binary", "crosscorrelation", "inverse")
  #wg <- pmatch(weight, WEIGHT)
  if (sum(is.na(weight) > 0))
    stop("the weight contain missing values, please insert correct weight")
  if (ncol(weight) !=  ncol(x) && nrow(weight) !=  ncol(x))
    stop("the weight shape should be ncol(x) * ncol(x), please insert correct weight")
  if (sum(rowSums(weight)) > ncol(x))
    warning("the sum of weight is equal to 1 every row")
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

 fit <-  gstar_est(x , W = weight, p, d, dt, freq)
 class(fit) <- "gstar"

 fit

}
