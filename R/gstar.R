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
#' #-----Use data with xts object----#
#' x = xts(LocationCPI[, -1], order.by = as.Date(LocationCPI[, 1]))
#'
#' s <- round(nrow(x) * 0.8) ## split into training and testing (80:20)
#' x_train <- x[1:s, ]
#' x_test <- x[-c(1:s), ]
#'
#'
#' weight = matrix(c(0, 1, 1, 1,                    # create the uniform weight.
#'                 1, 0, 1, 1,
#'                 1, 1, 0, 1,
#'                 1, 1, 1, 0), ncol = 4, nrow = 4)
#'
#'
#' weight = weight/(ncol(x) - 1) #the sum of weight is equal to 1 every row.
#'
#'
#' fit <-  gstar(x_train, weight = weight,
#'     p = 1, d = 0, est = "OLS")
#' summary(fit)
#'
#' performance(fit)
#' performance(fit, x_test) ## to check the performance with testing data
#'
#' predict(fit, n = 10) #forecast 10 data ahead
#'
#' plot(fit)
#' plot(fit, n_predict = 10) #plot with 10 forecasting data
#' plot(fit, testing = x_test)
#'
#' #---- Use dataframe or matrix---#
#' x2 <- LocationCPI
#' x2$Date <- NULL # remove the date column
#'
#' data(Loc)
#' dst <- as.matrix(dist(Loc[, -1], diag = TRUE, upper = TRUE))
#' dst1 <- matrix(0, nrow = nrow(dst), ncol = ncol(dst))
#'
#'
#' for(i in 1:nrow(dst)) {
#'    for(j in 1:ncol(dst)){
#'      if(j == i) next
#'      dst1[i, j] <- sum(dst[i, -j])/sum(dst[i,])
#'  }
#' }
#'
#' weight_inverse_distance <- matrix(0, nrow =
#'        nrow(dst), ncol = ncol(dst))
#'
#' for(i in 1:nrow(dst)) {
#'    for(j in 1:ncol(dst)){
#'      if(j == i) next
#'      weight_inverse_distance[i, j] <- sum(dst1[i, j])/sum(dst1[i,])
#'
#'  }
#' }
#'
#' fit_inverse_distance <-  gstar(x2, weight =
#'      weight_inverse_distance, p = 2, d = 1, est = "OLS")
#'
#' summary(fit_inverse_distance)
#' performance(fit_inverse_distance)
#' predict(fit_inverse_distance)
#' plot(fit_inverse_distance)
#'
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
