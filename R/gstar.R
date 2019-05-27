library(sp)
library(reshape2)
library(dplyr)
library(readr)
loc <- read.csv("R/location.csv")

coordinates(loc) <- ~ latitude + longitude

distance <- dist_gstar(coordinates(loc), method = "binary")

df <- read.csv("R/data_IHK.csv")

dist_gstar <- function(x, method = "euclidean"){
  dist(x, method = method, diag = T, upper = T, p = 2)
}
W <- matrix(c(0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0),4,4)/3
##problem
#1. missing data

x <- df
p = 2; d = 0
est = "OLS"; dmat = NULL; date_column = NULL
weight = "uniform"
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
  if(sum(!var_num) >= 2 ) {
    stop("The data 'x' should contain maximum 1 Date Column")
  }
  if(any(c(p,d) < 0 )){
    stop("The GSTAR order must be positive")
  }

  if(is.null(date_column)){
    dt <- as.Date(x[,  !var_num, drop = T])
  } else {
    dt <- as.Date(x[, date_column])
  }
  input <- x
  x <- as.matrix(x[, var_num])



 fit <-  gstar_est(x , p, d, dt)
 class(fit) <- "gstar"

 fit

}


fit <-  gstar(x, weight = "uniform",
              p = 2, d = 2, est = "OLS")
