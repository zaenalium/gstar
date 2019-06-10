context("output prediction class")

library(gstar)
#data("LocationCPI")
#data("Loc")



W <- matrix(c(0,1,1,1,
              1,0,1,1,
              1,1,0,1,
              1,1,1,0),4,4)/3

weight <- matrix(c(0,1,1,1,
              1,0,1,1,
              1,1,0,1,
              1,1,1,0),4,4)/3

x <- data.frame(
  Date = seq(as.Date('2000-01-01'), by = "month" , length.out = 100),
  X1 = rnorm(100),
  X2 = rnorm(100),
  X3 = rnorm(100),
  X4 = rnorm(100)

)


p = 2; d = 0
est = "OLS"; dmat = NULL; date_column = NULL
#weight = "uniform"



x_wo_date <- x[1:90, -1]
#x_test <- LocationCPI[-c(1:90), -1]

x <- xts::xts(x[, 2:5], order.by = as.Date(x[, 1]))

fit <-  gstar(x, weight = weight,
              p = 1, d = 0, est = "OLS")

fit2 <-  gstar(x_wo_date, weight = weight,
              p = 1, d = 0)


test_that("test s3 class of output forecasting", {
  expect_s3_class(predict(fit), "xts")
  expect_s3_class(predict(fit2), "data.frame")
})


