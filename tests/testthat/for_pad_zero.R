library(testthat)
source("R/pad_zero.R")

x <- data.frame(
#  Date = seq(as.Date('2000-01-01'), by = "month" , length.out = 100),
  X1 = rnorm(100),
  X2 = rnorm(100),
  X3 = rnorm(100),
  X4 = rnorm(100)
)


chek_pad  <- function(x, p) {
for(i in 1:p) {
  for(j in 1:ncol(x)) {
    ""
  }
}
}
x_mat <- as.matrix(x)
test_that("test pad_zero function", {
  expect_error(pad_zero(x, 1))
  expect_error(pad_zero(x, 2))
  expect_s3_class(pad_zero(x_mat, 1), "data.frame")
})
