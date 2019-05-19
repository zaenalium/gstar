dist_gstar <- function(x, method = "euclidean"){
  dist(x, method = method, diag = T, upper = T, p = 2)
}
