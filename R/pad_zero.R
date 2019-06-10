pad_zero <- function(x, p) {
  n_col <- ncol(x)
  n_row <- nrow(x)

  list_mat <- list()

  for(i in 1:p) {
    list_mat_temp <- rbind(matrix(ncol = ncol(x), nrow = i), x)
    list_mat[[i]]  <- list_mat_temp[1:(nrow(list_mat_temp)-i), ]
  }

  list_mat_comp <- na.omit(do.call(cbind,list_mat))


  n_rowp <- n_row - p
    B <- matrix(0,
                nrow =  n_rowp * n_col, ncol =n_col * p)
 l <- 1
for(j in 1:p) {
   for(k in 1:(n_col)) {
    B[(1 + (n_rowp * (k - 1))):(n_rowp * k), l] <- list_mat_comp[, l]
    l <- l + 1
  }
}
    return(B)
}
