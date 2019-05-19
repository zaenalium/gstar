list_lag <- function(x, p){
  list_mat <- list()
  for(i in 1:p) {
    list_mat[[i]] <- rbind(matrix(ncol = ncol(x), nrow = i), x)[1:(nrow(x)-p + 1), ]
    # colnames(list_mat[[i]]) <-  paste0(colnames(list_mat[[i]]),"_t-", i)
  }
  list_mat_comp <- na.omit(do.call(cbind,list_mat))
  return(list_mat_comp)
}

