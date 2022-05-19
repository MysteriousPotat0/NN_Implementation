# Add a column of ones
ones <- function(size, data){
  
  col_ones <- rep(1, size)
  
  return(cbind(col_ones, data))
  
}


# Add a column of zeros
zeros <- function(size, data){
  
  col_zeros <- rep(0, size)
  
  return(cbind(col_zeros, data))
  
}


# Performs Matrix Multiplication
mat_mult <- function(object_1, object_2){
  
  return(object_1 %*% object_2)
  
}



