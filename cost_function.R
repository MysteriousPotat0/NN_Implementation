# Cost function for the neural network, this function is only used for minimizing the Theta parameter
cost <- function(Theta){
  
  # Rolling weights into their respective sizes
  theta1 <- matrix(Theta[1:theta1_size], nrow(theta1), ncol(theta1))
  theta2 <- matrix(tail(Theta, theta2_size), nrow(theta2), ncol(theta2))
  
  m <- nrow(x)
  
  x <- ones(m, x) # Add a column of 1's to x
  
  z2 <- mat_mult(theta1, t(x))
  
  a2 <- sig(z2)
  
  a2 <- ones(m, t(a2))
  
  z3 <- mat_mult(theta2, t(a2))
  
  h <- sig(z3)

  # Implementing the cost function with regularization
  (1/m)*(sum((-y * log(h) - ((1-y)*log(1-h))))) + (lambda/(2*m))*(sum(theta1[, 2:ncol(theta1)]^2) + sum(theta2[, 2:ncol(theta2)]^2) +
                                                                    sum(theta3[,2:ncol(theta3)]^2))
  
}


# Function that is used for calculating the error the fitted value
error_cost <- function(Theta, x, y){
  
  # Rolling weights into their respective sizes
  theta1 <- matrix(Theta[1:theta1_size], nrow(theta1), ncol(theta1))
  theta2 <- matrix(tail(Theta, theta2_size), nrow(theta2), ncol(theta2))
  
  m <- nrow(x)
  
  x <- ones(m, x) # Add a column of 1's to x
  
  z2 <- mat_mult(theta1, t(x))
  
  a2 <- sig(z2)
  
  a2 <- ones(m, t(a2))
  
  z3 <- mat_mult(theta2, t(a2))
  
  h <- sig(z3)
  
  # Implementing the cost function with regularization
  (1/m)*(sum(sum((-y * log(h) - ((1-y)*log(1-h)))))) + (lambda/(2*m))*(sum(theta1[, 2:ncol(theta1)]^2) + sum(theta2[, 2:ncol(theta2)]^2) +
                                                                         sum(theta3[,2:ncol(theta3)]^2))

}


# Function used to calculate the error between two data set
train_cv_error <- function(Theta, set1, set2, y1, y2){
  
  
  cost1 <- error_cost(Theta, set1, y1)
  
  y <- validate[, ncol(validate)]
  
  cost2 <- error_cost(Theta, set2, y2)
  
  c(cost1, cost2)
  
  
}









