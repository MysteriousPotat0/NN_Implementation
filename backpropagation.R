source('Extra_Functions.R')
grad = function(Theta){
  
  # Forward Propagation
  theta1 <- matrix(Theta[1:theta1_size], nrow(theta1), ncol(theta1))
  theta2 <- matrix(tail(Theta, theta2_size), nrow(theta2), ncol(theta2))
  
  m <- nrow(x)
  
  x <- ones(m, x) # Add a column of 1's to x
  
  z2 <- mat_mult(theta1, t(x))
  
  a2 <- sig(z2)
  
  a2 <- ones(m, t(a2))
  
  z3 <- mat_mult(theta2, t(a2))
  
  h <- sig(z3)

  # Back propagation
  delta_3 <- h - y
  
  delta_2 <- mat_mult(t(theta2[,2:ncol(theta2)]), delta_3) * sig_grad(z2)
  
  # 0th theta 1 gradient
  theta1_grad <- (1/m)*(delta_2 %*% x + zeros(nrow(theta1),lambda*theta1[,2:ncol(theta1)]))

  # theta 2 gradient
  theta2_grad <- (1/m)*(delta_3 %*% a2 + zeros(nrow(theta2), lambda*theta2[,2:ncol(theta2)]))
  
  grad <- cbind(c(theta1_grad, theta2_grad))
  

}
