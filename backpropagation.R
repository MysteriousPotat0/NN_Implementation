grad = function(Theta, x, y, lambda, num_label){
  
  theta1 <- matrix(Theta[1:10025], nrow(theta1), ncol(theta1))
  theta2 <- matrix(Theta[10026:10285], nrow(theta2), ncol(theta2))

  
  # Forward Feed
  x <- cbind(rep(1, m), x) # Add a column of 1's to x
  
  z2 <- theta1 %*% t(x)
  
  a2 <- sig(z2)
  
  a2 <- cbind(c(rep(1, m)), t(a2))
  
  z3 <- theta2 %*% t(a2)
  
  h <- sig(z3)
  
  # Back propagation
  delta_3 <- h - labels
  
  delta_2 <- t(theta2[, 2:ncol(theta2)]) %*% delta_3 * sig_grad(z2)
  
  # 0th theta 1 gradient
  theta1_grad <- delta_2 %*% x
  theta1_grad[, 1] = (1/m)*theta1_grad[, 1]
  
  # When theta 1 gradient = j
  theta1_grad[, 2:ncol(theta1_grad)] <-(1/m)*(theta1_grad[, 2:ncol(theta1_grad)] + lambda*theta1[, 2:ncol(theta1)])
  
  # 0th theta 2 gradient
  theta2_grad <- delta_3 %*% a2
  theta2_grad[, 1] <- (1/m)*theta2_grad[, 1]
  
  # When theta 2 gradient = j
  theta2_grad[, 2:ncol(theta2_grad)] <- (1/m)*(theta2_grad[, 2:ncol(theta2_grad)] + lambda*theta2[, 2:ncol(theta2)])
  
  grad <- cbind(c(theta1_grad, theta2_grad))
  
}
