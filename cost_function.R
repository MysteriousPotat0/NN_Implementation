cost = function(theta, x, y, lambda, num_label){
  
  # Separating theta into theta 1 and 2
  theta1 <- matrix(Theta[1:10025], nrow(theta1), ncol(theta1))
  theta2 <- matrix(Theta[10026:10285], nrow(theta2), ncol(theta2))
  
  # Cost to 0
  cost = 0
  
  # Forward Propagation
  x <- cbind(rep(1, m), x) # Add a column of 1's to x
  
  z2 <- theta1 %*% t(x)
  
  a2 <- sig(z2)
  
  a2 <- cbind(c(rep(1, m)), t(a2))
  
  z3 <- theta2 %*% t(a2)
  
  h <- sig(z3)
  
  # Implementing the cost function with regularization
  cost = (1/m)*(sum((-labels * log(h)) - ((1-labels)*log(1-h)))) +
    
    (lambda/(2*m))*(sum(theta1[, 2:ncol(theta1)]^2) + sum(theta2[, 2:ncol(theta2)]^2))
  
}