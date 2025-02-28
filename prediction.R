# Function to Predict the results
# 
# Takes in trained Theta and the input used for predicton
# 

pred <- function(Theta, x){
  
  # Forward Propagation
  theta1 <- matrix(Theta[1:theta1_size], nrow(theta1), ncol(theta1))
  theta3 <- matrix(tail(Theta, theta2_size), nrow(theta2), ncol(theta2))
  
  m <- nrow(x)
  
  # Forward Feed
  # x <- x*matrix(rbinom(dim(x)[2], 1, 0.8), dim(x)[1], dim(x)[2], byrow = T)
  
  x <- cbind(rep(1, m), x) # Add a column of 1's to x
  
  z2 <- theta1 %*% t(x)
  
  a2 <- sig(z2)
  
  # a2 <- a2 * matrix(rbinom(dim(a2)[2], 1, 0.5), dim(a2)[1], dim(a2)[2], byrow = T)
  
  a2 <- cbind(c(rep(1, m)), t(a2))
  
  z3 <- theta2 %*% t(a2)
  
  h <- sig(z3)
  
}