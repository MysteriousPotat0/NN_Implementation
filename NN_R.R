library(R.matlab)
source('sigmoid_function.R')
source('sigmoid_gradient.R')
source('cost_function.R')
source('backpropagation.R')
source('initializing_weight.R')
set.seed(1)


# importing data
data <- readMat('ex4data1.mat')
weights <- readMat('ex4weights.mat')

# Initializing the parameters
input_layer <- 400
hidden_layer <- 25
num_label <- 10

# Setting x and y
x = as.matrix(data$X)
y = as.matrix(data$y)
m <- dim(x)[1]

# setting theta 1 and 2
theta1 = initial_weights(input_layer, hidden_layer)
theta2 = initial_weights(hidden_layer, num_label)
Theta = as.vector(cbind(c(theta1, theta2)))

# Setting up the labels to encode the output
labels <- matrix(0, num_label, dim(y)[1])

i = 0
for (i in 1:dim(labels)[2]){
  
  labels[y[i], i] <- 1
  
}

lambda = 1

i = 0
j = list()
learn_curve <- list()
for (i in 1:250){
  
  Theta <- Theta - 2.03*grad(Theta, x, y, lambda, num_label)
  
  j = rbind(j, cost(Theta, x, y, lambda, num_label))
}



plot(j)
lines(j)

pred <- function(Theta, x){
  
  theta1 <- matrix(Theta[1:10025], nrow(theta1), ncol(theta1))
  theta2 <- matrix(Theta[10026:10285], nrow(theta2), ncol(theta2))
  
  num_label <- dim(theta2)[1]
  
  x <- cbind(rep(1, m), x) # Add a column of 1's to x
  
  z2 <- theta1 %*% t(x)
  
  a2 <- sig(z2)
  
  a2 <- cbind(c(rep(1, m)), t(a2))
  
  z3 <- theta2 %*% t(a2)
  
  h <- sig(z3)
  
  
}


# grad_check <- function(Theta, lambda){
#   
#   
#   eps = 0.00001
#   
#   theta_plus <- Theta + eps
#   
#   theta_minus <- Theta - eps
#   
#   grad_plus <- (grad(theta_plus, x, y, num_label))
#   grad_minus<- (grad(theta_minus, x, y, num_label))    
#         
#   
#   num_grad <- (grad_plus - grad_minus)/(2*eps)
#   
#   grad <- grad(Theta, x, y, lambda, num_label)
#   
#   print(norm(num_grad - grad)/norm(num_grad + grad))
#   
#   cbind(num_grad, grad)
#   
#   
#   
#}


a <- pred(Theta, x)
b <- apply(a, 2, max)
p <- matrix(c(rep(0)), 10, 5000)
p[a==b] <- 1

i = 0
for (i in 1:5000){
  
  p[a[,i] == b[i],i] <- 1
  
}


i = 0
count = 0
for (i in 1:5000){
  
  if(p[y[i],i]== T){
    
    count <- count + 1

  }

}


count/length(y)
