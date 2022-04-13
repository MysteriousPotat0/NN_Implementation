source('sigmoid_function.R')

# This files calculates the sigmoid gradient

sig_grad = function(z){
  
  sig(z) * (1-sig(z))
  
  
}