# A function to fit the training data
fit_model <- function(train_data, train_labels, Theta, lambda, batch_size, iterations){
  
  # If batch size is missing, default to full batch size
  if(missing(batch_size)){
    
    batch_size = nrow(train_data)
    
  }
  
  # If lambda is missing, default to 0
  if(missing(lambda)){
    
    lambda = 0
    
  }
  
  # Set up index for each iteration
  index = 0
  
  # List to store all useful values
  history = list()
  
  # Slowly increasing the max eval for the nloptr option
  max_eval <- seq(from = 5, to = 2*iterations+5, by = 5)
  
  # Loop from 1 to number of epoch specified
  for (index in 1:iterations){
    
    # Random shuffle of training data
    shuffle <- sample(nrow(train_data), nrow(train_data), replace=F)
    train_data <- train_data[shuffle,]
    train_labels <- train_labels[, shuffle]
    
    # set x and y
    x <<- matrix(train_data, ncol=ncol(train_data))  # Training data
    y <<- matrix(train_labels, nrow = nrow(train_labels))  # Training Label
    
    # options for the algorithm
    options = list('algorithm' = 'NLOPT_LD_LBFGS', 
                   'maxeval' = max_eval[index])
    
    # uses nloptr package to find the minimum weight using gradient and cost function
    min_cost <- nloptr(Theta, eval_f = cost, 
                       eval_grad_f = grad, 
                       opt=options)
    
    # Updated new weight for neural netowrk
    weight <- matrix(min_cost$solution, ncol = 1)
    
    # Calculates the cost of the new weight
    current_cost <- cost(weight)
    
    print(paste('Currently Running Epoch:', 
                index, '| Current Cost:', 
                current_cost))
    
    # Store this iteration of values into history 
    history[[index]] <- list(current_cost)
    
    # Check if the length of history is greater than 1
    if(length(history) > 1){
      
      # Calculates the difference between the current and previous cost
      diff <- as.numeric(history[[index-1]]) -  as.numeric(history[[(index)]])
      
      # If the cost is less than 1e-8, then break out of the for loop
      if(diff <= 1e-8){

        print(paste('Difference Between Cost Is', diff, 'is less that 1e-8'))
        
        break
        
      }
      
    }
  
  }
  
  # Adds the final iteration of weight into history
  history$weight <- weight
  
  # Return all values
  return(history)
  
}