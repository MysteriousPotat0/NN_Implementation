# Neural Network Implementation in R  
  
## Changelog  
### V2 -- Second Version  
* Cleaned up some functions to make everything more compact
* Included a new fit_model function to make training model easier
* Added a new Extra.R used for extra functionality
### V1 - First Version
* Added cost functions
* Added backward propagation
* Added ability to make prediction
* Added weight Initialization
* Added sigmoid gradient and activation function
* Added ability to store and load trained model
## Introduction  
The Neural Network is based on a similar architecture in the OCTAVE programming language. I have reimplemented it into R. The Neural Network is a multi-layered perceptron, that uses forward propagation and backward propagation.  
## Weight Initialization  
Weight initialization is currently done using the random uniform function in R. It takes in the size of each input and output layer, and generate some random number that can be used to break symmetry.  
There are plans to add different types of weight initialization such as the normal weight initialization.
## Cost Function  
The cost function takes in the training data, and weights, then passes it forward through each layer. At the end of the layer, returns an output based on the logistic loss function. The loss function is regularized with the parameter $\lambda$.  
Future plans to add soft max, and RELU activation functions.
## BackPropagation
Backpropagation takes in training data, and weights, then it passes it foward using forward propagation. After that is finished, it goes back from the layer to the input layer. After going through each layer, all the weights are updated. 
## Activation Function
Currently only supports sigmoid activation function and sigmoid gradient. Limiting the neural network to mostly binary classification.
Plans for softmax and RELU.
## Fit Model
Fit model function takes in training data, training label, lambda, weights, batch_size, numeber of iteration. It returns a list of all the cost calculated from each iteration, and the final trained weight. The function has the ability to check if cost is stuck, then stop the process if the cost is not reducing fast enough. Most likely due to hitting the local minima. It uses the pack nloptr to train and update the weight, with the number of max evaluation per iteration increasing. 
## Extra Functions
* Ones: Adds a column of 1 to the matrix
* Zeros: Adds a column of 0 to the matrix
* mat_mult: Performs matrix multiplication between two objects
