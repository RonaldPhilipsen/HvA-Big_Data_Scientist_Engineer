## https://tensorflow.rstudio.com/blog/text-classification-with-keras.html
# Load in the keras package
library(keras)

# Install TensorFlow
library(tensorflow)
use_condaenv("r-tensorflow")
install_tensorflow(method = "auto")

imdb <- dataset_imdb(num_words = 10000)

train_data <- imdb$train$x
train_labels <- imdb$train$y
test_data <- imdb$test$x
test_labels <- imdb$test$y

# The variables train_data and test_data are lists of reviews; each review is a list of word indices (encoding a sequence of words). 
# train_labels and test_labels are lists of 0s and 1s, where 0 stands for negative and 1 stands for positive.

# Named list mapping words to an integer index.
word_index <- dataset_imdb_word_index()  
reverse_word_index <- names(word_index)
names(reverse_word_index) <- word_index

# Decodes the review. Note that the indices are offset by 3 because 0, 1, and 
# 2 are reserved indices for "padding," "start of sequence," and "unknown."
decoded_review <- sapply(train_data[[2]], function(index) {
  word <- if (index >= 3) reverse_word_index[[as.character(index - 3)]]
  if (!is.null(word)) word else "?"
})
cat(decoded_review)

#######################################################################################
##
##  Preparing the data
##  You can't feed lists of integers into a neural network. You have to turn your lists into tensors. There are two ways to do that:
##      1.Pad your lists so that they all have the same length, turn them into an integer tensor of shape (samples, word_indices), 
##      and then use as the first layer in your network a layer capable of handling such integer tensors
##      (the "embedding" layer, which we'll cover in detail later in the book).
##      2.One-hot encode your lists to turn them into vectors of 0s and 1s. This would mean, for instance, 
#3      turning the sequence [3, 5] into a 10,000-dimensional vector that would be all 0s except for indices 3 and 5, which would be 1s.
##      Then you could use as the first layer in your network a dense layer, capable of handling floating-point vector data.
##
##  Let's go with the latter solution to vectorize the data, which you'll do manually for maximum clarity.
##
#######################################################################################

vectorize_sequences <- function(sequences, dimension = 10000) {
  # Creates an all-zero matrix of shape (length(sequences), dimension)
  results <- matrix(0, nrow = length(sequences), ncol = dimension) 
  for (i in 1:length(sequences))
    # Sets specific indices of results[i] to 1s
    results[i, sequences[[i]]] <- 1 
  results
}

x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)


## You should also convert your labels from integer to numeric, which is straightforward

y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)

model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

val_indices <- 1:10000

x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]

y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

#######################################################################################
##
##  As you can see, the training loss decreases with every epoch, and the training accuracy 
##  increases with every epoch. That's what you would expect when running a gradient-descent
##  optimization - the quantity you're trying to minimize should be less with every iteration. 
##  But that isn't the case for the validation loss and accuracy: they seem to peak at the
##  fourth epoch. This is an example of what we warned against earlier: a model that performs 
##  better on the training data isn't necessarily a model that will do better on data it has 
##  never seen before. In precise terms, what you're seeing is overfitting: after the second 
##  epoch, you're overoptimizing on the training data, and you end up learning representations 
##  that are specific to the training data and don't generalize to data outside of the training set.
##
##  In this case, to prevent overfitting, you could stop training after three epochs. 
##  In general, you can use a range of techniques to mitigate overfitting.
##
##  Let's train a new network from scratch for four epochs and then evaluate it on the test data
##
########################################################################################

model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(x_train, y_train, epochs = 4, batch_size = 512)
results <- model %>% evaluate(x_test, y_test)

results

model %>% predict(x_test[1:10,])
