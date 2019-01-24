source("Settings.R")
source("Tools.R")
source("Mongo.R")
list.of.packages <- c("tibble", "magrittr", "dplyr",
                      "sparklyr", "shiny", "leaflet",
                      "keras", "tm", "ffbase")

Require.packages(list.of.packages)

path = paste0("R -e \"shiny::runApp(\'", getwd(), "/shiny\', launch.browser = TRUE)\"")
print(paste0("use the following command to start shiny: ", path))

if (!file.exists(fn.mixed.reviews)) {
    hotel.reviews <- hotel.reviews.collection$find();

    if (!exists(hotel.reviews)) {
        hotel.reviews.raw <- read.csv(file = fn.hotel.reviews, header = TRUE, quote = "\"", dec = ".")
        hotel.reviews.collection$insert(hotel.reviews.raw)
    }

    if (!file.exists(fn.positive.reviews)) {
        hotel.reviews.positive <- getPositiveReviews(numReviewsToDl);
        write.csv2(hotel.reviews.positive, file = fn.positive.reviews, row.names = FALSE)
    }

    if (!file.exists(fn.negative.reviews)) {
        hotel.reviews.negative <- getNegativeReviews(numReviewsToDl);
        write.csv2(hotel.reviews.negative, file = fn.negative.reviews, row.names = FALSE)
    }
}

if (file.exists(fn.mixed.reviews)) {
    reviews.mixed <- read.csv2.ffdf(file = fn.mixed.reviews)
    #reviews.mixed <- read.csv2(fn.mixed.reviews)
} else {
    hotel.reviews.positive <- read.csv2.ffdf(file = fn.positive.reviews, sep = ";")
    hotel.reviews.negative <- read.csv2.ffdf(file = fn.negative.reviews, sep = ";")
    #hotel.reviews.positive <- read.csv2(file = fn.positive.reviews, sep = ";")
    #hotel.reviews.negative <- read.csv2(file = fn.negative.reviews, sep = ";")

    reviews.mixed <- ffdfappend(hotel.reviews.positive, hotel.reviews.negative, adjustvmode = F)
    #reviews.mixed <- bind_rows(hotel.reviews.positive, hotel.reviews.negative)

    write.csv2.ffdf(reviews.mixed, file = fn.mixed.reviews)
}


print(paste0("Total number of reviews: ", nrow(reviews.mixed)))
#print(paste0("number of positive reviews: ", nrow(reviews.mixed[which(reviews.mixed$Review_Is_Positive == 1),])))
print(paste0("number of positive reviews: ", nrow(reviews.mixed[ffwhich(reviews.mixed, reviews.mixed$Review_Is_Positive == 1),])))

#print(paste0("number of negative reviews: ", nrow(reviews.mixed[which(reviews.mixed$Review_Is_Positive == 0),])))
print(paste0("number of negative reviews: ", nrow(reviews.mixed[ffwhich(reviews.mixed, reviews.mixed$Review_Is_Positive == 0),])))


#randomize the order 
data <- as.ffdf(reviews.mixed[sample(nrow(reviews.mixed)),])
train_size =  floor(nrow(data) * 0.75)

tokenizer <- text_tokenizer(num_words = vocab_size)

#grab the posts and set them as x-param
train_posts = data[1:train_size, 2]

tokenizer %<>% fit_text_tokenizer(train_posts)

# make a matrix
# we use term-frequency independe
x_train = texts_to_matrix(tokenizer, train_posts, mode = 'tfidf')
train_posts <- NULL


#grab the tags and set them as y-param
train_tags = data[1:train_size, 1]
y_train = to_categorical(train_tags)
train_tags <- NULL 


#define the keras model 
model <- keras_model_sequential()

#softmax is used for multiclass logistic regression, sigmoid is used for two-class logistic regression
model %>%
    # we use binary classification, therefore a rectified linear unit is used
    # returns max(x,0)
    layer_dense(units = batch_size, input_shape = c(vocab_size), activation = 'relu') %>%
    #Dropout consists in randomly setting a fraction rate of input units to 0 at each update during training time, which helps prevent overfitting.
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = (batch_size / 2), activation = "relu") %>%
    layer_dense(units = (batch_size/ 3), activation = 'relu') %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 2, activation = 'sigmoid')

# actually make the model
# use categorical_Crossentropy for multi-class logistic regression
# use binary_crossentropy for two-class logistic regression
model %>% compile(loss = 'binary_crossentropy',
                  optimizer = 'adam',
                  metrics = c('accuracy'))

#train the model on our training dataset
history <- model %>% fit(x_train, y_train,
                    batch_size = batch_size,
                    epochs = 2,
                    verbose = 1,
                    validation_split = 0.1)

#get some basic info about the model
summary(model)

#get rid of the training parameters
x_train <- NULL
y_train <- NULL

#initialize the testing posts and set the x-param
test_posts = data[(train_size + 1):nrow(data), 2]
x_test = texts_to_matrix(tokenizer, test_posts, mode = 'freq')
test_posts <- NULL

#initialize the testing labels and set the y-param
test_tags = data[(train_size + 1):nrow(data), 1]
y_test = to_categorical(test_tags)
test_tags <- NULL

#evaluate the model's performance
score <- evaluate(model, x_test, y_test, batch_size = batch_size, verbose = 1)

print(paste0('Test loss:', score[1]))
print(paste0('Test accuracy:', score[2]))

#get rid of the testing parameters
x_test <- NULL
y_test <- NULL


#predict new things
verify_post <- c("this hotel was very nice", "the waiter was bad and my bathroom was leaky")
x_verify = texts_to_matrix(tokenizer, verify_post, mode = 'freq')

prediction <- model %>% predict_classes(x_verify)