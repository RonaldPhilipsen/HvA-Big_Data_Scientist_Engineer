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
    reviews.mixed <- read.csv2(fn.mixed.reviews)
} else {
    hotel.reviews.positive <- read.csv2(file = fn.positive.reviews, sep = ";") %>% as_tibble()
    hotel.reviews.negative <- read.csv2(file = fn.negative.reviews, sep = ";") %>% as_tibble()

    reviews.mixed <- bind_rows(hotel.reviews.positive, hotel.reviews.negative)
    reviews.mixed <- reviews.mixed[sample(nrow(reviews.mixed)),]
    reviews.mixed <- reviews.mixed[sample(nrow(reviews.mixed)),]

    write.csv2(reviews.mixed, file = fn.mixed.reviews, row.names = FALSE)
}

print(paste0("Total number of reviews: ", nrow(reviews.mixed)))
print(paste0("number of positive reviews: ", nrow(reviews.mixed[which(reviews.mixed$Review_Is_Positive == 1),])))
print(paste0("number of negative reviews: ", nrow(reviews.mixed[which(reviews.mixed$Review_Is_Positive == 0),])))


#randomize the order 
data <- reviews.mixed[sample(nrow(reviews.mixed)),]
train_size = floor(nrow(data) * 0.75)
train_posts = data[1:train_size, 2]
train_tags = data[1:train_size, 1]
test_posts = data[(train_size + 1):nrow(data), 2]
test_tags = data[(train_size + 1):nrow(data), 1]

tokenizer <- text_tokenizer(num_words = vocab_size) %>%
    fit_text_tokenizer(train_posts)

x_train = texts_to_matrix(tokenizer, train_posts, mode = 'freq')
y_train = to_categorical(train_tags)

x_test = texts_to_matrix(tokenizer, test_posts, mode = 'freq')
y_test = to_categorical(test_tags)

x_verify = texts_to_matrix(tokenizer, c("i am very happy with this hotel", "this hotel is very bad, no room service, i would not come here again"), mode = 'freq')

model <- keras_model_sequential()

model %>%
    layer_dense(units = batch_size, input_shape = c(vocab_size), activation = 'relu') %>%
    layer_dense(units = batch_size, input_shape = c(vocab_size), activation = 'relu') %>%
    layer_dense(units = 2, activation = 'softmax')

model %>% compile(loss = 'categorical_crossentropy',
                  optimizer = 'adam',
                  metrics = c('accuracy'))

history <- model %>% fit(x_train, y_train,
                    batch_size = batch_size,
                    epochs = 2,
                    verbose = 1,
                    validation_split = 0.1)

summary(model)

score <- evaluate(model, x_test, y_test, batch_size = batch_size, verbose = 1)

print(paste0('Test score:', score[1]))
print(paste0('Test accuracy:', score[2]))


for (i in 1:10) {
   prediction <- predict(model, x_verify)
}
