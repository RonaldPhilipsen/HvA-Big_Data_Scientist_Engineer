source("Settings.R")
source("Tools.R")
source("Mongo.R")
list.of.packages <- c("tibble", "magrittr", "dplyr",
                      "sparklyr", "shiny", "leaflet", 
                      "keras", "tm", "lime",
                      "rsample", "recipes", "yardstick")

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
        hotel.reviews.positive <- getPositiveReviews();
        write.csv2(hotel.reviews.positive, file = fn.positive.reviews, row.names = FALSE)
    }

    if (!file.exists(fn.negative.reviews)) {
        hotel.reviews.negative <- getNegativeReviews();
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

corpus <- VCorpus(VectorSource(reviews.mixed$Review))
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = T, stopwords = T, removeNumbers = T, stemDocument = T, stripWhitespace = T)) %>%
    removeSparseTerms(0.998)

model <- keras_model_sequential()

model %>%
    layer_dense(units = 64, input_shape = 100) %>%
    layer_activation(activation = 'relu') %>%
    layer_dense(units = 10) %>%
    layer_activation(activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_sgd(lr = 0.02),
  metrics = c('accuracy')
)

#train model on batches of data
model %>% fit(x_train, y_train, epochs = 5, batch_size = 32)

#evaluate performance
loss_and_metrics %>% evaluate(x_test, y_test, batch_size = 128)
                     
# generate predictions on new data:
classes %>% predict(x_test, batch_size = 128)
