training_set <- df[1:100,]
test_set <- df[101:200,]


train <- Corpus(VectorSource(training_set$review_body)) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(removeNumbers)) %>%
    tm_map(content_transformer(removePunctuation)) %>%
    tm_map(cleanData) %>%
    tm_map(content_transformer(stripWhitespace))


train.dtm <- as.matrix(DocumentTermMatrix(train, control = list(wordLengths = c(1, Inf))))

test <- Corpus(VectorSource(test_set$review_body)) %>% cleanCorpus()

test.dtm <- as.matrix(DocumentTermMatrix(test, control = list(wordLengths = c(1, Inf))))

train.df <- data.frame(train.dtm[, intersect(colnames(train.dtm), colnames(test.dtm))])
test.df <- data.frame(test.dtm[, intersect(colnames(test.dtm), colnames(train.dtm))])

train.df$corpus <- training_set[1:100, 2]

df.train <- train.df
df.test <- train.df
#df.model <- ksvm(corpus ~ ., data = df.train, kernel = "rbfdot")
df.model <- ksvm(corpus ~ ., data = df.train, kernel = "rbfdot", C = 100, scaled = TRUE)

pred <- predict(df.model, df.test)
sum(pred == test_set[, 2]) / nrow(test_set)