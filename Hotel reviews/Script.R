required_packages <- c("magrittr", "tm", "mlr", "e1071", "dplyr", "kernlab", "lubridate", "dtplyr",
                       "readr", "ggplot2", "tidytext", "stringr", "tidyr", "scales", "broom", 
                       "SnowballC", "wordcloud", "reshape2")
lapply(required_packages, require, character.only = TRUE)

# Source a bunch of helper files
source("ReviewTools.R")
source("GetTokens.R")
source("Sentiment.R")

# Define undesirable patterns, such as stop_pattern
undesirable_words <- c("positive", "negative", "and", "it", "in", "hotel", "staff", "location", "breakfast", "bathroom")
stop_pattern <- paste0("\\b(", paste0(stopwords("en"), collapse = "|"), ")\\b")
undesirable_pattern <- paste0("\\b(", paste0(undesirable_words, collapse = "|"), ")\\b")
AFINN <- get_sentiments("afinn")

# Read in the hotel reviews
df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE)

# only use rows that have completely been filled in
#limit the amount of rows we use for test purposes
df <- df[complete.cases(df),][1:(nrow(df) / 100),]

#get the dates as dates insteadof strings
df$Review_Date <- as.Date(df$Review_Date, format = "%m/%d/%Y")

#display the amount of reviews per week
getReviewsPerWeek(df)

# split and clean the data
pr <- as.data.frame(t(rbind(review_body = df$Positive_Review, Consensus = +1)))
nr <- as.data.frame(t(rbind(review_body = df$Negative_Review, Consensus = -1)))
df <- rbind(pr, nr) %>% tibble::rownames_to_column("ID")

df$review_body <- as.vector(cleanBody(df$review_body))

#randomise 
#all_reviews <- all_reviews[sample(nrow(all_reviews)),]

reviews <- df %>%
    distinct(review_body, .keep_all = TRUE) %>%
    unnest_tokens(word, review_body, drop = FALSE) %>%
    distinct(ID, word, .keep_all = TRUE) %>%
    anti_join(rbind(stop_words, undesirable_words), by = "word") %>%
    filter(str_detect(word, "[^\\d]")) %>%
    group_by(word)

review_words <- reviews %>%
    mutate(word_total = n()) %>%
    ungroup()

getWordCount(review_words)


#get the word counts per word
bing_word_counts <- reviews %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

#get the contributions of single words for a given set of sentiments
get_contributions(reviews, AFINN)

#all_reviews$review_body <- cleanBody(all_reviews$review_body)
#source("NaiveBayes.R")
#doNaiveBayes(all_reviews)

training_set <- all_reviews[1:100,]
test_set <- all_reviews[101:200,]


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
