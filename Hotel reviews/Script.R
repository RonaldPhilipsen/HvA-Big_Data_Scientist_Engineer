library(magrittr)
library(tidytext)
library(dplyr)
library(dtplyr)
library(tm)
library(mlr)
library(parallelMap)


finn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
undesirable_words <- c("No positive", "No Negative", "AND", "it", "in")
stop_words <- stopwords(kind = "en")

# Regex pattern for removing stop words
stop_pattern <- paste0("\\b(", paste0(stopwords("en"), collapse = "|"), ")\\b")
undesirable_pattern <- paste0("\\b(", paste0(undesirable_words, collapse = "|"), ")\\b")

df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE)

df$Combined_Review <- paste(df$Positive_Review, df$Negative_Review)
combined <- group_by(df, Hotel_Name)
combined$ID <- seq.int(nrow(combined))

pr <- as.data.frame(t(rbind(Review = combined$Positive_Review, Consensus = +1)))
nr <- as.data.frame(t(rbind(Review = combined$Negative_Review, Consensus = -1)))
all_reviews <- rbind(pr, nr)
all_reviews <- all_reviews[sample.int(nrow(all_reviews)),]


all_reviews$Review <- all_reviews$Review %>%
    gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    gsub(pattern = "\\d+ ", replacement = "") %>%
    gsub(pattern = "[[:punct:]]", replacement = "") %>%
    gsub(pattern = "[\r\n]", replacement = "") %>%
    gsub(pattern = stop_pattern, replacement = "") %>%
    gsub(pattern = undesirable_pattern, replacement = "") %>%
    gsub(pattern = " {2,}", replacement = " ") %>%
    trimws()

training_set <- all_reviews[1:20,]


matrixReviews = RTextTools::create_matrix(all_reviews[1:1000, 1], language = "english",
                      removeStopwords = FALSE, removeNumbers = TRUE,
                      stemWords = FALSE)
maReviews = as.matrix(matrixReviews)


classifier = naiveBayes(maReviews[1:25,], as.factor(all_reviews[1:25, 2]))
predicted = predict(classifier, maReviews[26:100,])
table(all_reviews[26:10, 2], predicted)
RTextTools::recall_accuracy(all_reviews[26:100, 2], predicted)


#docs <- Corpus(VectorSource(combined$Positive_Review)) %>%
#    tm_map(removePunctuation) %>%
#    tm_map(removeNumbers) %>%
#    tm_map(cleanData)
