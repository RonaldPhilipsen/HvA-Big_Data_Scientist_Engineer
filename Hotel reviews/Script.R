library(magrittr)
library(tidytext)
library(dplyr)
library(dtplyr)
library(tm)
library(mlr)
library(e1071)

undesirable_words <- c("no positive", "no negative", "and", "it", "in", "hotel")

# Regex pattern for removing stop words
stop_pattern <- paste0("\\b(", paste0(stopwords("en"), collapse = "|"), ")\\b")
undesirable_pattern <- paste0("\\b(", paste0(undesirable_words, collapse = "|"), ")\\b")

df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE)
combined <- group_by(df, Hotel_Name)[min_rows:max_rows,]

pr <- as.data.frame(t(rbind(Review = combined$Positive_Review, Consensus = +1)))
nr <- as.data.frame(t(rbind(Review = combined$Negative_Review, Consensus = -1)))
all_reviews <- rbind(pr, nr)
all_reviews <- all_reviews[sample(nrow(all_reviews)),][1:1000,]

all_reviews$Review <- all_reviews$Review %>%
    tolower() %>%
    gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    gsub(pattern = "\\d+ ", replacement = "") %>%
    gsub(pattern = "[[:punct:]]", replacement = "") %>%
    gsub(pattern = "[\r\n]", replacement = "") %>%
    gsub(pattern = stop_pattern, replacement = "") %>%
    gsub(pattern = undesirable_pattern, replacement = "") %>%
    gsub(pattern = " {2,}", replacement = " ") %>%
    trimws()


training_set <- all_reviews[1:20,]
matrixReviews = RTextTools::create_matrix(all_reviews[, 1],
                                          language = "english",
                                          removeStopwords = FALSE,
                                          removeNumbers = FALSE,
                                          stemWords = FALSE)
maReviews = as.matrix(matrixReviews)


classifier = naiveBayes(maReviews[1:1000,], as.factor(all_reviews[1:1000,2]))
predicted = predict(classifier, maReviews[101:200,])
print(table(all_reviews[101:200, 2], predicted))
RTextTools::recall_accuracy(all_reviews[101:200, 2], predicted)
