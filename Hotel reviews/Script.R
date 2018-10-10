required_packages <- c("magrittr", "tm", "mlr", "e1071", "dplyr", "kernlab", "lubridate", "dtplyr",
                       "readr", "ggplot2", "tidytext", "stringr", "tidyr", "scales", "broom", 
                       "SnowballC", "wordcloud", "reshape2", "RTextTools")
lapply(required_packages, require, character.only = TRUE)

# Source a bunch of helper files
source("ReviewTools.R")
source("Sentiment.R")
source("Classifiers.R")

if (!file.exists("Cleaned.csv")) {
    source("CleanData.R")
}


df <- read.csv("Cleaned.csv", stringsAsFactors = FALSE)
colnames(df) <- gsub("X", "ID", colnames(df))


# randomise 
df <- df[sample(nrow(df)),]

trainStart <- 1
trainEnd <- 1000
testStart <- 1001
testEnd <- 2000


reviews <- df[trainStart:testEnd, 2:3]

# build dtm
matrix = create_matrix(reviews[, 1], language = "english",
                      removeStopwords = TRUE,
                      removeNumbers = TRUE,
                      stemWords = FALSE,
                      tm::weightTfIdf)

mat = as.matrix(matrix)


DoNaiveBayes2(mat, reviews)
completedContainer <- DoMultipleClassifiers(mat, reviews)
DoCrossValidation(completedContainer, 4)

#DoNaiveBayes(df[1:nrow(df) / 100,])


#reviews <- df %>%
#    distinct(review_body, .keep_all = TRUE) %>%
#    unnest_tokens(word, "review_body", drop = FALSE) %>%
#    distinct(ID, word, .keep_all = TRUE) %>%
#    filter(str_detect(word, "[^\\d]")) %>%
#    #remove all single characters
#    group_by(word)
#
#reviews$review_body <- NULL
#
#review_words <- reviews %>%
#    mutate(word_total = n()) %>%
#    ungroup()
#
#
#getWordCount(review_words)
#
##get the word counts per word
#BingWordCounts <- reviews %>%
#    inner_join(get_sentiments("bing")) %>%
#    count(word, sentiment, sort = TRUE) %>%
#    ungroup()
#
##get the contributions of single words for a given set of sentiments
#AFINN <- get_sentiments("afinn")
#getContribution(reviews, AFINN)

# build the data to specify response variable, training set, testing set.
