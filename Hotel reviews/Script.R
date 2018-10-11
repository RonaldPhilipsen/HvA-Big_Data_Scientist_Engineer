required_packages <- c("magrittr", "tm", "mlr", "e1071", "dplyr", "kernlab", "lubridate", "dtplyr",
                       "readr", "ggplot2", "tidytext", "stringr", "tidyr", "scales", "broom",
                       "SnowballC", "wordcloud", "reshape2", "RTextTools", "hunspell")
x <- lapply(required_packages, library, character.only = TRUE)

extra_scripts <- c("ReviewTools.R", "Sentiment.R", "Classifiers.R")
x <- lapply(extra_scripts, source)

if (!file.exists("Cleaned.csv")) {
    source("CleanData.R")
}

df <- read.csv("Cleaned.csv", stringsAsFactors = FALSE)
colnames(df) <- gsub("X", "ID", colnames(df))

getContribution(df$review_body, get_sentiments("afinn"))

# randomise 
df <- df[sample(nrow(df)),]
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

model <- DoMultipleClassifiers(mat, reviews, FALSE)

head(model)

test <- data.frame(review_body = "this hotel is a piece of shit i will never go there again you cocksuckers", consensus = NA)

newmatrix <- create_matrix(test, language = "english",
                      removeStopwords = TRUE,
                      removeNumbers = TRUE,
                      stemWords = FALSE,
                      tm::weightTfIdf)

container <- create_container(newmatrix,
                                as.numeric(as.factor(test[, 2])),
                                trainSize = 1:1,
                                testSize = 1:1,
                                virgin = FALSE)


prediction <- classify_model(container, model$TREE)