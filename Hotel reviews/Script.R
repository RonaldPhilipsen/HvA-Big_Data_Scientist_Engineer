library(devtools)
install_github("ronald245/RTextTools", subdir = "RTextTools")
library("RtextTools")

required_packages <- c("magrittr", "tm", "mlr", "e1071", "dplyr", "kernlab", "lubridate", "dtplyr",
                       "readr", "ggplot2", "tidytext", "stringr", "tidyr", "scales", "broom",
                       "SnowballC", "wordcloud", "reshape2", "rvest",
                       "tibble", "DBI", "httr", "RMySQL")
extra_scripts <- c("Tools.R", "Sentiment.R", "Classifiers.R", "CleanData.R", "Scraper.R")

x <- lapply(required_packages, require, character.only = TRUE)
x <- lapply(extra_scripts, source)

database <- "HotelReviews"
database.ip <- "localhost"
database.user <- "username"
database.password <- "password"

con <- GetMySQLConnection()
original.exists <- dbExistsTable(con, "original")
dbDisconnect(con)


if (!original.exists || !file.exists("originalMatrix.Rd")) {
    CleanCSV("Hotel_Reviews.csv")

    con <- GetMySQLConnection()
    df <- as_tibble(dbReadTable(con, "original"))
    dbDisconnect(con)

    getContribution(df, get_sentiments("afinn"))

    # randomise 
    df <- df[complete.cases(df),]
    df <- df[sample(nrow(df)),]
    df <- df[sample(nrow(df)),]
    df <- df[1:as.integer(nrow(df)/10),]
    SaveMatrix(df)
}

load("originalMatrix.Rd")
load("originalDataFrame.Rd")

if (!file.exists("trainedModels.Rd")) {
    container <- CreateContainer(df, doc_matrix)
    TrainClassifiers(container)
}

#load the models we defined earlier
load("Models.Rd")
load("Analytics.Rd")

#get model analytics
summary(analytics)


con <- GetMySQLConnection()
scraped.exists <- dbExistsTable(con, "scraped")
dbDisconnect(con)

if (!scraped.exists) {
    ScrapeHotels(500)
    ScrapeTripExpert()
}

numScrapedReviews <- ExecuteSQL(database, "call getNumScrapedReviews();")
newData <- as_tibble(ExecuteSQL(database, paste0("call getScrapedReviews(", numScrapedReviews ,");")))

newData <- newData[complete.cases(newData),]


test <- tibble(review_body = paste(newData$review.summary, newData$review.text, " "),
               Consensus = ifelse((as.numeric(newData$review.score)) > 7, +1, -1) )
test$review_body = CleanBody(test$review_body)

new_matrix <- create_matrix(test[, "review_body"],
                            language = "english",
                            removeNumbers = TRUE,
                            stemWords = TRUE,
                            removeSparseTerms = .99,
                            weighting = weightTfIdf,
                            originalMatrix = doc_matrix)

container <- create_container(new_matrix,
                              test$Consensus,
                              testSize = 1:nrow(test),
                              virgin = TRUE)

results <- classify_models(container, models)

test$LOGITBOOST_LABEL = results$LOGITBOOST_LABEL
test$GLMNET_LABEL = results$GLMNET_LABEL
test$FORESTS_LABEL = results$FORESTS_LABEL
test$SVM_LABEL = results$SVM_LABEL
test$calculatedConsensus <- 0

for (i in c(1:nrow(test))) {
    consensus = 0
    consensus <- if (results$LOGITBOOST_LABEL[i] == +1) consensus + 1 else consensus - 1
    consensus <- if (results$GLMNET_LABEL[i] == +1) consensus + 1 else consensus - 1
    consensus <- if (results$FORESTS_LABEL[i] == +1) consensus + 1 else consensus - 1
    consensus <- if (results$SVM_LABEL[i] == +1) consensus + 1 else consensus - 1

    test$calculatedConsensus[i] = consensus
}


analytics <- create_analytics(container, results)
summary(analytics)
recall_accuracy(test$Consensus, results$LOGITBOOST_LABEL)
recall_accuracy(test$Consensus, results$GLMNET_LABEL)
recall_accuracy(test$Consensus, results$FORESTS_LABEL)
recall_accuracy(test$Consensus, results$SVM_LABEL)



review.1 <- read_file("review5.csv")
review.2 <- read_file("review6.csv")
reviews.1 <- tibble(review_body = review.1, " ", Consensus = 0)
reviews.2 <- tibble(review_body = review.2, " ", Consensus = 0)
reviews <- rbind(reviews.1,reviews.2)

new_matrix <- create_matrix(reviews[, "review_body"],
                            language = "english",
                            removeNumbers = TRUE,
                            stemWords = TRUE,
                            removeSparseTerms = .99,
                            weighting = weightTfIdf,
                            originalMatrix = doc_matrix)

container <- create_container(new_matrix,
                              reviews$Consensus,
                              testSize = 1:nrow(reviews),
                              virgin = TRUE)

results <- classify_models(container, models)

reviews$LOGITBOOST_LABEL = results$LOGITBOOST_LABEL
reviews$GLMNET_LABEL = results$GLMNET_LABEL
reviews$FORESTS_LABEL = results$FORESTS_LABEL
reviews$SVM_LABEL = results$SVM_LABEL
reviews$calculatedConsensus <- 0

for (i in c(1:nrow(reviews))) {
    consensus = 0
    consensus <- if (results$LOGITBOOST_LABEL[i] == +1) consensus + 1 else consensus - 1
    consensus <- if (results$GLMNET_LABEL[i] == +1) consensus + 1 else consensus - 1
    consensus <- if (results$FORESTS_LABEL[i] == +1) consensus + 1 else consensus - 1
    consensus <- if (results$SVM_LABEL[i] == +1) consensus + 1 else consensus - 1

    test$calculatedConsensus[i] = consensus
}

analytics <- create_analytics(container, results)
summary(analytics)
recall_accuracy(test$Consensus, results$LOGITBOOST_LABEL)
recall_accuracy(test$Consensus, results$GLMNET_LABEL)
recall_accuracy(test$Consensus, results$FORESTS_LABEL)
recall_accuracy(test$Consensus, results$SVM_LABEL)
