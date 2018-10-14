required_packages <- c("magrittr", "tm", "mlr", "e1071", "dplyr", "kernlab", "lubridate", "dtplyr",
                       "readr", "ggplot2", "tidytext", "stringr", "tidyr", "scales", "broom",
                       "SnowballC", "wordcloud", "reshape2", "RTextTools", "hunspell", "rvest",
                       "tibble", "devtools", "DBI", "httr")

x <- lapply(required_packages, library, character.only = TRUE)
install_github("ronald245/RTextTools", subdir = "RTextTools")

extra_scripts <- c("Tools.R", "Sentiment.R", "Classifiers.R", "CleanData.R")
x <- lapply(extra_scripts, source)

if (!file.exists("originalMatrix.Rd") || !file.exists("trainedModels.Rd")) {
    doIfNotExists("Cleaned.csv", CleanCSV, "Hotel_Reviews.csv")

    df <- as_tibble(read.csv("Cleaned.csv", stringsAsFactors = FALSE))

    colnames(df) <- gsub("X", "ID", colnames(df))

    getContribution(df, get_sentiments("afinn"))

    # randomise 
    df <- df[sample(nrow(df)),]
    df <- df[sample(nrow(df)),]

    TrainClassifiers(df)

}

DoIfNotExists("HotelReviews.sqlite", source, "Scraper.R")
con <- dbConnect(RSQLite::SQLite(), dbname = "HotelReviews.sqlite")
newData = as_tibble(dbReadTable(con, "Scraped"))
dbDisconnect(con)


#load the models we defined earlier
load("originalMatrix.Rd")
load("trainedModels.Rd")


test <- tibble(review_body = paste(newData$review.summary, newData$review.text, " "), Consensus = NA)
test$review_body = CleanBody(test$review_body)

new_matrix <- create_matrix(test[, "review_body"],
                            language = "english",
                            removeNumbers = TRUE,
                            stemWords = FALSE,
                            removeSparseTerms = .998,
                            weighting = weightTfIdf,
                            originalMatrix = doc_matrix)


container <- create_container(new_matrix,
                              test$Consensus,
                              testSize = 1:nrow(test),
                              virgin = TRUE)



results <- classify_models(container, models)


test$MAXENTROPY_LABEL = results$MAXENTROPY_LABEL
test$SVM_LABEL = results$SVM_LABEL
test$FORESTS_LABEL = results$FORESTS_LABEL
test$TREE_LABEL = results$TREE_LABEL

for (i in c(1:nrow(test))) {
    consensus = 0
    consensus <- if (results$MAXENTROPY_LABEL[i] == +1) consensus + 1 else consensus - 1
    consensus <- if (results$SVM_LABEL[i] == +1) consensus + 1 else consensus - 1
    consensus <- if (results$FORESTS_LABEL[i] == +1)  consensus + 1 else consensus - 1
    consensus <- if (results$TREE_LABEL[i] == +1)  consensus + 1 else consensus - 1
    
    test$Consensus[i] = consensus
}



analytics <- create_analytics(container, results)
