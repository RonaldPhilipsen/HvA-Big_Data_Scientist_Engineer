SaveMatrix <- function(df) {
    print("Creating the Document term frequency matrix:  this might take a while")

    doc_matrix <- create_matrix(df$review_body,
                                language = "english",
                                removeNumbers = TRUE,
                                stemWords = TRUE,
                                removeSparseTerms = .99)

    save(df, file = "originalDataFrame.Rd")
    save(doc_matrix, file = "originalMatrix.Rd")
    rm(list = c("doc_matrix"))
}

CreateContainer <- function(df, doc_matrix) {
    print("Creating Container; this may take a while")

    train <- 1: as.integer(nrow(df) * 0.7)
    test <- as.integer(nrow(df) * 0.7): nrow(df)

    if (nrow(df) < max(test)) {
        print("Error: data frame was smaller than test size")
        return()
    }

    training_container <- create_container(doc_matrix,
                                  df$Consensus,
                                  trainSize = train,
                                  testSize = test,
                                  virgin = FALSE)

    save(training_container, file = "trainingContainer.Rd")

    return(training_container)
}


TrainClassifiers <- function(training_container) {

    algos <- as.vector(c("BOOSTING", "GLMNET", "RF", "SVM"))

    print("Training models: this may take a while")

    models = train_models(training_container, algorithms = algos)
    results = classify_models(training_container, models)
    save(models, file = "Models.Rd")

    # model summary
    analytics = create_analytics(training_container, results)
    save(analytics, file = "Analytics.Rd")

    rm(list = c("training_container", "models", "results", "analytics"))
}

