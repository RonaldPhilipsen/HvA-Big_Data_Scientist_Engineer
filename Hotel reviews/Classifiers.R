SaveMatrix <- function(df) {
    print("Creating the Document term frequency matrix:  this might take a while")

    doc_matrix <- create_matrix(df$review_body,
                                language = "english",
                                removeNumbers = TRUE,
                                stemWords = TRUE,
                                removeSparseTerms = .99)

    save(doc_matrix, file = "originalMatrix.Rd")
    rm(list = c("doc_matrix"))
}


TrainClassifiers <- function(df, doc_matrix, savemodelsInSingleFile) {
    print("Creating Container:")
    training_container <- create_container(doc_matrix,
                                  df$Consensus,
                                  trainSize = 1:30000,
                                  testSize = 30001:35000,
                                  virgin = FALSE)

    save(training_container, file = "trainingContainer.Rd")

    algos <- as.vector(c("BOOSTING",
                         "GLMNET",
                         "RF",
                         "SVM"))

    models = train_models(training_container, algorithms = algos)
    results = classify_models(training_container, models)
    save(models, file = "Models.Rd")
    
    # model summary
    analytics = create_analytics(training_container, results)
    save(analytics, )
    summary(analytics)
    head(analytics@document_summary)
    analytics@ensemble_summary

    write.csv(analytics@document_summary, "DocumentSummary.csv")
    write.csv(analytics@topic_summary, "TopicSummary.csv")
    write.csv(analytics@algorithm_summary, "AlgorithmSummary.csv")
    write.csv(analytics@ensemble_summary, "EnsembleSummary.csv")
       
    rm(list = c("df", "doc_matrix", "training_container", "models", "results"))
}

