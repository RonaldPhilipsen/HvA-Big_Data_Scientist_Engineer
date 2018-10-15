DoNaiveBayes <- function(df) {

    matrixReviews = RTextTools::create_matrix(df[, "review_body"],
                                          language = "english",
                                          removeStopwords = FALSE,
                                          removeNumbers = FALSE,
                                          stemWords = FALSE)
    maReviews = as.matrix(matrixReviews)

    classifier = naiveBayes(maReviews, as.factor(df[1:ncol(df) / 2, "Consensus"]))
    predicted = predict(classifier, maReviews[ncol(df) / 2:ncol(df),])


    print(table(df[ncol(df) / 2:ncol(df), "Consensus"], predicted))
    print(RTextTools::recall_accuracy(df[ncol(df) / 2:ncol(df), "Consensus"], predicted))

}



DoMultipleClassifiers <- function(mat, reviews, doCrossValidation) {
    # build the data to specify response variable, training set, testing set.
    # using a RTextTools container
    container = create_container(mat,
                                 as.numeric(as.factor(reviews[, 2])),
                                trainSize = trainStart:trainEnd,
                                testSize = testStart:testEnd,
                                virgin = FALSE)
    algos = c("MAXENT", "SVM", "BAGGING", "RF", "TREE")

    models = train_models(container, algorithms = algos)
    results = classify_models(container, models)

    # model summary
    analytics = create_analytics(container, results)
    summary(analytics)
    head(analytics@document_summary)
    analytics@ensemble_summary


    if (doCrossValidation) {
        DoCrossValidation(completedContainer, 4)
    }

    return(models)
}

DoCrossValidation <- function(container, N) {
    set.seed(2014)

    cat("\n Cross validating Max entropy: \n")
    cross_validate(container, N, "MAXENT")
    cat("\n Cross validating SVM: \n")
    cross_validate(container, N, "SVM")
    cat("\n Cross validating Tree: \n")
    cross_validate(container, N, "TREE")
    cat("\n Cross validating RF: \n")
    cross_validate(container, N, "RF")
    cat("\n Cross validating Bagging: \n")
    cross_validate(container, N, "BAGGING")
}


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


TrainClassifiers <- function(df, doc_matrix) {
    print("Creating Container:")
    training_container <- create_container(doc_matrix,
                                  df$Consensus,
                                  trainSize = 1:30000,
                                  testSize = 30001:35000,
                                  virgin = FALSE)

    save(training_container, file = "trainingContainer.Rd")

    algos <- as.vector(c("BOOSTING", "GLMNET", "MAXENT", "NNET", "RF", "SLDA", "SVM"))
         
    boosting <- train_model(training_container, algos[1])
    save(boosting, file = paste0("Model_", algos[1], ".Rd"))

    glmnetVignette <- train_model(training_container, algos[2])
    save(glmnetVignette, file = paste0("Model_", algos[2], ".Rd"))

    maximumEntropy <- train_model(training_container, algos[3])
    save(maximumEntropy, file = paste0("Model_", algos[3], ".Rd"))

    neuralNet <- train_model(training_container, algos[4])
    save(neuralNet, file = paste0("Model_", algos[4], ".Rd"))

    randomForest <- train_model(training_container, algos[5])
    save(randomForest, file = paste0("Model_", algos[5], ".Rd"))

    supervisedLDA <- train_model(training_container, algos[6])
    save(supervisedLDA, file = paste0("Model_", algos[6], ".Rd"))

    supportVectorMachine <- train_model(training_container, algos[7])
    save(supportVectorMachine, file = paste0("Model_", algos[7], ".Rd"))
    
    #models <- train_models(training_container, algorithms <- algos)

    rm(list = c("df", "doc_matrix", "training_container",
    "boosting", "glmnetVignette", "maximumEntropy", 
    "neuralNet", "randomForest", "supervisedLDA",
    "supportVectorMachine"))
}

TrainSingleClassifier <- function(training_container, algorithm) {
    models <- train_model(training_container, algorithm)
    save(models, file = paste0(algorithm,".rd"))
}