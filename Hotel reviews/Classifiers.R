
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

    maReviews <- NULL
    matrixReviews <- NULL
}

DoNaiveBayes2 <- function(mat, reviews) {
    # train the model
    classifier = naiveBayes(mat[trainStart:trainEnd,], as.factor(reviews[1:trainEnd, 2]))

    # test the validity
    predicted = predict(classifier, mat[testStart:testEnd,]);
    predicted
    table(reviews[testStart:testEnd, 2], predicted)
    recall_accuracy(reviews[testStart:testEnd, 2], predicted)
}

DoMultipleClassifiers <- function(mat, reviews) {
    # build the data to specify response variable, training set, testing set.
    # using a RTextTools container
    container = create_container(mat, as.numeric(as.factor(reviews[, 2])),
                             trainSize = trainStart:trainEnd, testSize = testStart:testEnd, virgin = FALSE)
    algos = c("MAXENT", "SVM", "BAGGING", "TREE")

    models = train_models(container, algorithms = algos)
    results = classify_models(container, models)

    # model summary
    analytics = create_analytics(container, results)
    summary(analytics)
    head(analytics@document_summary)
    analytics@ensemble_summary

    return(container)
}

DoCrossValidation <- function(container, N) {
    set.seed(2014)

    cat("\n Cross validating Max entropy: \n")
    cross_validate(container, N, "MAXENT")
    cat("\n Cross validating SVM: \n")
    cross_validate(container, N, "SVM")
    cat("\n Cross validating Tree: \n")
    cross_validate(container, N, "TREE")
    cat("\n Cross validating Bagging: \n")
    cross_validate(container, N, "BAGGING")
}

# accuracy table
#cat("Showing truth tables for algorithms: ", algos)
#table(as.numeric(as.factor(reviews[testStart:testEnd, 2])), results[, "MAXENTROPY_LABEL"])
#table(as.numeric(as.factor(reviews[testStart:testEnd, 2])), results[, "SVM_LABEL"])
#table(as.numeric(as.factor(reviews[testStart:testEnd, 2])), results[, "BAGGING_LABEL"])
#table(as.numeric(as.factor(reviews[testStart:testEnd, 2])), results[, "TREE_LABEL"])

# recall accuracy
#cat("Max entropy accuracy: ",
#    recall_accuracy(as.numeric(as.factor(reviews[testStart:testEnd, 2])), results[, "MAXENTROPY_LABEL"])
#    , "\n")
#cat("SVM  accuracy: ",
#    recall_accuracy(as.numeric(as.factor(reviews[testStart:testEnd, 2])), results[, "SVM_LABEL"])
#    , "\n")
#
#cat("Bagging accuracy: ",
#    recall_accuracy(as.numeric(as.factor(reviews[testStart:testEnd, 2])), results[, "BAGGING_LABEL"])
#    , "\n")
#cat("Tree accuracy: ",
#    recall_accuracy(as.numeric(as.factor(reviews[testStart:testEnd, 2])), results[, "TREE_LABEL"])
#    , "\n\n\n")
