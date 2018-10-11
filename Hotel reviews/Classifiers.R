
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
    classifier = naiveBayes(mat[trainStart:trainEnd,], as.factor(reviews[trainStart:trainEnd, 2]))

    # test the validity
    predicted = predict(classifier, mat[testStart:testEnd,]);
    predicted
    table(reviews[testStart:testEnd, 2], predicted)
    recall_accuracy(reviews[testStart:testEnd, 2], predicted)
}

DoNaiveBayes3 <- function(x) {
    corpus <- Corpus(VectorSource(df$review_body))
    dtm <- DocumentTermMatrix(corpus)

    trainCorpus <- corpus[1:250]
    trainDtm <- dtm[1:250,]
    trainDf <- df[1:250,]

    testCorpus <- corpus[251:500]
    testDtm <- dtm[251:500,]
    testdf <- df[251:500,]

    fiveFreq <- findFreqTerms(trainDtm)

    testDtm <- DocumentTermMatrix(trainCorpus, control = list(dictionary = fiveFreq))
    trainDtm <- DocumentTermMatrix(testCorpus, control = list(dictionary = fiveFreq))

    classifier <- naiveBayes(Consensus ~., trainDf, laplace = 1)
    pred <- predict(classifier, newdata = testNB)
    pred    

    table(predictions= pred, actual = df$Consensus)
}


DoMultipleClassifiers <- function(mat, reviews, doCrossValidation) {
    # build the data to specify response variable, training set, testing set.
    # using a RTextTools container
    container = create_container(mat,
                                 as.numeric(as.factor(reviews[, 2])),
                                trainSize = trainStart:trainEnd,
                                testSize = testStart:testEnd,
                                virgin = FALSE)
    algos = c("MAXENT", "SVM", "BAGGING", "RF","TREE")

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
