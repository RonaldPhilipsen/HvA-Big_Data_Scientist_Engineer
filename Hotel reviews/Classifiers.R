
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

    classifier <- naiveBayes(Consensus ~ ., trainDf, laplace = 1)
    pred <- predict(classifier, newdata = testNB)
    pred

    table(predictions = pred, actual = df$Consensus)
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


RTTDemo <- function() {

    # SET THE SEED AND LOAD THE DATA
    set.seed(95616)
    data(USCongress)

    # CREATE THE DOCUMENT-TERM MATRIX AND WRAP THE DATA IN A CONTAINER
    doc_matrix <- create_matrix(USCongress$text, language = "english", removeNumbers = TRUE, stemWords = FALSE, removeSparseTerms = .998)
    container <- create_container(doc_matrix, USCongress$major, trainSize = 1:4000, testSize = 4001:4449, virgin = FALSE)

    # TRAIN THE ALGORITHMS USING THE CONTAINER
    # ALTERNATIVELY, train_models(container, c("SVM","GLMNET","MAXENT","SLDA","BOOSTING","BAGGING","RF","NNET","TREE"))
    SVM <- train_model(container, "SVM")
    GLMNET <- train_model(container, "GLMNET")
    MAXENT <- train_model(container, "MAXENT")
    SLDA <- train_model(container, "SLDA")
    BOOSTING <- train_model(container, "BOOSTING")
    BAGGING <- train_model(container, "BAGGING")
    RF <- train_model(container, "RF")
    NNET <- train_model(container, "NNET")
    TREE <- train_model(container, "TREE")

    # CLASSIFY THE TESTING DATA USING THE TRAINED MODELS.
    # ALTERNATIVELY, classify_models(container, list_of_trained_models)
    SVM_CLASSIFY <- classify_model(container, SVM)
    GLMNET_CLASSIFY <- classify_model(container, GLMNET)
    MAXENT_CLASSIFY <- classify_model(container, MAXENT)
    SLDA_CLASSIFY <- classify_model(container, SLDA)
    BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
    BAGGING_CLASSIFY <- classify_model(container, BAGGING)
    RF_CLASSIFY <- classify_model(container, RF)
    NNET_CLASSIFY <- classify_model(container, NNET)
    TREE_CLASSIFY <- classify_model(container, TREE)

    # CREATE THE ANALYTICS USING THE RESULTS FROM ALL THE ALGORITHMS
    analytics <- create_analytics(container, cbind(SVM_CLASSIFY, SLDA_CLASSIFY,
    BOOSTING_CLASSIFY, BAGGING_CLASSIFY, RF_CLASSIFY, GLMNET_CLASSIFY,
    NNET_CLASSIFY, TREE_CLASSIFY, MAXENT_CLASSIFY))
}

TrainClassifiers <- function(df) {
    df <- df[1:nrow(df)/10,]
    doc_matrix <- create_matrix(df$review_body,
                                language = "english",
                                removeNumbers = TRUE,
                                stemWords = FALSE,
                                removeSparseTerms = .998)
    save(doc_matrix, file = "originalMatrix.Rd")
    
    container <- create_container(doc_matrix,
                                  df$Consensus,
                                  trainSize = 1:4000,
                                  testSize = 4001:4449,
                                  virgin = FALSE)

    models <- train_models(container, algorithms = c("MAXENT", "SVM", "BAGGING", "RF", "TREE"))
    save(models, file = "trainedModels.Rd")

    rm(list = c("df", "doc_matrix", "container", "models"))
}