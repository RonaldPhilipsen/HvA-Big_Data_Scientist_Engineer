doNaiveBayes <- function(all_reviews) {

    matrixReviews = RTextTools::create_matrix(all_reviews[, 1],
                                          language = "english",
                                          removeStopwords = FALSE,
                                          removeNumbers = FALSE,
                                          stemWords = FALSE)
    maReviews = as.matrix(matrixReviews)

    classifier = naiveBayes(maReviews[1:1000,], as.factor(all_reviews[1:1000, 2]))
    predicted = predict(classifier, maReviews[101:200,])
    print(table(all_reviews[101:200, 2], predicted))
    RTextTools::recall_accuracy(all_reviews[101:200, 2], predicted)


    maReviews <- NULL
    matrixReviews <- NULL
}