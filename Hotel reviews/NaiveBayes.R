matrixReviews = RTextTools::create_matrix(reviews[, "review_body"],
                                          language = "english",
                                          removeStopwords = FALSE,
                                          removeNumbers = FALSE,
                                          stemWords = FALSE)
maReviews = as.matrix(matrixReviews)

classifier = naiveBayes(maReviews, as.factor(reviews[1:1000, "Consensus"]))
predicted = predict(classifier, maReviews[101:200,])
print(table(reviews[101:200, "Consensus"], predicted))
RTextTools::recall_accuracy(all_reviews[101:200, "Consensus"], predicted)


maReviews <- NULL
matrixReviews <- NULL
