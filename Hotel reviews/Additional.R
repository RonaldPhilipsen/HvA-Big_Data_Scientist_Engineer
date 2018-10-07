# add_targets takes our feature matrix,
# the original data frame (with the documents in the same order) 
# and adds the dependent variable for model training. In this case it's our pre-labeled sentiment.
add_targets <- function(feature_matrix, df) {
    feature_matrix$sentiment <- df$sentiment
    return(feature_matrix)
}

# The ensemble function takes a list of prediction vectors, 
# each with a length equal to the number of documents, and takes a majority vote.
ensemble <- function(predictions) {
    votes <- matrix(0, length(predictions), length(predictions[[1]]))
    for (i in 1:length(predictions)) {
        votes[i,] <- ifelse(predictions[[i]] == "P", 1, 0)
    }
    vote_decision <- colSums(votes) / nrow(votes)
    vote_decision <- ifelse(vote_decision >= .5, "P", "N")

    return(vote_decision)
}


# Calculates accuracy, true negative, true positive, 
# and positive predictive value of a confusion matrix.
sensitivity <- function(confusion_matrix) {
    acc <- (confusion_matrix[1] + confusion_matrix[4]) / sum(confusion_matrix)
    tn <- (confusion_matrix[1]) / (confusion_matrix[3] + confusion_matrix[1])
    ppv <- confusion_matrix[4] / (confusion_matrix[4] + confusion_matrix[3])
    tp <- (confusion_matrix[4]) / (confusion_matrix[4] + confusion_matrix[2])
    return(list(accuracy = acc, specificity = tn, precision = ppv, sensitivity = tp))
}
