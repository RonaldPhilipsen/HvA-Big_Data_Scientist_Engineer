require(mongolite)

hotel.reviews.collection <- mongo(collection = "hotel_reviews",
                                  db = "hotel_reviews",
                                  url = "mongodb://localhost")

getPositiveReviews <- function(numReviews) {
    hotel.reviews.positive <- hotel.reviews.collection$find(query = '{"Review_Is_Positive" : 1}',
                                    fields = '{
                                                "_id" : false,
                                                "Positive_Review":"$Positive_Review",
                                                "Review_Is_Positive":"$Review_Is_Positive"
                                              }',
                                    limit = numReviews,
                                )

    hotel.reviews.positive$Review <- hotel.reviews.positive$Positive_Review
    hotel.reviews.positive$Positive_Review <- NULL

    return(hotel.reviews.positive)
}

getNegativeReviews <- function(numReviews) {
    hotel.reviews.negative <- hotel.reviews.collection$find(query = '{"Review_Is_Positive" : 0}',
                                    fields = '{
                                                "_id" : false,
                                                "Negative_Review":"$Negative_Review",
                                                "Review_Is_Positive":"$Review_Is_Positive"
                                              }',
                                    limit = numReviews
                                ) %>% as_tibble()

    hotel.reviews.negative$Review <- hotel.reviews.negative$Negative_Review
    hotel.reviews.negative$Negative_Review <- NULL
    return(hotel.reviews.negative)
}







