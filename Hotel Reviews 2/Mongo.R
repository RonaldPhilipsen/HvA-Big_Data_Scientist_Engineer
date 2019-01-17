hotel.reviews.collection <- mongo(collection = "hotel_reviews", db = "hotel_reviews", url = "mongodb://localhost")
hotel.reviews.collection$insert(hotel.reviews.cleaned)

hotel.reviews.positive <- hotel.reviews.collection$find(query = '{"Review_Is_Positive" : 1}',
                                    fields = '{
                                                "_id" : false,
                                                "Positive_Review":"$Positive_Review",
                                                "Review_Is_Positive":"$Review_Is_Positive"
                                              }',
                                    limit = 10000
                                ) %>% as_tibble()

hotel.reviews.positive$Review <- hotel.reviews.positive$Positive_Review;
hotel.reviews.positive$Positive_Review <- NULL;

write.csv(hotel.reviews.positive, file = hotel.reviews.positive.csv)


hotel.reviews.negative <- hotel.reviews.collection$find(query = '{"Review_Is_Positive" : 0}',
                                    fields = '{
                                                "_id" : false,
                                                "Negative_Review":"$Negative_Review",
                                                "Review_Is_Positive":"$Review_Is_Positive"
                                              }',
                                    limit = 10000
                                ) %>% as_tibble()

hotel.reviews.negative$Review <- hotel.reviews.negative$Negative_Review;
hotel.reviews.negative$Negative_Review <- NULL;

write.csv(hotel.reviews.negative , file = hotel.reviews.negative.csv)
