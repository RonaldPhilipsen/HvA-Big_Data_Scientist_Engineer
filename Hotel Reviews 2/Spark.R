require(sparklyr)

if (nrow(spark_installed_versions()) < 1) {
    spark_install(tail(spark_available_versions()$spark, n = 1))
}

sc <- spark_connect(master = "local")

dataframe  <- data.frame("Review" = "The pictures from online made it seem the room was big", "Review_Is_Positive" = 0 )
write.csv2(dataframe, file = "test.csv", row.names = FALSE)

dataframe.spark <- spark_read_csv(sc, 
                                  name =  "test",
                                  "test.csv",
                                  overwrite = TRUE,
                                  delimiter = ";")

dataframe.spark

dataframe.spark <- ft_tokenizer(dataframe.spark, input_col = "Review", output_col = "tokens")


#hotel.reviews.spark <- copy_to(sc, reviews.mixed, "hotel_reviews", overwrite = TRUE)
hotel.reviews.spark <- spark_read_csv(sc,
                                      name = "hotel_reviews",
                                      fn.mixed.reviews,
                                      overwrite = TRUE,
                                      delimiter = ";",
                                      )

hotel.reviews.spark <- filter(hotel.reviews.spark, nchar(Review) > 0) %>%
    mutate(Review = regexp_replace(Review, "[_\"\'():;,.!?\\-]", "")) %>%
    ft_tokenizer(input_col = "Review", output_col = "tokens", uid = random_string("tokenizer_"), "") %>%
    mutate(Word = explode(tokens))




#ft_tokenizer(input_col = "Review", output_col = "word_list")
spark_disconnect(sc)

conf <- spark_config()
conf$spark.sql.catalogImplementation <- "in-memory"
sc <- spark_connect(master = "local", config = conf)
