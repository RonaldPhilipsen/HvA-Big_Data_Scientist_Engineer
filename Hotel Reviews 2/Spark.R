devtools::install_github("rstudio/sparklyr")
library(sparklyr)

if (nrow(spark_installed_versions()) < 1) {
    spark_install(tail(spark_available_versions()$spark, n = 1))
}

sc <- spark_connect(master = "local")

#hotel.reviews.spark <- copy_to(sc, reviews.mixed, "hotel_reviews", overwrite = TRUE)
hotel.reviews.spark <- spark_read_csv(sc,
                                      hotel.reviews.mixed.csv,
                                      name = "hotel_reviews",
                                      overwrite = TRUE,
                                      delimiter = ";",
                                      )

all_words <- ft_tokenizer(input_col = )

spark_disconnect(sc)