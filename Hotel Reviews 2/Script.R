list.of.packages <- c("ffbase", "shiny", "tibble", "mongolite", "LaF")

false <- FALSE
true <- TRUE

reviews = "hotel_Reviews_Enriched.csv"

source("Tools.r")
Require.packages(list.of.packages)

# Detect data types 
#m <- detect_dm_csv(reviews, header = T)
#con <- laf_open(m)
# FFDF is null
#ffdf <- laf_to_ffdf(con)



source("CleanData.R")

hotel.reviews.collection <- mongo(collection = "hotel_reviews", db = "hotel_reviews", url = "mongodb://localhost")
hotel.reviews.collection$insert(hotel.reviews.raw)