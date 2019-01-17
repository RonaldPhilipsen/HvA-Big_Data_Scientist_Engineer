source("Tools.r")
list.of.packages <- c("devtools", "ffbase", "shiny", "tibble", "mongolite", "tm", "magrittr", "sparklyr","dplyr", "MLlib")
Require.packages(list.of.packages)

hotel.reviews.collection <- mongo(collection = "hotel_reviews", db = "hotel_reviews", url = "mongodb://localhost")
hotel.reviews.cleaned <- hotel.reviews.collection$find();

hotel.reviews.dataset.csv = "hotel_Reviews_Enriched.csv"
hotel.reviews.negative.csv = "Review_neg.csv"
hotel.reviews.positive.csv = "Review_pos.csv"
hotel.reviews.mixed.csv = "Review_mix.csv"


if (!exists(hotel.reviews.cleaned)) {
    hotel.reviews.raw <- read.csv(file = hotel.reviews.dataset.csv, header = TRUE, quote = "\"", dec = ".")
    source("CleanData.R")
    hotel.reviews.cleaned <- CleanData()
}


if (!file.exists(hotel.reviews.negative.csv) || !file.exists(hotel.reviews.positive.csv)) {
    source("Mongo.R")
}

hotel.reviews.positive <- read.csv2(file = hotel.reviews.positive.csv, sep = ",")[,2:3] %>% as_tibble()
hotel.reviews.negative <- read.csv2(file = hotel.reviews.negative.csv, sep = ",")[, 2:3] %>% as_tibble()

reviews.mixed <- bind_rows(hotel.reviews.positive, hotel.reviews.negative)
reviews.mixed <- reviews.mixed[sample(nrow(reviews.mixed)),] 
reviews.mixed <- reviews.mixed[sample(nrow(reviews.mixed)),]

write.csv2(reviews.mixed, file = hotel.reviews.mixed.csv)

## Detect data types 
#m <- detect_dm_csv(reviews, header = T)
#paste(m$columns$type, collapse = ",")
#con <- laf_open(m)
#ffdf <- laf_to_ffdf(con)


#read.csv.ffdf(file = reviews, header = TRUE, quote = "\"", dec = ".")





