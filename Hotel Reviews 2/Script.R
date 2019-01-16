require(devtools)
devtools::install_github("rstudio/sparklyr")

source("Tools.r")
list.of.packages <- c("ffbase", "shiny", "tibble", "mongolite", "tm", "magrittr", "sparklyr")
Require.packages(list.of.packages)


reviews = "hotel_Reviews_Enriched.csv"

## Detect data types 
#m <- detect_dm_csv(reviews, header = T)
#paste(m$columns$type, collapse = ",")
#con <- laf_open(m)
#ffdf <- laf_to_ffdf(con)

hotel.reviews.raw <- read.csv(file = reviews, header = TRUE, quote = "\"", dec = ".")

#read.csv.ffdf(file = reviews, header = TRUE, quote = "\"", dec = ".")


source("CleanData.R")
hotel.reviews.cleaned <- CleanData()

if (!file.exists("Review_neg.csv") || !file.exists("Review_pos.csv")) {
    source("Mongo.R")
}

