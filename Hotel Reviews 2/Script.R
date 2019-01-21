source("Settings.R")
source("Tools.R")
source("Mongo.R")
list.of.packages <- c("devtools", "tibble", "magrittr", "dplyr", "sparklyr", "shiny", "leaflet")
Require.packages(list.of.packages)



if (!file.exists(fn.mixed.reviews)) {
    hotel.reviews <- hotel.reviews.collection$find();

    if (!exists(hotel.reviews)) {
        hotel.reviews.raw <- read.csv(file = hotel.reviews.dataset.csv, header = TRUE, quote = "\"", dec = ".")
        hotel.reviews.collection$insert(hotel.reviews.raw)

    }

    if (!file.exists(fn.positive.reviews)) {
        hotel.reviews.positive <- getPositiveReviews();
        write.csv2(hotel.reviews.positive, file = fn.positive.reviews, row.names = FALSE)
    }

    if (!file.exists(fn.negative.reviews)) {
        hotel.reviews.negative <- getNegativeReviews();
        write.csv2(hotel.reviews.negative, file = fn.negative.reviews, row.names = FALSE)
    }
}

if (file.exists(fn.mixed.reviews)) {
    reviews.mixed <- read.csv2(fn.mixed.reviews)
} else {
    hotel.reviews.positive <- read.csv2(file = fn.positive.reviews, sep = ";") %>% as_tibble()
    hotel.reviews.negative <- read.csv2(file = fn.negative.reviews, sep = ";") %>% as_tibble()

    reviews.mixed <- bind_rows(hotel.reviews.positive, hotel.reviews.negative)
    reviews.mixed <- reviews.mixed[sample(nrow(reviews.mixed)),]
    reviews.mixed <- reviews.mixed[sample(nrow(reviews.mixed)),]

    write.csv2(reviews.mixed, file = fn.mixed.reviews, row.names = FALSE)
}

path = paste0("R -e \"shiny::runApp(\'", getwd(),"/shiny\', launch.browser = TRUE)\"")
system(path, wait = FALSE)

runApp(appDir = (getwd() + "/shiny/"),
        port = 80,
        launch.browser = TRUE,
        host = "127.0.0.1",
        display.mode = "auto",
        test.mode = FALSE)

