#file names
fn.hotel.reviews <- "hotel_Reviews_Enriched.csv"
fn.negative.reviews <- "Review_neg.csv"
fn.positive.reviews <- "Review_pos.csv"
fn.mixed.reviews <- "Review_mix.csv"

# settings for mongo
numReviewsToDl = 10000

# Keras settings
vocab_size <- 5000 #number of words
batch_size <- 512  #number of words to process in at once