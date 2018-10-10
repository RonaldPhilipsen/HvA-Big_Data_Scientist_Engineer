#cleanData <- function() {
    # Define undesirable patterns, such as stop_pattern
    fuckThisShit <- as.vector(c(stopwords("en"), undesirable_words))
    undesirable_words <- c("positive", "negative",
                           "and", "it", "in", "hotel", 
                           "staff", "location", "breakfast", 
                           "bathroom","room", "bed" )
    stop_pattern <- paste0("\\b(", paste0(fuckThisShit, collapse = "|"), ")\\b")
    AFINN <- get_sentiments("afinn")

    # Read in the hotel reviews
    df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE)

    # only use rows that have completely been filled in
    #limit the amount of rows we use for test purposes
    df <- df[complete.cases(df),][1:(nrow(df) / 100),]

    #get the dates as dates insteadof strings
    df$Review_Date <- as.Date(df$Review_Date, format = "%m/%d/%Y")

    #display the amount of reviews per week
    getReviewsPerWeek(df)

    # split and clean the data
    pr <- as.data.frame(t(rbind(review_body = df$Positive_Review, Consensus = as.integer(1))))
    nr <- as.data.frame(t(rbind(review_body = df$Negative_Review, Consensus = as.integer(-1))))
    df <- rbind(pr, nr)

    df$review_body <- as.vector(cleanBody(df$review_body))

    write.csv(df, file = "Cleaned.csv")
#}
