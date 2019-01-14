UndesirableWords <- c("positive", "negative",
                           "and", "it", "in", "hotel",
                           "staff", "location", "breakfast",
                           "bathroom", "room", "bed", "na", "nana")

unwantedWords <- as.vector(c(stopwords("en"), UndesirableWords))
UnwantedPattern <- paste0("\\b(", paste0(unwantedWords, collapse = "|"), ")\\b")

CleanData <- function() {
    hotel.reviews.cleaned <- hotel.reviews.raw

    hotel.reviews.cleaned$Positive_Review <- as.vector(CleanBody(hotel.reviews.raw$Positive_Review))
    hotel.reviews.cleaned$Negative_Review <- as.vector(CleanBody(hotel.reviews.raw$Negative_Review))

    write.csv(hotel.reviews.cleaned$Positive_Review, file = "Review_pos.csv")
    write.csv(hotel.reviews.cleaned$Negative_Review, file = "Review_neg.csv")
}

CleanString <- function(words) {
    # convert the text to a vector for processing
    as.vector(words) %>%
    # convert the text into ASCII, removing all unicode characters
    iconv(to = "ASCII", sub = "") %>%
    # Make all text lowercase
    tolower() %>%
    # remove decimals longer than 3, they confuse the matrix
    gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    # remove all numbers
    gsub(pattern = "\\d+ ", replacement = "") %>%
    # remove the stop words
    gsub(pattern = UnwantedPattern, replacement = "") %>%
    # remove punctuation
    gsub(pattern = "[[:punct:]]", replacement = "") %>%
    # remove newlines
    gsub(pattern = "[\r\n]", replacement = "") %>%
    # remove single letters
    gsub(pattern = "\b[a-zA-Z]\b", replacement = "") %>%
    # remove whitespace longer than one
    gsub(pattern = " {2,}", replacement = " ") %>%
    # make sure we save the text as a factor
    as.factor %>%
    # trim whitespace at the front and the end
    trimws() %>%
    #return the cleaned text
    return()
}
