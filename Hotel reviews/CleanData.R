#cleanData <- function() {
UndesirableWords <- c("positive", "negative",
                           "and", "it", "in", "hotel",
                           "staff", "location", "breakfast",
                           "bathroom", "room", "bed", "na", "nana")

unwantedWords <- as.vector(c(stopwords("en"), UndesirableWords))
UnwantedPattern <- paste0("\\b(", paste0(unwantedWords, collapse = "|"), ")\\b")


CleanBody <- function(words) {
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


# Define undesirable patterns, such as UnwantedPattern
CleanCSV <- function(filename) {
       
    # Read in the hotel reviews
    df <- read.csv(filename, stringsAsFactors = FALSE)

    # only use rows that have completely been filled in
    df <- df[complete.cases(df),]

    #get the dates as dates insteadof strings
    df$Review_Date <- as.Date(df$Review_Date, format = "%m/%d/%Y")
    #display the amount of reviews per week
    getReviewsPerWeek(df)

    # split and clean the data
    pr <- as.data.frame(t(rbind(review_body = df$Positive_Review, Consensus = as.integer(1))))
    nr <- as.data.frame(t(rbind(review_body = df$Negative_Review, Consensus = as.integer(-1))))
    df <- rbind(pr, nr)

    df$review_body <- as.vector(CleanBody(df$review_body))
    SaveDataFrameToDB(database, "Original", df, doAppend = FALSE)
}
#}
