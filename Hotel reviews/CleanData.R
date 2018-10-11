#cleanData <- function() {

CleanBody <- function(words) {
    as.vector(words) %>%
    stemDocument() %>%
    tolower() %>%
    #remove decimals longer than 3, they confuse the matrix
    gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    #remove all numbers
    gsub(pattern = "\\d+ ", replacement = "") %>%
    #remove punctuation
    gsub(pattern = "[[:punct:]]", replacement = "") %>%
    #remove newlines
    gsub(pattern = "[\r\n]", replacement = "") %>%
    #remove the stop words
    gsub(pattern = UnwantedPattern, replacement = "") %>%
    #remove single letters
    gsub(pattern = "\b[a-zA-Z]\b", replacement = "") %>%
    #remove whitespace longer than one
    gsub(pattern = " {2,}", replacement = " ") %>%
    #trim whitespace at the front and the end
    as.factor %>%
    trimws() %>%
    return()
}


# Define undesirable patterns, such as UnwantedPattern

UndesirableWords <- c("positive", "negative",
                           "and", "it", "in", "hotel",
                           "staff", "location", "breakfast",
                           "bathroom", "room", "bed")

unwantedWords <- as.vector(c(stopwords("en"), UndesirableWords))

UnwantedPattern <- paste0("\\b(", paste0(unwantedWords, collapse = "|"), ")\\b")

# Read in the hotel reviews
df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE)

# only use rows that have completely been filled in
#limit the amount of rows we use for test purposes
df <- df[complete.cases(df),][1:(nrow(df)),]

#get the dates as dates insteadof strings
df$Review_Date <- as.Date(df$Review_Date, format = "%m/%d/%Y")

#display the amount of reviews per week
getReviewsPerWeek(df)

# split and clean the data
pr <- as.data.frame(t(rbind(review_body = df$Positive_Review, Consensus = as.integer(1))))
nr <- as.data.frame(t(rbind(review_body = df$Negative_Review, Consensus = as.integer(-1))))
df <- rbind(pr, nr)

df$review_body <- as.vector(CleanBody(df$review_body))

write.csv(df, file = "Cleaned.csv")
#}
