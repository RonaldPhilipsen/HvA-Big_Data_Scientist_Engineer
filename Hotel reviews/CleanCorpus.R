


cleanCorpus <- function(x) {
    x %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(removeNumbers)) %>%
    tm_map(content_transformer(removePunctuation)) %>%
    tm_map(cleanData) %>%
    tm_map(content_transformer(stripWhitespace)) %>%
    return()
}

cleanData <- content_transformer(function(data) {
    data %>% gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    gsub(pattern = "[\r\n]", replacement = "") %>%
    gsub(pattern = UnwantedPattern, replacement = "")
})