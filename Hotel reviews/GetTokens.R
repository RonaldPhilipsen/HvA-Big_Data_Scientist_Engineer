cleanBody <- function(words) {
    words %>%
    tolower() %>%
    gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    gsub(pattern = "\\d+ ", replacement = "") %>%
    gsub(pattern = "[[:punct:]]", replacement = "") %>%
    gsub(pattern = "[\r\n]", replacement = "") %>%
    gsub(pattern = stop_pattern, replacement = "") %>%
    gsub(pattern = undesirable_pattern, replacement = "") %>%
    gsub(pattern = " {2,}", replacement = " ") %>%
    trimws() %>%
    return()
}

cleanData <- content_transformer(function(data) {
    data %>% gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    gsub(pattern = "[\r\n]", replacement = "") %>%
    gsub(pattern = stop_pattern, replacement = "") %>%
    gsub(pattern = undesirable_pattern, replacement = "")
})

cleanCorpus <- function(x) {
    x %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(removeNumbers)) %>%
    tm_map(content_transformer(removePunctuation)) %>%
    tm_map(cleanData) %>%
    tm_map(content_transformer(stripWhitespace)) %>%
    return()
}