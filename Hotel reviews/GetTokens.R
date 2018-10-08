GetTokens <- function(dataFrame, Column, stop_pattern, undesirable_pattern) {
    dataFrame %>%  unnest_tokens(word, Column) %>%
    gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    gsub(pattern = "\\d+ ", replacement = "") %>%
    gsub(pattern = "[[:punct:]]", replacement = "") %>%
    gsub(pattern = "[\r\n]", replacement = "") %>%
    gsub(pattern = stop_pattern, replacement = "") %>%
    gsub(pattern = undesirable_pattern, replacement = "") %>%
    gsub(pattern = " {2,}", replacement = " ") %>%
    trimws() %>% return()
}

cleanData <- content_transformer(function(data) {
    data %>% gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    gsub(pattern = "[\r\n]", replacement = "") %>%
    gsub(pattern = stop_pattern, replacement = "") %>%
    gsub(pattern = undesirable_pattern, replacement = "") %>%
    gsub(pattern = " {2,}", replacement = " ") %>%
    trimws()
})