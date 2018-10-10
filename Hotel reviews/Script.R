required_packages <- c("magrittr", "tm", "mlr", "e1071", "dplyr", "kernlab", "lubridate", "dtplyr",
                       "readr", "ggplot2", "tidytext", "stringr", "tidyr", "scales", "broom", 
                       "SnowballC", "wordcloud", "reshape2")
lapply(required_packages, require, character.only = TRUE)

# Source a bunch of helper files
source("ReviewTools.R")
source("GetTokens.R")
source("Sentiment.R")

if (!file.exists("Cleaned.csv")) {
    source("CleanData.R")
}

df <- read.csv("Cleaned.csv", stringsAsFactors = FALSE)
colnames(df) <- gsub("X", "ID", colnames(df))

#randomise 
df <- df[sample(nrow(df)),]

reviews <- df %>%
    distinct(review_body, .keep_all = TRUE) %>%
    unnest_tokens(word, "review_body", drop = FALSE) %>%
    distinct(ID, word, .keep_all = TRUE) %>%
    filter(str_detect(word, "[^\\d]")) %>%
    group_by(word)

reviews$review_body <- NULL

review_words <- reviews %>%
    mutate(word_total = n()) %>%
    ungroup()



getWordCount(review_words)


#get the word counts per word
bing_word_counts <- reviews %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

#get the contributions of single words for a given set of sentiments
get_contributions(reviews, AFINN)

#source("NaiveBayes.R")
#source("Ksvm.R")


