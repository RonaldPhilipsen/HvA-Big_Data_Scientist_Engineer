library(magrittr)
library(tidytext)
library(dplyr)
library(dtplyr)
library(tm)

finn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
undesirable_words <- c("No positive", "No Negative", "AND", "it", "in")
stop_words <- stopwords(kind = "en")

# Regex pattern for removing stop words
stop_pattern <- paste0("\\b(", paste0(stopwords("en"), collapse = "|"), ")\\b")
undesirable_pattern <- paste0("\\b(", paste0(undesirable_words, collapse = "|"), ")\\b")

# Takes in the token list and the size of the corpus
corpus_freq <- function(tokens, corpus_size = NULL, word_list = NULL) {
    all_words <- do.call(c, tokens)

    if (is.null(word_list) & !is.null(corpus_size)) {
        corpusfreq <- data.frame(table(all_words))
        names(corpusfreq) <- c("Word", "Freq")
        corpusfreq$Word <- as.character(corpusfreq$Word)
        corpusfreq$Freq <- as.numeric(corpusfreq$Freq)
        corpusfreq <- corpusfreq[order(-corpusfreq$Freq),]
        corpusfreq <- corpusfreq[1:corpus_size,]
        corpusfreq <- corpusfreq[order(corpusfreq$Word),]
    }
    else {
        #Else it is assumed a pre-compiled word list has been passed into the function
        corpusfreq <- data.frame(word_list)
        names(corpusfreq) <- c("Word")
    }

    # N docs is where we will store the document frequency (I.E how many documents a word appears in)
    # We'll need this to calculate TF-IDF
    corpusfreq$n_docs <- 0

    # For every vector of words in our tokenized list, count how many times each word in our corpus occurs
    for (token_list in tokens) {
        t <- data.frame(table(token_list))
        names(t) <- c("Word", "n_docs")
        t$n_docs <- 1
        t_freq <- merge(x = corpusfreq, y = t, by = "Word", all.x = TRUE)
        t_freq$n_docs.y[is.na(t_freq$n_docs.y)] <- 0
        corpusfreq$n_docs <- corpusfreq$n_docs + t_freq$n_docs.y
    }
    return(corpusfreq)
}


tfidf <- function(document, corpus) {
    #Create a data frame out of a single document and its word frequency
    doc_f <- data.frame(unlist(table(document)))
    names(doc_f) <- c("Word", "Freq")

    #Get a data frame of the words in the corpus found in the current document
    in_doc <- intersect(doc_f$Word, corpus$Word)
    doc_f <- doc_f[doc_f$Word %in% in_doc,]

    #Get a data frame of the words in the corpus not found in the current document
    #Set their frequency to 0
    not_in_doc <- data.frame(Word = setdiff(corpus$Word, document))
    not_in_doc$Freq <- 0

    #Bind our two data frames, we now have frequencies for the words that are in our corpus, and 0s everywhere else
    tf <- rbind(doc_f, not_in_doc)
    tf$Word <- as.character(tf$Word)
    tf$Freq <- as.numeric(tf$Freq)

    #Order alphabetically again so it remains compatible with our corpus data frame
    tf <- tf[order(tf$Word),]

    #Calculate the tfidf
    #log1p is the same as log(1+___)
    log_freq <- log1p(tf$Freq)
    log_doc_freq <- log1p(nrow(corpus) / corpus$n_docs)

    
    tf$tfidf <- log_freq * log_doc_freq


    #Divide by zero errors get NA values, but should be 0s
    tf$tfidf[is.na(tf$tfidf)] <- 0
    return(tf)
}

# This function takes a token_list (the output of tokenize) 
# and either a corpus size to create a new corpus, or a pre-compiled corpus
get_feature_vectors <- function(tokens_list, corpus_size = 1500, corpus = NULL) {
    if (is.null(corpus)) {
        corpus <- corpus_freq(tokens_list, corpus_size = corpus_size)
    }

    #Our feature matrix starts out as an all 0 matrix with N by C dimensions
    feature_matrix <- matrix(0, length(tokens_list), nrow(corpus))

    #For every document in our tokenized list, calculate the tfidf feature vector, and put it into our feature matrix row-wise
    for (i in 1:length(tokens_list)) {
        feature_vector <- tfidf(tokens_list[[i]], corpus)$tfidf
        feature_matrix[i, 1:nrow(corpus)] <- feature_vector
    }

    #The column names are the same as the alphabetical list of words in our corpus
    #Unnecessary step, but useful for examining the resulting feature matrix
    colnames(feature_matrix) <- corpus$Word
    return(data.frame(feature_matrix))
}




df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE)

df$Combined_Review <- paste(df$Positive_Review, df$Negative_Review)
combined <- group_by(df, Hotel_Name) %>% select(Hotel_Name, Combined_Review)

combined$ID <- seq.int(nrow(combined))

source("Corpus.r")

tokens <- tokenize(combined[1, "Combined_Review"])
reviewCorpus <- corpus_freq(tokens, 149)
#vectors <- get_feature_vectors(tokens, corpus = reviewCorpus)

tokens <- combined %>%
    unnest_tokens(word, Combined_Review) %>%
    gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    gsub(pattern = "\\d+ ", replacement = "") %>%
    gsub(pattern = "[[:punct:]]", replacement = "") %>%
    gsub(pattern = "[\r\n]", replacement = "") %>%
    gsub(pattern = stop_pattern, replacement = "") %>%
    gsub(pattern = undesirable_pattern, replacement = "") %>%
    gsub(pattern = " {2,}", replacement = " ") %>%
    trimws() %>%
    count(ID, word, sort = TRUE)

source("Additional.r")

#head(tokens)
#head(vectors)
#head(corpusfreq)


