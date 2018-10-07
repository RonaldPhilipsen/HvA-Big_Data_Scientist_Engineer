# pass a dataframe to this
# returns a corpus
tokenize <- function(x) {
    doc <- tolower(x)

    # Remove words with more than 3 numbers in them (they overwhelm the corpus, and are uninformative)
    doc <- gsub("[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", "", doc)

    # remove numbers from the corpus, they're confusing
    doc <- gsub("\\d+ ", "", x)

    # Remove all punctuation
    doc <- gsub("[[:punct:]]", "", doc)

    # Remove all newline characters
    doc <- gsub("[\r\n]", "", doc)

    # replace stuff
    doc <- gsub(stop_pattern, "", doc)
    doc <- gsub(undesirable_pattern, "", doc)

    # Replace whitespace longer than 1 space with a single space
    doc <- gsub(" {2,}", " ", doc)

    doc <- trimws(doc)
    print(doc)

    doc_words <- strsplit(doc, " ")

    return(doc_words)
}




