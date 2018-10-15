getReviewsPerWeek <- function(df) {
    df %>%
    count(Week = round_date(Review_Date, "week")) %>%
    ggplot(aes(Week, n)) +
    geom_line() +
    ggtitle("Reviews/week")
}

getWordCount <- function(review_words) {
    word_counts <- review_words %>%
    count(word, sort = TRUE)

    word_counts %>%
    head(50) %>%
    #mutate(word = wordStem(word)) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    coord_flip() +
    scale_y_continuous(labels = comma_format()) +
    labs(title = "Most common words in review texts",
         subtitle = "stop words and undesirable words removed and stemmed",
        y = "# of uses")

    return(word_counts)
}

DoIfNotExists <- function(filename, FUN, params) {
    if (!file.exists(filename)) {
        FUN(params)
    }
}

SaveDataFrameToDB <- function(database, table, df, doAppend) {
    con <- dbConnect(RSQLite::SQLite(), dbname = database)
    dbWriteTable(con, table, df, overwrite = !doAppend, append = doAppend)
    dbDisconnect(con)
}

ExecuteSQL <- function(database, sqlQuery) {
    con <- dbConnect(RSQLite::SQLite(), dbname = database)

    query <- dbSendQuery(con, sqlQuery)
    result <- dbFetch(query)
    dbClearResult(query)
    dbDisconnect(con)

    return(result)
}