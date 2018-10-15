ScrapeHotels <- function() {
    url.base = "https://uk.hotels.com/ho107128-tr-p"

    #we want a sufficient amount of reviews
    numPages <- read_html(paste0(url.base, 0)) %>%
                                        html_nodes(".review-pagination span") %>%
                                        html_text()

    lastPage <- as.integer(str_extract_all(numPages, "\\(?[0-9,.]+\\)?")[[1]][2])

    print(paste0("Reading: ", lastPage, " Pages"))

    for (i in c(1:lastPage)) {
        url <- paste(url.base, i, sep = "")
        # get every review card, usually 50 per page 
        reviews <- read_html(url) %>%
        html_nodes(".review-card")

        # get the exact time the review was posted, sufficiently unique to count as id
        review.id <- reviews %>% html_attr("data-review-date")
        # get the date of the stay and capitalize the first letter of every word
        review.date <- reviews %>%
        html_node(".date") %>%
        html_text() %>%
        str_to_title() %>%
        parse_date_time2("b! d!,Y!")

        # Fill in and clean the rest of the data
        review.score <- reviews %>% html_attr("data-review-rating")
        review.summary <- reviews %>% html_node(".review-summary") %>% html_text()

        review.text <- reviews %>%
                        html_node(".review-content .expandable-content") %>%
                        html_text() %>%
                        CleanBody()
        # Add rows to scraped.reviews and push this to the outer scope
        SaveDataFrameToDB(database,
                      "Scraped",
                      tibble(review.id,
                             review.date,
                             review.score,
                             review.summary,
                             review.text),
                      doAppend = TRUE)
    }
}

ScrapeTripExpert <- function() {

    url <- "https://www.tripexpert.com/las-vegas/hotels/ballys-las-vegas-hotel-and-casino"
    # get every review card, usually 50 per page 
    reviews <- read_html(url) %>%
        html_nodes(".expert_review")

    # Fill in and clean the rest of the data
    review.id <- NA
    review.date <- NA
    review.score <- NA
    review.summary <- NA
    review.text <- reviews %>%
                        html_node(".review_content p span") %>%
                        html_text() %>%
                        CleanBody()
    # Add rows to scraped.reviews and push this to the outer scope
    SaveDataFrameToDB(database,
                      "Scraped",
                      tibble(review.id,
                             review.date,
                             review.score,
                             review.summary,
                             review.text),
                      doAppend = TRUE)

}


