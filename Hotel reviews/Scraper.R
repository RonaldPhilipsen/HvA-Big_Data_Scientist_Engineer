scraped.reviews <- tibble()

#we want 100 reviews
for (i in c(1:10)) {
    # get a hotels.com page (we prepend the UK to get english-language results)
    url <- paste("https://uk.hotels.com/ho107128-tr-p", i, sep = "")


    reviews <- url %>%
    read_html() %>%
    html_nodes(".review-card")

    # get the exact time the review was posted, sufficiently unique to count as id
    review.id <- reviews %>% html_attr("data-review-date")

    # get the date of the stay and capitalize the first letter of every word
    # using title-case
    review.date <- reviews %>%
        html_node(".date") %>%
        html_text() %>%
        str_to_title() %>%
        parse_date_time2("d! b!,Y!")
           
    review.score <- reviews %>% html_attr("data-review-rating")

    review.summary <- reviews %>% html_node(".review-summary") %>% html_text()

    review.text <- reviews %>% html_node(".review-content .expandable-content") %>%
                               html_text() %>% CleanBody()

    # Add rows to scraped.reviews and push this to the outer scope
    scraped.reviews <- rbind(scraped.reviews, tibble(review.id, review.date, review.score, review.summary, review.text))
}

SaveDataFrameToDB("HotelReviews.sqlite", "Scraped", scraped.reviews, doAppend = FALSE)
