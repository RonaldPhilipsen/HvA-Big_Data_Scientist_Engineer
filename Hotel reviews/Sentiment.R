#pulls up the grapth to show how much impact the top 25 contibuters 
#have for a given sentiments set.
getContribution <- function(reviews, sentiments) {
    reviews %>%
    unnest_tokens(word, "review_body") %>%
    inner_join(sentiments, by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(), contribution = sum(score)) %>%
    top_n(50, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    ggtitle('Words with the greatest contributions to positive/negative 
          sentiment in reviews') +
          geom_bar(stat = "identity", show.legend = FALSE) +
          coord_flip()
}


