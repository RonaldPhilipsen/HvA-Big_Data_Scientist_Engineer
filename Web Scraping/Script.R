library(rvest)
library(magrittr)
library(stringr)


parseMovies <- function(movies) {
    titles <- c()
    years <- c()
    scores <- c()
    runtimes <- c()
    metaScores <- c()
    grossProfits <- c()
    genres <- c()

    for (movie in movies) {
        titles <- append(titles, (html_node(movie, ".lister-item-header a") %>% html_text))

        year = html_node(movie, ".lister-item-year") %>% html_text
        years <- append(years, gsub("\\D", "", year))

        scores <- append(scores, (html_node(movie, ".ratings-imdb-rating strong") %>% html_text))

        runtimes <- append(runtimes, gsub("\\D", "", (html_node(movie, ".runtime") %>% html_text)))

        metaScores <- append(metaScores, (html_node(movie, ".metascore") %>% html_text))

        grossProfits <- append(grossProfits, (html_node(movie, "[name=nv]") %>% html_text))

        genres <- append(genres, (html_node(movie, ".genre") %>% html_text))
    }

    return(data.frame(title = titles, release_year = years, imdb_score = scores, meta_score = metaScores, gross_profits = grossProfits))
}


movies <- data.frame(title = character(), release_year = character(), imdb_score = character(), meta_score = character(), gross_profits = character())



is_error <- FALSE
i <- 1


while (!is_error) {
    print(paste("Reading page: ", i, sep = ""))
    imdbURL <- url(paste("https://www.imdb.com/search/title?year=2018,2018&title_type=feature&sort=moviemeter,asc&page=", i, "&ref_=adv_nxt", sep = ""))

    result = tryCatch({
        page_html <- read_html(imdbURL)
        error <- html_nodes(page_html, ".error_code") %>% html_text

        if (length(error) > 0) {
            is_error <<- TRUE;
            break;
        }

        items <- html_nodes(page_html, ".lister-item")
        len <- length(items);
        movies <- rbind(movies, parseMovies(items))
    }, warning = function(w) {
        is_error <<- TRUE
    }, error = function(e) {
        is_error <<- TRUE
    }, finally = {
        i <- i + 1
    })
}


movies

