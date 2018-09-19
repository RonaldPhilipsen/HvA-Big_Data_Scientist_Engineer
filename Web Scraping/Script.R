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


    for (movie in movies) {
        titles <- append(titles, (html_node(movie, ".lister-item-header a") %>% html_text))

        year = html_node(movie, ".lister-item-year") %>% html_text
        years <- append(years, gsub("\\D", "", year))

        scores <- append(scores, (html_node(movie, ".ratings-imdb-rating strong") %>% html_text))

        runtimes <- append(runtimes, gsub("\\D", "", (html_node(movie, ".runtime") %>% html_text)))

        metaScores <- append(metaScores,(html_node(movie, ".metascore") %>% html_text))

        grossProfits <- append(grossProfits, (html_node(movie, "[name=nv]") %>% html_text))
    }

    return(data.frame(title = titles, release_year = years, imdb_score= scores, meta_score = metaScores, gross_profits = grossProfits))
}

imdbURL <- url("https://www.imdb.com/search/title?year=2018,2018&title_type=feature&sort=moviemeter,asc")
page_html <- read_html(imdbURL)
items <- html_nodes(page_html, ".lister-item")
len <- length(items);

movies <- parseMovies(items)

movies

