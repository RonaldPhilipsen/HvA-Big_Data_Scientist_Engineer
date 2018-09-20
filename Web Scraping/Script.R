library(rvest)
library(magrittr)
library(stringr)
library(doParallel)
library(foreach)
library(dplyr)
library(ggplot2)

parseMovies <- function(movies) {
    options(stringsAsFactors = FALSE)

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
        years <- append(years, as.integer(gsub("\\D", "", year)))

        score <- (html_node(movie, ".ratings-imdb-rating strong") %>% html_text)
        if (!is.na(score)) { score <- as.integer(as.double(score) * 10) }
        scores <- append(scores, as.integer(score))

        runtimes <- append(runtimes, gsub("\\D", "", (html_node(movie, ".runtime") %>% html_text)))

        metaScore <- html_node(movie, ".metascore") %>% html_text
        metaScores <- append(metaScores, as.integer(metaScore))

        grossProfit <- html_node(movie, "[name=nv]") %>% html_text
        grossProfits <- append(grossProfits, as.double(grossProfit))
        
        genres <- append(genres, (html_node(movie, ".genre") %>% html_text))
    }
    return(data.frame(title = titles, release_year = years, imdb_score = scores, meta_score = metaScores, gross_profits = grossProfits))
}

# Parses a single page from IMDB, returns the data frame from the parseMovies Function
parseImbdPage <- function(url) {
    options(stringsAsFactors = FALSE)

    tryCatch({
        page_html <- read_html(url)
        error <- html_nodes(page_html, ".error_code") %>% html_text

        if (length(error) > 0) {
            return(NA);
        }

        items <- html_nodes(page_html, ".lister-item")
        len <- length(items)
        return(suppressWarnings(parseMovies(items)))
    },
    warning = function(w) {
        return(NA)
    },
    error = function(e) {
        return(NA)
    },
    finally = { }
    )
}

combineListToDF <- function(x, ...) {
    rbind(x, ...)
}


#setup parallel backend to use many processors
cores = detectCores()

#not to overload your computer
cl <- makeCluster(cores[1] - 1)
registerDoParallel(cl)

movies <- data.frame(title = character(), release_year = integer(), imdb_score = integer(), meta_score = integer(), gross_profits = double())
i <- 1

movies <- foreach(i = 1:201, .combine = combineListToDF, .packages = c("rvest", "magrittr")) %do% {

    imdbURL <- url(paste("https://www.imdb.com/search/title?year=2018,2018&title_type=feature&sort=moviemeter,asc&page=", i, "&ref_=adv_nxt", sep = ""))

    parseImbdPage(imdbURL)
}

summary(movies)
qplot(movies$meta_score, movies$imdb_score, geom = c("point", "smooth"))