require(leaflet)
require(mongolite)
library(ggplot2)
library(lubridate)

hotel.reviews.collection <- mongo(collection = "hotel_reviews", db = "hotel_reviews", url = "mongodb://localhost")


fluidPage(
    sidebarLayout(
        mainPanel(
            leafletOutput("map"),
             splitLayout(
                plotOutput("ReviewsOverTime"),
                plotOutput("ReviewerNationality")
            )
        ),
        sidebarPanel(
              sliderInput("minimum_score",
                          "Minimum score:",
                          min = 0,
                          max = 10,
                          step = 0.1,
                          value = 8),
              sliderInput("minimum_reviews",
                          "Minimum Reviews:",
                          min = 0,
                          max = 10,
                          step = 1,
                          value = 0),
              dateRangeInput('dateRange',
                            label = 'Show reviews from: To:',
                            start = ymd("20170101"), end = Sys.Date()),
              actionButton("recalculate", "Recalculate graph")
                  )
        )
    )
