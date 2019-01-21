require(leaflet)
require(mongolite)
require(dplyr)

hotel.reviews.collection <- mongo(collection = "hotel_reviews", db = "hotel_reviews", url = "mongodb://localhost")

function(input, output, session) {
    data <- reactive({
        x <- hotel.reviews.collection$find(query = paste(
                                            '{',
                                            paste('"Hotel_Country" : ', '"NL",'),
                                            paste('"Average_Score" : ', '{ "$gt" : ', input$minimum_score, '}',
                                            "}"
                                            )
                                      )) %>%
                                      group_by(Hotel_Name) %>%
                                      summarise(lat = mean(Hotel_lat),
                                  lng = mean(Hotel_lng),
                                  avg_score = sprintf("%.1f", mean(Reviewer_Score)),
                                  ) %>%
                                  mutate(group = cut(as.numeric(avg_score), breaks = c(3, 5, 6, 7, 9, 10),
                                         labels = c('darkred', 'red', 'orange', 'green', "darkgreen")))
    })

    observe({
        click = input$map_marker_click
        if (is.null(click)) {
            return()
        }


    })

    output$map <- renderLeaflet({
        df <- data()

        icons <- awesomeIcons(icon = "whatever",
                      iconColor = "black",
                      library = "ion",
                      markerColor = df$group)

        leaflet(data = df) %>%
        addTiles() %>%
        addAwesomeMarkers(
                          lng = df$lng,
                          lat = df$lat,
                          popup = {
            paste(sep = "<br/>",
                  paste("<strong>", df$Hotel_Name, "</strong>"),
                  paste("<strong>", "Avg rating: ", "</strong>", df$avg_score)
                  )
        },
        clusterOptions = markerClusterOptions(),
        icon = icons
        )

    })
}
