require(leaflet)
require(mongolite)
require(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)

hotel.reviews.collection <- mongo(collection = "hotel_reviews", db = "hotel_reviews", url = "mongodb://localhost")

function(input, output, session) {
    hotels <- reactive({
        x <- hotel.reviews.collection$find(query = paste(
                                            '{',
                                            paste('"Hotel_Country" : ', '"NL",'),
                                            paste('"Average_Score" : ', '{ "$gt" : ', input$minimum_score, '}',
                                            "}"
                                            )
                                      ))
    })

    hotel_stats <- reactive({
        df <- hotels()

        x <- df %>%
            group_by(Hotel_Name) %>%
            summarise(lat = mean(Hotel_lat),
                lng = mean(Hotel_lng),
                avg_score = sprintf("%.1f", mean(Reviewer_Score)),
                num_reviews = n(),
                address = first(Hotel_Address)
                ) %>%
                mutate(group = cut(as.numeric(avg_score), breaks = c(3, 5, 6, 7, 9, 10),
                labels = c('darkred', 'red', 'orange', 'green', "darkgreen")))
        x <- x[which(x$num_reviews >= input$minimum_reviews),]
    })


    observeEvent(input$recalculate, {
        df <- hotels()

        print("observed map_marker_click")
        clickedMarker = input$map_marker_click
        print(clickedMarker)

        df$Review_Date <- ymd(df$Review_Date)


        df %<>% filter(Hotel_lat == clickedMarker$lat &
                       Hotel_lng == clickedMarker$lng &
                       Review_Date >= ymd(input$dateRange[1]) &
                       Review_Date <= ymd(input$dateRange[2]))

        topNationalities <- as.data.frame(table(df$Reviewer_Nationality))
        topNationalities <- topNationalities[order(-rank(topNationalities$Freq)),][1:5,]
        print(topNationalities)

        output$ReviewsOverTime <- renderPlot({
            ggplot(df, aes(x = Review_Date, y = Reviewer_Score)) +
            ylim(0, 10) +
            geom_point() +
            geom_smooth(method = "auto")
        })


        output$ReviewerNationality <- renderPlot({
            bp <- ggplot(topNationalities, aes(x = " ", y = topNationalities$Freq, fill = topNationalities$Var1)) +
            geom_bar(width = 1, stat = "identity")
            pie <- bp + coord_polar("y", start = 0)
            pie
        })
    })


    observeEvent(input$map_marker_click, {
        df <- hotels()

        print("observed map_marker_click")
        clickedMarker = input$map_marker_click
        print(clickedMarker)

        df$Review_Date <- ymd(df$Review_Date)


        df %<>% filter(Hotel_lat == clickedMarker$lat &
                       Hotel_lng == clickedMarker$lng &
                       Review_Date >= ymd(input$dateRange[1]) &
                       Review_Date <= ymd(input$dateRange[2]))

        topNationalities <- as.data.frame(table(df$Reviewer_Nationality))
        topNationalities <- topNationalities[order(-rank(topNationalities$Freq)),][1:5,]
        print(topNationalities)

        output$ReviewsOverTime <- renderPlot({
            ggplot(df, aes(x = Review_Date, y = Reviewer_Score)) +
            ylim(0, 10) +
            geom_point() +
            geom_smooth(method = "auto")
        })


        output$ReviewerNationality <- renderPlot({
            bp <- ggplot(topNationalities, aes(x = " ", y = topNationalities$Freq, fill = topNationalities$Var1)) +
            geom_bar(width = 1, stat = "identity")
            pie <- bp + coord_polar("y", start = 0)
            pie
        })
    })

    output$map <- renderLeaflet({
        df <- hotel_stats()
        updateSliderInput(session = session,
                          inputId = "minimum_reviews",
                          min = 0,
                          max = max(df$num_reviews))

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
                  paste("<italic>", df$address, "</italic>"),
                  paste("<strong>", "Avg rating: ", "</strong>", df$avg_score),
                  paste("<strong>", "Number of reviews: ", "</strong>", df$num_reviews)
                  )
        },
        clusterOptions = markerClusterOptions(),
        icon = icons
        )

    })
}
