require(leaflet)

fluidPage(
    sidebarLayout(
        mainPanel(
            leafletOutput("map")
        ),
        sidebarPanel(
              sliderInput("minimum_score",
                          "Minimum score:",
                          min = 0,
                          max = 10,
                          step = 0.1,
                          value = 8)
                  )
        )
    )
