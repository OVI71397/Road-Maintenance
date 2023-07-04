library(shiny)
library(dplyr)
library(leaflet)

kenya_network <- sf::st_read("/vsicurl/https://github.com/OVI71397/Road-Maintenance/raw/main/data/kenya_roads/Kenya_roads_version2.shp")
kenya_bound <- sf::st_read("/vsicurl/https://github.com/OVI71397/Road-Maintenance/raw/main/data/Kenya_County_Boundaries/Kenya_County_Boundaries.shp")
map <- leaflet(kenya_network) %>% 
  addTiles() %>% 
  setView(36.906, 0.4, zoom = 6) %>%
  addPolygons(data = kenya_bound, color = "#F8F9FB",
              opacity = 0.3,
              fillColor = "#F8F9FB",
              fillOpacity = 0.8) %>%
  addPolylines(data = kenya_network, 
               weight = 3,
               opacity = 1)
  
ui <- fluidPage(
  
  fluidRow(
    shinycssloaders::withSpinner(
      leaflet::leafletOutput('kenya_roads',
                             width = "100%",
                             height = 700), 
      size = 1, color = "#719b25")
  )
)

server <- function(input, output, session){
  output$kenya_roads <- renderLeaflet({
    map
    })
}

shinyApp(ui = ui, server = server)


