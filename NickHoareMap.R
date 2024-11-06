setwd('C:/dev/code/Petra')


#################

library(leaflet)

library(dplyr)

#Nick Hoare - Inoculated treatment


leaflet() %>%
  addTiles(
    urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution = 'Map data © <a href="https://openstreetmap.org/copyright">OpenStreetMap</a> contributors'
  ) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  setView(lng = -2.3202680, 51.0936780, zoom = 10) %>%
  addPolygons(
    lng = c(-2.3202680, -2.321158, -2.321158, -2.319996, -2.3202680),
    lat = c(51.0936780, 51.09239, 51.093854, 51.094244,51.0936780 ),
    color = "blue" 
  )
