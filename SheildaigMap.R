setwd('C:/dev/code/Petra')


#################

library(leaflet)

library(dplyr)




library(leaflet)

leaflet() %>%
  addTiles(
    urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution = 'Map data © <a href="https://openstreetmap.org/copyright">OpenStreetMap</a> contributors'
  ) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  setView(lng = -5.6155, lat = 57.4899, zoom = 16) %>%
  
  # 🟩 Amanita polygon
  addPolygons(
    lng = c(-5.61554, -5.61574, -5.61559, -5.61525, -5.61554),
    lat = c(57.48992, 57.49002, 57.49005, 57.48973, 57.48992),
    color = "green",
    fillColor = "lightblue",
    fillOpacity = 0.4,
    weight = 2,
    popup = "Amanita"
  ) %>%
  
  # 🟧 Control polygon
  addPolygons(
    lng = c(-5.61422, -5.61431, -5.61496, -5.61422),
    lat = c(57.48862, 57.48894, 57.48891, 57.48862),
    color = "orange",
    fillColor = "orange",
    fillOpacity = 0.4,
    weight = 2,
    popup = "Control"
  ) %>%
  
  # 🟩 Suillus polygon (fixed)
  addPolygons(
    lng = c(-5.61496, -5.61492, -5.61470, -5.61496),
    lat = c(57.48957, 57.48924, 57.48956, 57.48957),
    color = "darkgreen",      # ✅ valid color
    fillColor = "lightgreen",
    fillOpacity = 0.4,
    weight = 2,
    popup = "Suillus"
  )

leaflet() %>%
  addTiles(
    urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution = 'Map data © <a href="https://openstreetmap.org/copyright">OpenStreetMap</a> contributors'
  ) %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  setView(lng = -5.6155, lat = 57.4899, zoom = 16) %>%
  
  # 🟩 Amanita polygon
  addPolygons(
    lng = c(-5.61554, -5.61574, -5.61559, -5.61525, -5.61554),
    lat = c(57.48992, 57.49002, 57.49005, 57.48973, 57.48992),
    color = "green",
    fillColor = "lightblue",
    fillOpacity = 0.4,
    weight = 2,
    popup = "Amanita"
  ) %>%
  
  # 🟧 Control polygon
  addPolygons(
    lng = c(-5.61422, -5.61431, -5.61496, -5.61422),
    lat = c(57.48862, 57.48894, 57.48891, 57.48862),
    color = "orange",
    fillColor = "orange",
    fillOpacity = 0.4,
    weight = 2,
    popup = "Control"
  ) %>%
  
  # 🟩 Suillus polygon (fixed)
  addPolygons(
    lng = c(-5.61496, -5.61492, -5.61470, -5.61496),
    lat = c(57.48957, 57.48924, 57.48956, 57.48957),
    color = "darkgreen",      # ✅ valid color
    fillColor = "lightgreen",
    fillOpacity = 0.4,
    weight = 2,
    popup = "Suillus"
  )


#different base layer

leaflet() %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  setView(lng = -5.6155, lat = 57.4899, zoom = 18) %>%
  
  # Amanita polygon
  addPolygons(
    lng = c(-5.61554, -5.61574, -5.61559, -5.61525, -5.61554),
    lat = c(57.48992, 57.49002, 57.49005, 57.48973, 57.48992),
    color = "green",
    fillColor = "green",
    fillOpacity = 0.7,
    weight = 3,
    popup = "Amanita"
  ) %>%
  
  # Control polygon
  addPolygons(
    lng = c(-5.61422, -5.61431, -5.61496, -5.61422),
    lat = c(57.48862, 57.48894, 57.48891, 57.48862),
    color = "orange",
    fillColor = "orange",
    fillOpacity = 0.7,
    weight = 3,
    popup = "Control"
  ) %>%
  
  # Suillus polygon
  addPolygons(
    lng = c(-5.61496, -5.61492, -5.61470, -5.61496),
    lat = c(57.48957, 57.48924, 57.48956, 57.48957),
    color = "darkgreen",
    fillColor = "darkgreen",
    fillOpacity = 0.7,
    weight = 3,
    popup = "Suillus"
  )
