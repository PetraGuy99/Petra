setwd('C:/dev/code/Petra')


#################

library(leaflet)

library(dplyr)
#LkDXYcbS1dYXk2hbGZTbsRGACx0kDWdd

# Example data frame with coordinates (lat/lon)
points_df <- data.frame(
  name = c("C1", "T1", "C2","T2", "C3", "T3"),
  lat = c(56.405141,56.404089,56.405029,56.404829,56.405019, 56.404960),  # latitude
  lon = c(-3.967394,-3.965881,-3.964707,-3.965019,-3.966727,-3.965886) ,  # longitude
  category = c("C", "C", "C", "T", "T", "T")  # C = control, T = treatment
)


os_key <- "LkDXYcbS1dYXk2hbGZTbsRGACx0kDWdd"

colors <- c(C = "orange", T = "green")
points_df$color <- ifelse(points_df$category == "C", "orange", "green")

# Create leaflet map
leaflet(points_df) %>%
  setView(lng = -3.967, lat = 56.405, zoom = 8) %>%
    addTiles(
      urlTemplate = paste0(
        "https://api.os.uk/maps/raster/v1/zxy/Outdoor_3857/{z}/{x}/{y}.png?key=", os_key
      ),
      attribution = "© Ordnance Survey",
      group = "Outdoor"
    ) %>%

  addCircleMarkers(
    ~lon, ~lat,
    label = ~name,
    color = "black",          # border color
    fillColor = ~color,       # fill color (orange/green)
    fillOpacity = 0.8,
    radius = 6,
    group = "Points"
  ) 
