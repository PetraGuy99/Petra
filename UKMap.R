setwd('C:/dev/code/Petra')


library(rnaturalearth)
library(leaflet)
library(leaflet.extras)
library(sf)
library(rnaturalearthhires)
# Load the UK boundary (1:10m resolution)
uk_shapefile <- ne_countries(country = "United Kingdom", scale = 'large', returnclass = "sf")
locations = read.csv('../../data/sitelocs.csv')
# Reproject to WGS84 if needed (EPSG:4326)
if (st_crs(uk_shapefile) != 4326) {
  uk_shapefile <- st_transform(uk_shapefile, crs = 4326)
}

# Normalize dot sizes for better visualization
max_size <- 10  # Adjust as needed
min_size <- 3   # Adjust as needed
locations$scaled_size <- scales::rescale(locations$Tree.Numbers, to = c(min_size, max_size))
# Create the map using leaflet
leaflet() %>%
  addPolygons(data = uk_shapefile, 
              color = "black", 
              weight = 2, 
              fillColor = "palegreen",  
              fillOpacity = 0.5) %>%
  addCircleMarkers(data = locations, 
                   lng = ~Longitude, lat = ~Latitude, 
                   radius = ~scaled_size,  # Scale marker size
                   color = "red", 
                   fill = TRUE, fillColor = "red", fillOpacity = 0.8) %>%
  setView(lng = -3.1883, lat = 55.3781, zoom = 6)  # Center on the UK
