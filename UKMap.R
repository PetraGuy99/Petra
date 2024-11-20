
#map of site locations
library(ggplot2)
library(sf)
library(ggspatial)
library(osmdata)
library(ggmap)
library(leaflet)
library(rnaturalearth)

locations = read.csv('../../data/sitelocs.csv')

uk_shapefile <- ne_countries(country = "United Kingdom", returnclass = "sf")

# Check the CRS
st_crs(uk_shapefile)

# Reproject to WGS84 if it's not already (EPSG:4326)
if (st_crs(uk_shapefile) != 4326) {
  uk_shapefile <- st_transform(uk_shapefile, crs = 4326)
}

leaflet() %>%
  # Add the UK boundary outline
  addPolygons(data = uk_shapefile, color = "black", weight = 2, fillOpacity = 0) %>%
  # Set the view to the UK
  setView(lng = -3.1883, lat = 55.3781, zoom = 6) %>%
  # Add a title using Label Only Marker
  addLabelOnlyMarkers(lng = -3.1883, lat = 55.3781, label = "<strong>UK Outline Map</strong>", 
                      labelOptions = labelOptions(noHide = TRUE, direction = "top", offset = c(0, 10)))