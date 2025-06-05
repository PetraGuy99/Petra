setwd('C:/dev/code/Petra')


library(rnaturalearth)
library(leaflet)
library(leaflet.extras)
library(sf)
library(rnaturalearthhires)
# Load the UK boundary (1:10m resolution)
uk_shapefile <- ne_countries(country = "United Kingdom", scale = 'large', returnclass = "sf")
locations = read.csv('../../data/Mapping.csv')

uk_shapefile_sp <- as(uk_shapefile, "Spatial")
# Reproject to WGS84 if needed (EPSG:4326)
if (st_crs(uk_shapefile) != 4326) {
  uk_shapefile <- st_transform(uk_shapefile, crs = 4326)
}

# Normalize dot sizes for better visualization
max_size <- 10  # Adjust as needed
min_size <- 3   # Adjust as needed
locations$scaled_size <- scales::rescale(locations$num.trees, to = c(min_size, max_size))

locations$years <- as.factor(locations$years)  

library(RColorBrewer)

# Choose a more visible color palette
pal <- colorFactor(palette = brewer.pal(8, "Dark2"), domain = levels(locations$years))

# Create the map ##with time as a colour
leaflet() %>%
  addPolygons(data = uk_shapefile_sp, 
              color = "black", 
              weight = 2, 
              fillColor = "palegreen",  
              fillOpacity = 0.5) %>%
  addCircleMarkers(data = locations, 
                   lng = ~long, lat = ~lat.1, 
                   radius = ~scaled_size,  
                   color = ~pal(years),  
                   fill = TRUE, fillColor = ~pal(years), fillOpacity = 1) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = locations$years,  # Ensure this matches the factor domain
            title = "years", 
            opacity = 1) %>%
  setView(lng = -3.1883, lat = 55.3781, zoom = 6) 



leaflet() %>% #without years points coloured
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
  setView(lng = -3.1883, lat = 55.3781, zoom = 6)  # Center on the UK .

##add eire


library(scales)
uk_ire_shapefile <- ne_countries(country = c("United Kingdom", "Ireland"), 
                                 scale = 'large', returnclass = "sf")

locations$scaled_size <- rescale(locations$num.trees, to = c(3, 10))
locations$years <- as.factor(locations$years)

# Define color palette
pal <- colorFactor(palette = brewer.pal(8, "Dark2"), domain = levels(locations$years))

# Create leaflet map
leaflet() %>%
  addPolygons(data = uk_ire_shapefile, 
              color = "black", weight = 2, 
              fillColor = "palegreen", fillOpacity = 0.5) %>%
  addCircleMarkers(data = locations, 
                   lng = ~long, lat = ~lat.1, 
                   radius = ~scaled_size,  
                   color = ~pal(years), fill = TRUE, 
                   fillColor = ~pal(years), fillOpacity = 1) %>%
  addLegend("bottomright", 
            pal = pal, values = locations$years, 
            title = "years", opacity = 1) %>%
  setView(lng = -3.1883, lat = 55.3781, zoom = 6)

#without years in the key


leaflet() %>%
  addPolygons(data = uk_ire_shapefile, 
              color = "black", weight = 2, 
              fillColor = "palegreen", fillOpacity = 0.5) %>%
  addCircleMarkers(data = locations, 
                   lng = ~long, lat = ~lat.1, 
                   radius = ~scaled_size,  
                   color = "blue", fill = TRUE, 
                   fillColor = "blue", fillOpacity = 1) %>%
  setView(lng = -3.1883, lat = 55.3781, zoom = 6)
