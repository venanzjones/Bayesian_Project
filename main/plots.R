library(mapview)
library(leaflet)
library(geojsonio)
library(viridis)
library(rnaturalearth)
library(dplyr)
library(sf)
library(sp)
# devtools::install_github("ropensci/rnaturalearthhires")
# library(rnaturalearthires)

setwd("C:/Users/39339/OneDrive/Desktop/I semestre/Bayesian_Project/main")

stazioni = read.csv("C:/Users/39339/OneDrive/Desktop/I semestre/dati_ozono/stazioni_O3.csv")

italy_map <- ne_states(country = "Italy", returnclass = "sf") 
map_lombardia <- italy_map[which(italy_map$region == "Lombardia"),] %>% 
  group_by(region)  %>%
  summarise(n = n())


mappa <- leaflet( data = stazioni) %>%
  addTiles() %>%
  # addAwesomeMarkers(lng=stazioni$lng, lat=stazioni$lat) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolygons(data = map_lombardia)   %>%
  addCircleMarkers(
    ~ lng, ~ lat,
    fillColor = "black",
    fillOpacity = 1,
    stroke = F,
    radius = 3
  ) 
mappa



