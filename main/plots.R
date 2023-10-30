
library(leaflet)
library(dplyr)
# library(geojsonio)
# library(viridis)
library(rnaturalearth)
# library(sf)
# library(sp)
# library(mapview)
# devtools::install_github("ropensci/rnaturalearthhires")
# library(rnaturalearthires)

setwd("C:/Users/39339/OneDrive/Desktop/I semestre/Bayesian_Project/main")

stazioni = read.csv("C:/Users/39339/OneDrive/Desktop/I semestre/dati_ozono/stazioni_O3.csv")
# ozono = read.csv("C:/Users/39339/OneDrive/Desktop/I semestre/dati_ozono/datasetO3.csv")

italy_map <- ne_states(country = "Italy", returnclass = "sf") 
map_lombardia <- italy_map[which(italy_map$region == "Lombardia"),] %>% 
  group_by(region)  %>%
  summarise(n = n())


mappa <- leaflet( data = stazioni) %>%
  addTiles() %>%
  # addAwesomeMarkers(lng=stazioni$lng, lat=stazioni$lat) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolygons(data = map_lombardia, stroke = T, opacity = .4, color = "blue",
              fillOpacity = .2, fill = T, fillColor =  "blue")   %>%
  addCircleMarkers(
    ~ lng, ~ lat,
    fillColor = ifelse(stazioni$IdSensore %in% unique(ozono$idSensore), "green","red"),
    fillOpacity = 1,
    stroke = F,
    radius = 4
  ) 

mappa




