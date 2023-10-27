# loading all the required packages

library(lubridate)
library(rstan)
library(rnaturalearth)
library(rnaturalearthires)
library(dplyr)
library(mapview)
library(leaflet)
library(geojsonio)
library(viridis)
devtools::install_github("ropensci/rnaturalearthhires")

# read data

ozono = read.csv("C:/Users/39339/OneDrive/Desktop/I semestre/dati_ozono/datasetO3.csv")

stazioni = read.csv("C:/Users/39339/OneDrive/Desktop/I semestre/dati_ozono/stazioni_O3.csv")

# converting dates to dmy hms format (-> to deal with am/pm) 

# ozono$Data = lubridate::dmy_hms(ozono$Data)
# per qualche ragione strana crea NAs, da vedere il perchè
# intanto meglio dare un occhio alle stazioni "chiuse temporaneamente"


# rm idOperatore since they re = 1

ozono$idOperatore = NULL

# check NAs

na_count <-sapply(ozono, function(y) sum(length(which(is.na(y)))))
na_count

na_count[3] / length(ozono[,3])

# 2.376734% di NAs 
# da vedere meglio comunque

# leaflet for cool visualization ?


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=stazioni$lng[1], lat=stazioni$lat[1]) %>%
  addProviderTiles(providers$Esri.WorldImagery)
m

# from https://github.com/openpolis/geojson-italy

regioni.italiane <- geojsonio::geojson_read("C:/Users/39339/OneDrive/Desktop/I semestre/Bayesian_Project/limits_IT_regions.geojson", what = "sp") 

italy_map <- ne_states(country = "Italy", returnclass = "sf") 

italy_map_region <- italy_map %>%
  group_by(region) %>%
  summarise(n = n()) 

map_lombardia <- italy_map[which(italy_map$region == "Lombardia"),] %>% 
                 group_by(region)  %>%
                 summarise(n = n())

# PLOT DEFINTIVO SHAPE LOMBARDIA

# icon = iconList(makeIcon("icona1.png", 80, 80)) 
# FIX THIS IF YOU WANT CUSTOM ICOn

# icon <- awesomeIcons(
#   icon = 'ion-close',
#   iconColor = 'black',
#   library = 'ion',
#   markerColor = "red"
# )

# use to create palettes, not needed now bt worth noticing

pal <- viridis::magma(1, direction = 1)


sfondo = leaflet(data = stazioni) %>% addTiles() %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  # addMarkers(~lng, ~lat, ) 
  # addCircles(~lng, ~lat , weight = 1,radius = 1000)
  addCircleMarkers(
    fillColor = "black",
    fillOpacity = 1,
    stroke = F,
    radius = 3
  ) 

map_lombardia %>% mapview (map = sfondo, col.regions = "chocolate2")













































































