library(leaflet)
library(dplyr)
library(lubridate)
# library(geojsonio)
# library(viridis)
library(rnaturalearth)
# library(sf)
# library(sp)
# library(mapview)
# devtools::install_github("ropensci/rnaturalearthhires")
# library(rnaturalearthires)

ozono <- read.csv("./Dati_iniziali/datasetO3.csv")
stazioni <- read.csv("./Dati_iniziali/stazioni_O3.csv")
stazioni.usate <- stazioni[which(stazioni$IdSensore %in% unique(ozono$idSensore)), ]
rm(ozono)



italy_map <- ne_states(country = "Italy", returnclass = "sf") 
map_lombardia <- italy_map[which(italy_map$region == "Lombardia"),] %>% 
  group_by(region)  %>%
  summarise(n = n())

not_to_keep = c(1)

mappa <- leaflet( data = stazioni.usate) %>%
  addTiles() %>%
  # addAwesomeMarkers(lng=stazioni$lng, lat=stazioni$lat) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolygons(data = map_lombardia, stroke = T, opacity = .4, color = "blue",
              fillOpacity = .2, fill = T, fillColor =  "blue")   %>%
  addCircleMarkers(
    ~ lng, ~ lat,
    fillColor = ifelse(stazioni.usate$IdSensore < 17288, "green","red"),
    # fillColor = "green",
    fillOpacity = 1,
    stroke = F,
    radius = 4
  ) 

mappa

library(mapview)
mapshot(mappa, file = "51stations.png", remove_controls = c("zoomControl","layersControl", "homeButton", "scaleBar", "drawToolbar", "easyButton"))


# for when rgdal will be back

# library(rgdal)
# library(sp)
# 
# italy = readOGR("Reg01012021_g/Reg01012021_g_WGS84.shp", GDAL1_integer64_policy = TRUE)
# italy <- spTransform(italy, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# 
# lombardia = italy[italy$DEN_REG=='Lombardia', ]

ozono$idOperatore <- NULL

ozono$Data <- mdy_hms(ozono$Data)
ozono$Year <- year(ozono$Data - 1)
ozono$Month <- month(ozono$Data - 1)
ozono$Day <- day(ozono$Data - 1)
ozono$Hour <- hour(ozono$Data - 1)

### STEP 1: fill the dataset with NAs where there are non registered values
# 4748*51*24 = 5811552 osservazioni in tutto dal 1-1-2010 al 31-12-2022
ozono$timestamp <- make_datetime(ozono$Year, ozono$Month, ozono$Day, ozono$Hour)

# Generate a complete set of timestamps for all sensors
all_timestamps <- expand.grid(
  idSensore = unique(ozono$idSensore),
  timestamp = seq(min(ozono$timestamp), max(ozono$timestamp), by = "1 hour")
)

# Merge the complete set of timestamps with your data
ozono_completed <- left_join(all_timestamps, ozono, by = c("idSensore", "timestamp"))

ozono_completed <- ozono_completed %>%
  mutate(
    Year = year(timestamp),
    Month = month(timestamp),
    Day = day(timestamp),
    Hour = hour(timestamp)
  )

ozono_completed$Data <- NULL
rm(ozono)
rm(all_timestamps)

ozono_completed$Stato <- NULL

ozono_filtered <- ozono_completed %>%
  filter(Month >= 4 & Month <= 10)
rm(ozono_completed)

data <- ozono_filtered %>%
  filter(Month == 6 & Year == 2019)


wrapped <- data[c(1,3,4,5,6,7)] %>% 
  pivot_wider(names_from = idSensore, values_from = Valore, values_fill = 0)

days <- 1:720
x11()
matplot(wrapped[days,5:55], type = 'l', xaxt = "n", xlab = "June 2019", ylab = "Ozone concentration")
abline(h = c(120,180),  lty = 5)
for (i in 5:55) {
  bigpoints <- which(wrapped[days,i]>120)
  points( bigpoints, t(wrapped[bigpoints,i]) ,  pch = 15, col = i-5)
}
for (i in 5:55) {
  bigpoints <- which(wrapped[days,i]>180)
  points( bigpoints, t(wrapped[bigpoints,i]) ,  pch = 15, col = i-5)
}

