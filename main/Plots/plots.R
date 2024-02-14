library(leaflet)
library(dplyr)
library(tidyr)
library(lubridate)
library(rnaturalearth)
library(htmltools)

ozono <- read.csv("./Dati_iniziali/datasetO3.csv")
stazioni <- read.csv("./Dati_iniziali/stazioni_O3.csv")
# ozono <- read.csv("datasetO3.csv")
# stazioni <- read.csv("stazioni_O3.csv")
stazioni.usate <- stazioni[which(stazioni$IdSensore %in% unique(ozono$idSensore)), ]
# rm(ozono)



italy_map <- ne_states(country = "Italy", returnclass = "sf") 
map_lombardia <- italy_map[which(italy_map$region == "Lombardia"),] %>% 
  group_by(region)  %>%
  summarise(n = n())
stazioni.usate <- stazioni.usate[which(stazioni.usate$IdSensore < 17288),]

#  replace it with the 45 etas from model
strength120 <- c(1.961210, 1.591435, 1.586590, 1.557275, 1.943370, 1.636370, 1.304795 ,2.124930, 1.495165,
                 1.446165, 1.788870 ,1.095650,1.298005 ,2.016960 ,1.960915, 1.473965 ,1.744950, 1.488940,
                 2.094700 ,1.497165, 1.578075 ,1.524735 ,1.870495, 1.481710, 1.973875, 1.599035, 1.221080,
                 1.809575 ,1.605450, 1.473920, 2.042855, 3.197480 ,1.322965, 1.883575 ,1.987500 ,1.818460,
                 1.581980 ,1.730450, 1.458175 ,2.012490 ,1.180645 ,1.650580, 1.697135 ,1.844760, 1.633950 )

color_palette <- colorNumeric(palette = "viridis", domain = strength120)

mappa <- leaflet( data = stazioni.usate) %>%
  addTiles() %>%
  # addAwesomeMarkers(lng=stazioni$lng, lat=stazioni$lat) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolygons(data = map_lombardia, stroke = T, opacity = 1, color = "blue",
              fillOpacity = .1, fill = T, fillColor =  "blue",dashArray = "3" , weight = 1)   %>%
  addCircleMarkers(
    ~ lng, ~ lat,
    fillOpacity = 1,
    color = ~color_palette(strength120) ,
    radius = 4
  ) %>%
  addLegend(
    pal = color_palette,
    values = ~strength120,
    title = "&#x03B7; effect",
    opacity = 1
  )

mappa


strength180 <- c(0.3477005 , -0.0680988 , -0.674555  , -0.8092395 ,  0.5908655 ,
                 -0.989601  , -1.12846   ,  0.826025  , -0.02671945, -0.09674225,
                 -0.169612  , -2.93162   , -0.6333455 ,  0.6763975 ,  0.473053  ,
                 -0.912664  , -0.141275  ,  0.1007385 ,  0.796203  , -0.6310385 ,
                 -0.176847  , -0.911738  , -0.02962505, -1.030415  ,  0.437789  ,
                 -0.155127  , -1.08409   ,  0.388442  , -0.624366  , -0.703545  ,
                 0.695023  ,  1.145555  , -1.26213   ,  0.6447765 ,  0.467545  ,
                 -0.1882655 , -0.803763  ,  0.114107  , -0.684858  ,  0.7084855 ,
                 -1.19519   , -0.3621425 , -0.531716  ,  0.595513  , -0.463031 )

color_palette <- colorNumeric(palette = "viridis", domain = strength180)

mappa <- leaflet( data = stazioni.usate) %>%
  addTiles() %>%
  # addAwesomeMarkers(lng=stazioni$lng, lat=stazioni$lat) %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolygons(data = map_lombardia, stroke = T, opacity = 1, color = "blue",
              fillOpacity = .1, fill = T, fillColor =  "blue",dashArray = "3" , weight = 1)   %>%
  addCircleMarkers(
    ~ lng, ~ lat,
    fillOpacity = 1,
    color = ~color_palette(strength180) ,
    radius = 4
  ) %>%
  addLegend(
    pal = color_palette,
    values = ~strength180,
    title = "&#x03B7; effect",
    opacity = 1
  )

mappa



# ------------------------------------------------------------------------------

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


days <- 1:96

x11()
matplot(wrapped[,5:55], type = 'l', xaxt = "n", 
        ylab = "Ozone concentration")
abline(h = c(120,180),  lty = 5)
for (i in 5:55) {
  bigpoints <- which(wrapped[,i]>120)
  points( bigpoints, t(wrapped[bigpoints,i]) ,  pch = 16, col =  'darkorange' , cex = .35 )
}
for (i in 5:55) {
  bigpoints <- which(wrapped[,i]>180)
  points( bigpoints, t(wrapped[bigpoints,i]) ,  pch = 16, col =  'darkorange3', cex = .6)
}
legend("topleft", legend=c("Values > 120", "Values > 180"), 
       cex = .6, col = c("darkorange","darkorange3" ), pch = c(16, 16))


####Plot of quatities of interest####
y120 <- read.csv("./Datasets/Dataset_120.csv")
y180 <- read.csv("./Datasets/Dataset_180.csv")
colnames(y120)[2] <- "IdSensore"
colnames(y180)[2] <- "IdSensore"
y120$Count_180 = y180$Count_180

x11()
par(mfrow = c(2,1))

wrapped120 = y120 %>%
  pivot_wider(names_from = IdSensore, values_from = Count_120, values_fill = 0)
matplot(wrapped120[-c(1,2)], type = 'l',ylab = "Day count", 
        main = "Days with MA > 120",xaxt = "n" )
ticks <- seq(4.5,88.5,by = 7)
labels <- c("2010","2011","2012","2013","2014","2015","2016","2017",
            "2018","2019","2020","2021","2022") 
axis(1, at = c(ticks), labels = labels)
abline(v = seq(8,86,7), lty = 4) 

y120$Count_180 = NULL
wrapped180 = y180 %>%
  pivot_wider(names_from = IdSensore, values_from = Count_180, values_fill = 0)
matplot(wrapped180[-c(1,2)], type = 'l', ylab = "Day count", ylim = c(0,30),
        xaxt = "n",main = "Days with max > 180")

ticks <- seq(4.5,88.5,by = 7)
labels <- c("2010","2011","2012","2013","2014","2015","2016","2017",
            "2018","2019","2020","2021","2022")
axis(1, at = c(ticks), labels = labels)
abline(v = seq(8,86,7), lty = 4)

