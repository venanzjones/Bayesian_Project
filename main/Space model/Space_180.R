####Introductive analysis for the space-dependance study on the model####
data <- read.csv("Datasets/eta_180.csv")
ind <- read.csv("Datasets/Dataset_180.csv")
stazioni <- read.csv("Dati_iniziali/stazioni_O3.csv")
to_get <- which(stazioni$IdSensore %in% gsub("X", "", names(data)))
stazioni <- stazioni[to_get ,]

dati <- data.frame(val=colMeans(data), variance = sapply(data, var), id = stazioni$IdSensore, lat = stazioni$lat, lon=stazioni$lng)

coordinates(dati) <- c('lat','lon')
svgm <- variogram(val ~ 1, dati)
plot(svgm, main = 'Sample Variogram',pch=19)

temp <- data.frame(value = dati$val/dati$variance, dati)
coordinates(temp) <- c('lat','lon')
svgm <- variogram(value ~ 1, temp)
plot(svgm, main = 'Sample Variogram',pch=19)

stazioni <- stazioni %>% arrange(IdSensore)
write.csv(stazioni, file = "Dati_iniziali/stazioni.csv")

# Carica la libreria geosphere
library(geosphere)



# Coordinate del primo punto (latitudine, longitudine)
point1 <- c(45.4641, 9.1900)  # Ad esempio, Milano, Italia

# Coordinate del secondo punto (latitudine, longitudine)
point2 <- c(40.7128, -74.0060)  # Ad esempio, New York, USA

# Calcola la distanza tra i due punti in chilometri
distance_km <- distGeo(point1, point2) / 1000