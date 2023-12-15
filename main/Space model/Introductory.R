library(dplyr)
library(sp)
library(gstat)

####Introductive analysis for the space-dependance study on the model####
data <- read.csv("Datasets/Dataset_120.csv")
stazioni <- read.csv("Dati_iniziali/stazioni_O3.csv")
to_get <- which(stazioni$IdSensore %in% unique(data$idSensore))
stazioni <- stazioni[to_get ,]

#Prendiamo il primo mese del primo anno e facciamo un modello base, vediamo che succede
first <- data[which(data$Year==2015 & data$Month==6) ,]
stazioni_sorted <- stazioni %>% arrange(IdSensore)
first <- data.frame(first, lat = stazioni_sorted$lat, lon = stazioni_sorted$lng)
first <- first[-which(is.na(first$Count_120)) ,]

coordinates(first) <- c('lat','lon')
svgm <- variogram(Count_120 ~ 1, first)
plot(svgm, main = 'Sample Variogram',pch=19)

mesi <- unique(data$Month)
x11()
par(mfrow=c(3,2))
for (i in 1:(length(mesi)-1))
{
  first <- data[which(data$Year==2015 & data$Month==mesi[i]) ,]
  stazioni_sorted <- stazioni %>% arrange(IdSensore)
  first <- data.frame(first, lat = stazioni_sorted$lat, lon = stazioni_sorted$lng)
  first <- first[which(!is.na(first$Count_120)) ,]
  
  coordinates(first) <- c('lat','lon')
  svgm <- variogram(Count_120 ~ 1, first)
  plot(svgm$dist, svgm$gamma, col='skyblue', main = 'Sample Variogram',pch=19)
}


####Prova sugli eta####
data <- read.csv("Datasets/eta_120.csv")
stazioni <- read.csv("Dati_iniziali/stazioni_O3.csv")
to_get <- which(stazioni$IdSensore %in% gsub("X", "", names(data)))
stazioni <- stazioni[to_get ,]

dati <- data.frame(val=colMeans(data), id = stazioni$IdSensore, lat = stazioni$lat, lon=stazioni$lng)

coordinates(dati) <- c('lat','lon')
svgm <- variogram(val ~ 1, dati)
plot(svgm, main = 'Sample Variogram',pch=19)

####Prova su residui####
vari <- sapply(res, var)
plot(vari)

vari <- res/vari
means <- colMeans(vari)

row <- is.na(data$Count_120)
vals <- data[!row ,]
new <- NULL
for (i in 1:length(means))
{
  new <- rbind(new, cbind(means[i], vals$idSensore[i], vals$Year[i], vals$Month[i], 
                     stazioni$lat[which(stazioni$IdSensore==vals$idSensore[i])],
                     stazioni$lng[which(stazioni$IdSensore==vals$idSensore[i])]))
}

dati <- data.frame(new)
names(dati) <- c("val", "idSensore", "Year", "Month", "lat", "lon")

coordinates(dati) <- c('lat','lon')
svgm <- variogram(val ~ 1, dati)
plot(svgm, main = 'Sample Variogram',pch=19)
