library(dplyr)
library(sp)
library(gstat)

####Introductive analysis for the space-dependance study on the model####
data <- read.csv("Datasets/Dataset_180.csv")
stazioni <- read.csv("Dati_iniziali/stazioni_O3.csv")
to_get <- which(stazioni$IdSensore %in% unique(data$idSensore))
stazioni <- stazioni[to_get ,]

#Prendiamo il primo mese del primo anno e facciamo un modello base, vediamo che succede
first <- data[which(data$Year==2015 & data$Month==6) ,]
stazioni_sorted <- stazioni %>% arrange(IdSensore)
first <- data.frame(first, lat = stazioni_sorted$lat, lon = stazioni_sorted$lng)
first <- first[which(!is.na(first$Count_180)) ,]

coordinates(first) <- c('lat','lon')
svgm <- variogram(Count_180 ~ 1, first)
plot(svgm, main = 'Sample Variogram',pch=19)

mesi <- unique(data$Month)
x11()
par(mfrow=c(3,2))
for (i in 1:(length(mesi)-1))
{
  first <- data[which(data$Year==2015 & data$Month==mesi[i]) ,]
  stazioni_sorted <- stazioni %>% arrange(IdSensore)
  first <- data.frame(first, lat = stazioni_sorted$lat, lon = stazioni_sorted$lng)
  first <- first[which(!is.na(first$Count_180)) ,]
  
  coordinates(first) <- c('lat','lon')
  svgm <- variogram(Count_180 ~ 1, first)
  plot(svgm$dist, svgm$gamma, col='skyblue', main = 'Sample Variogram',pch=19)
}

data <- read.csv("Datasets/Residuals.csv")
#These should be predicted - actual

mean(colMeans(data)) #Ok

aux <- read.csv("Datasets/Dataset_180.csv")
aux <- aux[!is.na(aux$Count_180), -1]
res <- colMeans(data)
res <- data.frame(aux, res)

##Inizio tentativo
first <- res[which(res$Year==2015 & res$Month==6) ,]
stazioni_sorted <- stazioni[which(stazioni$IdSensore %in% first$idSensore) ,] %>% arrange(IdSensore)
first <- data.frame(first, lat = stazioni_sorted$lat, lon = stazioni_sorted$lng)

coordinates(first) <- c('lat','lon')
svgm <- variogram(res ~ 1, first)
plot(svgm, main = 'Sample Variogram',pch=19)

mesi <- unique(res$Month)
x11()
par(mfrow=c(3,2))
for (i in 1:(length(mesi)-1))
{
  first <- res[which(res$Year==2011& res$Month==mesi[i]) ,]
  stazioni_sorted <- stazioni[which(stazioni$IdSensore %in% first$idSensore) ,] %>% arrange(IdSensore)
  first <- data.frame(first, lat = stazioni_sorted$lat, lon = stazioni_sorted$lng)

  coordinates(first) <- c('lat','lon')
  svgm <- variogram(res ~ 1, first)
  plot(svgm$dist, svgm$gamma, col='skyblue', main = 'Sample Variogram',pch=19)
}

####Introductive analysis for the space-dependance study on the model####
data <- read.csv("Datasets/eta_180.csv")
stazioni <- read.csv("Dati_iniziali/stazioni_O3.csv")
to_get <- which(stazioni$IdSensore %in% gsub("X", "", names(data)))
stazioni <- stazioni[to_get ,]

dati <- data.frame(val=colMeans(data), id = stazioni$IdSensore, lat = stazioni$lat, lon=stazioni$lng)

coordinates(dati) <- c('lat','lon')
svgm <- variogram(val ~ 1, dati)
plot(svgm, main = 'Sample Variogram',pch=19)
