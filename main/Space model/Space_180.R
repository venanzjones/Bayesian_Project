library(gstat)
library(sp)

####Introductive analysis for the space-dependance study on the model####
data <- read.csv("Datasets/eta_180.csv")
ind <- read.csv("Datasets/Dataset_180.csv")
stazioni <- read.csv("Dati_iniziali/stazioni_O3.csv")
to_get <- which(stazioni$IdSensore %in% gsub("X", "", names(data)))
stazioni <- stazioni[to_get ,]

dati <- data.frame(val=sapply(data, median), variance = sapply(data, var), id = stazioni$IdSensore, lat = stazioni$lat, lon=stazioni$lng)

coordinates(dati) <- c('lat','lon')
svgm <- variogram(val ~ 1, dati)
plot(svgm, main = 'Sample Variogram',pch=19)

v.fit <- fit.variogram(svgm, vgm(nugget = 0, model = 'Gau', range = 0.5, psill = 1))  #For more see cheat sheet and enter in the console vgm()
plot(svgm, v.fit, pch = 19)

v.fit

temp <- data.frame(value = dati$val/dati$variance, dati)
coordinates(temp) <- c('lat','lon')
svgm <- variogram(value ~ 1, temp)
plot(svgm, main = 'Sample Variogram',pch=19)

stazioni <- stazioni %>% arrange(IdSensore)
#write.csv(stazioni, file = "Dati_iniziali/stazioni.csv")
stazioni <- read.csv("Dati_iniziali/stazioni.csv")

plot(dati$variance)
View(cbind(dati$variance, dati$id))

var2 <- which(dati$variance>1.5)
var1 <- which(dati$variance<1.5 & dati$variance>0.7)

# Carica la libreria geosphere
library(geosphere)

distGeo(c(stazioni$lat[1],stazioni$lng[1]), c(stazioni$lat[2],stazioni$lng[2]))/1000

dist_mat <- matrix(0, ncol = dim(stazioni)[1], nrow = dim(stazioni)[1])
for (i in 1:dim(stazioni)[1])
{
  for (j in i:dim(stazioni)[1])
  {
    dist_mat[i,j] <- dist_mat[j,i] <- distGeo(c(stazioni$lat[i],stazioni$lng[i]), c(stazioni$lat[j],stazioni$lng[j]))/1000
  }
}

0.229*max(dist_mat)

write.csv(dist_mat, file="Datasets/distances.csv")

#######
staz <- ind$idSensore[which(ind$Count_180!=0)]
write.csv(staz, file = "./Datasets/stazzip.csv")
year <- ind$Year[which(ind$Count_180!=0)]
write.csv(year, file = "./Datasets/yearzip.csv", row.names = FALSE)