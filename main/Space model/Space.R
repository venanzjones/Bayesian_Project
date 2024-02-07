library(gstat)
library(sp)
library(fdagstat)

# Carica la libreria geosphere
library(geosphere)
stazioni <- read.csv("Dati_iniziali/stazioni_O3.csv")
ind <- read.csv("Datasets/Dataset_180.csv")
to_get <- which(stazioni$IdSensore %in% ind$idSensore)
stazioni <- stazioni[to_get ,]

distGeo(c(stazioni$lat[1],stazioni$lng[1]), c(stazioni$lat[2],stazioni$lng[2]))/1000

dist_mat <- matrix(0, ncol = dim(stazioni)[1], nrow = dim(stazioni)[1])
for (i in 1:dim(stazioni)[1])
{
  for (j in i:dim(stazioni)[1])
  {
    dist_mat[i,j] <- dist_mat[j,i] <- distGeo(c(stazioni$lat[i],stazioni$lng[i]), c(stazioni$lat[j],stazioni$lng[j]))/1000
  }
}

write.csv(dist_mat, file="Datasets/distances.csv")

####Introductive analysis for the space-dependance study on the model - 180####
data <- read.csv("Datasets/Space_estimate/eta_180.csv")

dati <- data.frame(val=colMeans(data), variance = sapply(data, var), id = stazioni$IdSensore, lat = stazioni$lat, lon=stazioni$lng)

coordinates(dati) <- c('lat','lon')
svgm <- variogram(I(val/variance) ~ 1, dati)
plot(svgm, main = 'Sample Variogram',pch=19)

v.fit <- fit.variogram(svgm, vgm(nugget = 0, model = 'Gau', range = 0.105, psill = 20))  #For more see cheat sheet and enter in the console vgm()
plot(svgm, v.fit, pch = 19)

v.fit

res <- read.csv("Datasets/Space_estimate/res_180.csv")
staz <- stazioni$IdSensore
val <- ind[, 1:2]
val <- val[-which(is.na(val$Count_180)) ,]
for (i in 1:length(staz))
{
  dati$val[which(dati$id==staz[i])] = median(colMeans(res[, which(val$idSensore==staz[i])]))
}

svgm <- variogram(val ~ 1, dati)
plot(svgm, main = 'Sample Variogram',pch=19)

####Eta model - 120####

data <- read.csv("Datasets/Space_estimate/eta_120.csv")

dati <- data.frame(val=colMeans(data), variance = sapply(data, var), id = stazioni$IdSensore, lat = stazioni$lat, lon=stazioni$lng)

coordinates(dati) <- c('lat','lon')
svgm <- variogram(I(val/variance) ~ 1, dati)
plot(svgm, main = 'Sample Variogram',pch=19)

v.fit <- fit.variogram(svgm, vgm(nugget = 0, model = 'Gau', range = 0.3, psill = 20))  #For more see cheat sheet and enter in the console vgm()
plot(svgm, v.fit, pch = 19)

v.fit

ind <- read.csv("Datasets/Dataset_120.csv")
res <- read.csv("Datasets/Space_estimate/res_120.csv")
staz <- stazioni$IdSensore
val <- ind[, 1:2]
val <- val[-which(is.na(val$Count_120)) ,]
for (i in 1:length(staz))
{
  dati$val[which(dati$id==staz[i])] = median(colMeans(res[, which(val$idSensore==staz[i])]))
}

svgm <- variogram(val ~ 1, dati)
plot(svgm, main = 'Sample Variogram',pch=19)
