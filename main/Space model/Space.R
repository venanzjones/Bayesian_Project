library(gstat)
library(sp)
library(fdagstat)

####Introductive analysis for the space-dependance study on the model - 180####
data <- read.csv("Datasets/eta_180.csv")
ind <- read.csv("Datasets/Dataset_180.csv")
stazioni <- read.csv("Dati_iniziali/stazioni_O3.csv")
to_get <- which(stazioni$IdSensore %in% ind$idSensore)
stazioni <- stazioni[to_get ,]

dati <- data.frame(val=colMeans(data), variance = sapply(data, var), id = stazioni$IdSensore, lat = stazioni$lat, lon=stazioni$lng)

coordinates(dati) <- c('lat','lon')
svgm <- variogram(val ~ 1, dati)
plot(svgm, main = 'Sample Variogram',pch=19)

v.fit <- fit.variogram(svgm, vgm(nugget = 0, model = 'Exp', range = 0.5, psill = 0.5))  #For more see cheat sheet and enter in the console vgm()
plot(svgm, v.fit, pch = 19)

v.fit

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

write.csv(dist_mat, file="Datasets/distances.csv")

#### Residui ####
res <- (read.csv("Datasets/res_180.csv"))
res <- data.frame(staz= NA, lat=NA, lon=NA, res=colMeans(res))
ind <- ind[-which(is.na(ind$Count_180)) ,]
for (i in 1:dim(res)[1])
{
  res[i,1] <- ind$idSensore[i]
  res[i,2] <- stazioni$lat[which(stazioni$IdSensore==res[i,1])]
  res[i,3] <- stazioni$lng[which(stazioni$IdSensore==res[i,1])]
}

staz <- unique(res$staz)
which(res$staz==staz[i+3])
residui_func <- matrix(0, nrow=91, ncol=length(staz))
ye <- unique(ind$Year)
mo <- unique(ind$Month)
for (i in 1:length(staz))
{
  for (j in 1:length(ye))
  {
    for (k in 1:length(mo))
    {
      temp <- res$res[which(ind$idSensore==staz[i]&ind$Year==ye[j]&ind$Month == mo[k])]
      if(length(temp))
      {
        residui_func[(j-1)*7+k,i] <- temp
      }
    }
  }
}

matplot(residui_func, type='l')
data <- data.frame((residui_func))
coord <- data.frame(unique(res$lat), unique(res$lon))

g <- fstat(NULL, vName = "FWPR", Coordinates = coord, Functions = data, scalar = FALSE)
g <- estimateDrift("~.", g, Intercept = TRUE)
g <- fvariogram("~.", g, Nlags = 50, LagMax = 5, ArgStep = 1, useResidual = TRUE, comments=FALSE)
plotVariogram(g)

####Eta model - 120####
data <- read.csv("Datasets/eta_120.csv")
ind <- read.csv("Datasets/Dataset_120.csv")
stazioni <- read.csv("Dati_iniziali/stazioni_O3.csv")
to_get <- which(stazioni$IdSensore %in% ind$idSensore)
stazioni <- stazioni[to_get ,]

dati <- data.frame(val=colMeans(data), variance = sapply(data, var), id = stazioni$IdSensore, lat = stazioni$lat, lon=stazioni$lng)

coordinates(dati) <- c('lat','lon')
svgm <- variogram(val ~ 1, dati)
plot(svgm, main = 'Sample Variogram',pch=19)

v.fit <- fit.variogram(svgm, vgm(nugget = 0.01, model = 'Exp', range = 0.4, psill = 0.05))  #For more see cheat sheet and enter in the console vgm()
plot(svgm, v.fit, pch = 19)

v.fit
v.fit$range[2]/3  #0.017

#dati <- dati[-which(dati$val<(-0.5)) ,]
res <- t(read.csv("Datasets/res_120.csv"))
res <- data.frame(res=res, staz= NA, lat=NA, lon=NA)
ind <- ind[-which(is.na(ind$Count_120)) ,]
for (i in 1:dim(res)[1])
{
  res[i,2] <- ind$idSensore[i]
  res[i,3] <- stazioni$lat[which(stazioni$IdSensore==res[i,2])]
  res[i,4] <- stazioni$lng[which(stazioni$IdSensore==res[i,2])]
}

coordinates(res) <- c("lat", "lon")
svgm <- variogram(res ~ 1, res)
plot(svgm, main = 'Sample Variogram',pch=19)
res <- res[-which(abs(res$res)>5) ,]
svgm <- variogram(res ~ 1, res)
plot(svgm, main = 'Sample Variogram',pch=19)