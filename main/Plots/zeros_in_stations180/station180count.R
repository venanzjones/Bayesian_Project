library(readr)
Dataset_180 <- read_csv("...") #add path for Dataset_180
head(Dataset_180)

ds <- Dataset_180[,1:2]

plot(ds$idSensore, ds$Count_180)

sensore <- unique(ds$idSensore)

par(mfrow = c(3,5))
for (i in 1:15){
  hist(ds$Count_180[which(ds$idSensore == sensore[i])], main = paste("Histogram of" , sensore[i]))
}

par(mfrow = c(3,5))
for (i in 16:30){
  hist(ds$Count_180[which(ds$idSensore == sensore[i])], main = paste("Histogram of" , sensore[i]))
}

par(mfrow = c(3,5))
for (i in 31:45){
  hist(ds$Count_180[which(ds$idSensore == sensore[i])], main = paste("Histogram of" , sensore[i]))
}






