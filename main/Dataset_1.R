library(lubridate)
library(rstan)
library(dplyr)
library(tidyr)

ozono = read.csv("datasetO3.csv")
stazioni = read.csv("stazioni_O3.csv")
ozono$idOperatore = NULL
stazioni.usate <- stazioni[which(stazioni$IdSensore %in% unique(ozono$idSensore)),]
start.stop <- data.frame(stazioni.usate$DataStart, stazioni.usate$DataStop)

started.late <- start.stop[which(as.Date(start.stop$stazioni.usate.DataStart, format = "%d/%m/%Y") > 
                                   as.Date("01/01/2010", format = "%d/%m/%Y")),] 

closed.early <- start.stop[which(as.Date(start.stop$stazioni.usate.DataStop, format = "%d/%m/%Y") > 
                                   as.Date("01/01/2010", format = "%d/%m/%Y")),] 
ozono$Data <- mdy_hms(ozono$Data)
ozono$Year <- year(ozono$Data-1)    
ozono$Month <- month(ozono$Data-1)
ozono$Day <- day(ozono$Data-1)    
ozono$Hour <- hour(ozono$Data-1)  

sapply(ozono, function(y) sum(length(which(is.na(y)))))

####Tentativo plot Nas####

#Prima plot di mesi mancanti, poi giorni, poi ore, tutti e tre da considerare insieme

#In generale, in ogni riga voglio un sensore, e nelle colonne le varie cose
#Ideale sarebbe un plot con colori, tipo rosso manca mese, verde mancano giorni e blu mancano ore

sensors <- unique(ozono$idSensore)
years <- unique((ozono$Year))
mesi <- 3:10
#We have to consider only from 5 to 10 in the month
giorni_true <- c(31, 30, 31, 30, 31, 31, 30, 31)
mis_month <- 0
lista_sensori <- list()
Na_mese <- matrix(rep(0, length(sensors)*length(mesi)*length(years)), nrow = length(sensors), ncol=(length(mesi)*length(years)))
Na_giorni <- matrix(rep(0, length(sensors)*length(mesi)*length(years)), nrow = length(sensors), ncol=(length(mesi)*length(years)))
Na_ore <- matrix(rep(0, length(sensors)*length(mesi)*length(years)), nrow = length(sensors), ncol=(length(mesi)*length(years)))
for (i in 1:length(sensors))
{
  lista_sensori[[i]] <- ozono[which(ozono$idSensore==sensors[i]) ,]
}

for (i in 1:length(lista_sensori))
{
  #Entro qua per ogni sensore
  temp <- lista_sensori[[i]]
  lista_anni <- list()
  for (j in 1:length(years))
  {
    lista_anni[[j]] <- temp[which(temp$Year == years[j]) ,]
  }
  #Ogni elemento di lista_anni è per un sensore per un anno
  
  for (j in 1:length(years))
  {
    temp_year <- lista_anni[[j]]
    lista_mesi <- list()
    for (k in 1:length(mesi))
    {
      lista_mesi[[k]] <- temp_year[which(temp_year$Month==mesi[k]) ,]
    }
    
    #Mese per anno per sensore, se il mese non c'è lista_mesi[[k]] è un null
    
    for (k in 1:length(mesi))
    {
      temp_mese <- lista_mesi[[k]]
      giorni <- unique(lista_mesi[[k]]$Day)
      lista_giorni <- list()
      
      #Se il mese non c'è non entra nell'if, altrimenti si prende temp_mese che è un mese di un anno di un sensore
      
      if(length(giorni))
      {
        miss <- setdiff(1:giorni_true[k], giorni)
        for (d in 1:length(giorni))
        {
          lista_giorni[[d]] <- temp_mese[which(temp_mese$Day==giorni[d]) ,]
        }
        na <- NULL
        missing_hours <- NULL
        for (d in 1:length(giorni))
        {
          na[d] <- sum(is.na(lista_giorni[[d]]$Valore))
          missing_hours[d] <- 24-(dim(lista_giorni[[d]])[1])
        }
        #Update sui giorni mancanti
        if (length(miss))
        {
          Na_giorni[i, length(mesi)*(j-1)+k] <- length(miss)
        }
        #Update su ore mancanti o Na
        if (sum(missing_hours))
        {
          Na_ore[i, length(mesi)*(j-1)+k] <- sum(missing_hours)
        }
      }
      else
      {
        mis_month <- mis_month + 1
        #Do something on Na_mese, i sensor, j index of year, k index month
        Na_mese[i, length(mesi)*(j-1)+k] <- 1
      }
    }
  }
}

mis_month
image(Na_mese, y = 1:104)
sum(Na_mese>0)
#Non sono un problema, balziamo il mese e andiamo avanti -> 128

x11()
par(mfrow=c(1,2))
image(Na_giorni)
image(Na_ore)

C <- NULL
for(i in 1:length(sensors))
{
  C[i] <- cor(Na_giorni[i ,], Na_ore[i ,])
}
mean(na.omit(C))

Means_per_col <- colMeans(Na_ore)
Na_per_month <- rep(0, length(mesi))
for(i in 1:length(years))
{
  for (j in 1:length(mesi))
  {
    Na_per_month[j] <- Na_per_month[j]+Means_per_col[(i-1)*length(mesi)+j]
  }
}
Na_per_month

####Inizio costruzione dataset####

counts <- NULL
sensors <- unique(ozono$idSensore)
years <- unique((ozono$Year))
mesi <- 3:10
#We have to consider only from 5 to 10 in the month
giorni_true <- c(31, 30, 31, 30, 31, 31, 30, 31)
lista_sensori <- list()
for (i in 1:length(sensors))
{
  lista_sensori[[i]] <- ozono[which(ozono$idSensore==sensors[i]) ,]
}

for (i in 1:length(lista_sensori))
{
  temp <- lista_sensori[[i]]
  lista_anni <- list()
  for (j in 1:length(years))
  {
    lista_anni[[j]] <- temp[which(temp$Year == years[j]) ,]
  }
  
  for (j in 1:length(years))
  {
    temp_year <- lista_anni[[j]]
    lista_mesi <- list()
    for (k in 1:length(mesi))
    {
      lista_mesi[[k]] <- temp_year[which(temp_year$Month==mesi[k]) ,]
    }
    
    for (k in 1:length(mesi))
    {
      temp_mese <- lista_mesi[[k]]
      giorni <- unique(lista_mesi[[k]]$Day)
      lista_giorni <- list()
      
      if(length(giorni))
      {
        #Il mese non è un problema, quindi devo vedere qua dentro quanti giorni butterei.
        miss <- setdiff(1:giorni_true[k], giorni)
        #Giorni interi mancanti
        for (d in 1:length(giorni))
        {
          lista_giorni[[d]] <- temp_mese[which(temp_mese$Day==giorni[d]) ,]
        }
        count <- NULL
        na <- NULL
        missing_hours <- NULL
        total_miss <- NULL
        for (d in 1:length(giorni))
        {
          na[d] <- sum(is.na(lista_giorni[[d]]$Valore))
          count[d] <- (sum(na.omit(lista_giorni[[d]]$Valore)>180)>0)
          missing_hours[d] <- 24-(dim(lista_giorni[[d]])[1])
          total_miss[d] <- missing_hours[d]+na[d]
        }
        counts <- rbind(counts, c(sum(count), sensors[i], years[j], mesi[k], length(miss), sum(total_miss>3), sum(total_miss>5), sum(total_miss>8), sum(total_miss>10)))
      }
      else
      {
        print(i)
      }
    }
  }
}

counts <- data.frame(counts)
colnames(counts) <- c("Count", "idSensore", "Year", "Month", "Missing days", "Days with more than 3 hours missed", "Days with more than 5 hours missed", "Days with more than 8 hours missed", "Days with more than 10 hours missed")

#Mancano dei giorni, il codice dovrà tenerne conto, facciamo media (?) ma successiva, prima vediamo se funziona
#Mancano delle righe, verranno ttrattate come Nas, infatti se queste dovessero essere >180 allora assumiamo che una
#tra quella prima o quella dopo sforino la soglia.

###Commenti su NAs###

sum(counts$`Missing days`>0)
hist(counts$`Missing days`[which(counts$`Missing days`>0)])
mean(counts$`Missing days`[which(counts$`Missing days`>0)])
max(counts$`Missing days`[which(counts$`Missing days`>0)])
tail(sort(counts$`Missing days`[which(counts$`Missing days`>0)]))
#Mancano nel 10% dei mesi almeno un giorno. Quando ne manca uno ne mancano in media 2.2. Un'idea è stimare quanti
#producano effetti in base agli altri quando ne mancano e.g. <5
#Devo contare in quanti mesi mancano più di 5 giorni
sum(counts$`Missing days`>5)     #Li buttiamo tranquillamente

sum(counts$`Days with more than 3 hours missed`)
mean(counts$`Days with more than 3 hours missed`)
median(counts$`Days with more than 3 hours missed`)
hist(counts$`Days with more than 3 hours missed`)

sum(counts$`Days with more than 5 hours missed`)
mean(counts$`Days with more than 5 hours missed`)
median(counts$`Days with more than 5 hours missed`)
hist(counts$`Days with more than 5 hours missed`)

#Quando buttiamo il giorno? --> quando mancano più di x ore
sum(counts$`Days with more than 8 hours missed`>5)

#Quanti ne rimangono?
length(which(counts$`Missing days` + counts$`Days with more than 8 hours missed`>=5))+144 #Mesi rimossi
(length(which(counts$`Missing days` + counts$`Days with more than 8 hours missed`<5)))/(51*104) #Voglio il 90%

length(which(counts$`Missing days` + counts$`Days with more than 8 hours missed`>=6))+144 #Mesi rimossi
(length(which(counts$`Missing days` + counts$`Days with more than 8 hours missed`<=5)))/(51*104)

#Aiuto qualcuno ci aiuti, tuttavia togliamo un giorno quando abbiamo più di 8 ore mancanti
#Quando abbiamo meno di 6 giorni mancanti allora simuliamo il count con una bernoulliana

max(counts$Count[which(counts$Month==3)])
max(counts$Count[which(counts$Month==5)])
max(counts$Count[which(counts$Month==6)])
max(counts$Count[which(counts$Month==7)])
max(counts$Count[which(counts$Month==8)])
max(counts$Count[which(counts$Month==9)])
sum(counts$Count[which(counts$Month==10)])

#Marzo lo balziamo, non ha mai osservazioni

####Costruzione definitiva dataset####

rm(list=ls())
ozono = read.csv("datasetO3.csv")
stazioni = read.csv("stazioni_O3.csv")
ozono$Data <- mdy_hms(ozono$Data)
ozono$Year <- year(ozono$Data-1)    
ozono$Month <- month(ozono$Data-1)
ozono$Day <- day(ozono$Data-1)    
ozono$Hour <- hour(ozono$Data-1)

counts <- NULL
sensors <- unique(ozono$idSensore)
years <- unique((ozono$Year))
mesi <- 4:10
#We have to consider only from 5 to 10 in the month
giorni_true <- c(30, 31, 30, 31, 31, 30, 31)
lista_sensori <- list()
for (i in 1:length(sensors))
{
  lista_sensori[[i]] <- ozono[which(ozono$idSensore==sensors[i]) ,]
}

counts <- NULL
set.seed(1)
for (i in 1:length(lista_sensori))
{
  temp <- lista_sensori[[i]]
  lista_anni <- list()
  for (j in 1:length(years))
  {
    lista_anni[[j]] <- temp[which(temp$Year == years[j]) ,]
  }
  
  for (j in 1:length(years))
  {
    temp_year <- lista_anni[[j]]
    lista_mesi <- list()
    for (k in 1:length(mesi))
    {
      lista_mesi[[k]] <- temp_year[which(temp_year$Month==mesi[k]) ,]
    }
    
    for (k in 1:length(mesi))
    {
      temp_mese <- lista_mesi[[k]]
      giorni <- unique(lista_mesi[[k]]$Day)
      lista_giorni <- list()
      
      if(length(giorni))
      {
        miss <- setdiff(1:giorni_true[k], giorni)
        #Giorni interi mancanti
        for (d in 1:length(giorni))
        {
          lista_giorni[[d]] <- temp_mese[which(temp_mese$Day==giorni[d]) ,]
        }
        count <- NULL
        na <- NULL
        missing_hours <- NULL
        total_miss <- NULL
        added <- NULL
        for (d in 1:length(giorni))
        {
          na[d] <- sum(is.na(lista_giorni[[d]]$Valore))
          count[d] <- (sum(na.omit(lista_giorni[[d]]$Valore)>180)>0)
          missing_hours[d] <- 24-(dim(lista_giorni[[d]])[1])
          total_miss[d] <- missing_hours[d]+na[d]
        }
        if (length(miss)+sum(total_miss>8)>5)
        {
          counts <- rbind(counts, c(NA, sensors[i], years[j], mesi[k]))
        }
        else
        {
          added <- rbinom(n = 1, size = length(miss)+sum(total_miss>8), prob = sum(count)/(giorni_true[k]-(length(miss)+sum(total_miss>8))))
          counts <- rbind(counts, c(sum(count)+added, sensors[i], years[j], mesi[k]))
        }
      }
      else
      {
        counts <- rbind(counts, c(NA, sensors[i], years[j], mesi[k]))
      }
    }
  }
}

counts <- data.frame(counts)
colnames(counts) <- c("Count", "idSensore", "Year", "Month")

sum(is.na(counts$Count))/length(counts$Count)

#Manca da creare un dataset per fare una sorta di matplot, righe sensori colonne time

time <- NULL
for (i in years)
{
  for (j in mesi)
  {
    time <- c(time, paste(i, j))
  }
}
mat <- matrix(rep(0, length(sensors)*length(time)), nrow = length(sensors), ncol=length(time))
for (s in 1:length(sensors))
{
  for (i in 1:length(years))
  {
    for (j in 1:length(mesi))
    {
      if (length(which(counts$idSensore==sensors[s] & counts$Year==years[i] & counts$Month==mesi[j])))
      {
        mat[s, (i-1)*length(mesi)+j] <- counts$Count[which(counts$idSensore==sensors[s] & counts$Year==years[i] & counts$Month==mesi[j])]
      }
      else
      {
        mat[s, (i-1)*length(mesi)+j] <- NA
      }
    }
  }
}

matplot(t(mat), type='l')


