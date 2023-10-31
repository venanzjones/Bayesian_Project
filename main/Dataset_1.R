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

counts <- NULL
sensors <- unique(ozono$idSensore)
years <- unique((ozono$Year))
mesi <- 5:10
#We have to consider only from 5 to 10 in the month
giorni_true <- c(31, 30, 31, 31, 30, 31)
mis_month <- 0
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
        miss <- setdiff(1:giorni_true[k], giorni)
        for (d in 1:length(giorni))
        {
          lista_giorni[[d]] <- temp_mese[which(temp_mese$Day==giorni[d]) ,]
        }
        count <- NULL
        na <- NULL
        missing_hours <- NULL
        for (d in 1:length(giorni))
        {
          na[d] <- sum(is.na(lista_giorni[[d]]$Valore))
          count[d] <- sum(na.omit(lista_giorni[[d]]$Valore)>180)>0
          missing_hours[d] <- 24-(dim(lista_giorni[[d]])[1])
        }
        counts <- rbind(counts, c(sum(count), sensors[i], years[j], mesi[k], length(miss), sum(missing_hours), mean(missing_hours), sum(na)))
      }
      else
      {
        print(c(sensors[i], mesi[k], years[j]))
        mis_month <- mis_month + 1
      }
    }
  }
}

counts <- data.frame(counts)
mis_month
colnames(counts) <- c("Count", "idSensore", "Year", "Month", "#Missing days", "Total hours missing", "Mean of missing hours per day", "Nas on 720 obs.")
counts <- data.frame(counts, total_miss = counts[, 6]+counts[, 8])
names(counts)[5] <- "#Missing days"

#Mancano dei giorni, il codice dovrà tenerne conto, facciamo media (?) ma successiva, prima vediamo se funziona
#Mancano delle righe, verranno ttrattate come Nas, infatti se queste dovessero essere >180 allora assumiamo che una
#tra quella prima o quella dopo sforino la soglia.

####Commenti su NAs####

sum(counts$`#Missing days`>0)
hist(counts$`#Missing days`[which(counts$`#Missing days`>0)])
mean(counts$`#Missing days`[which(counts$`#Missing days`>0)])
max(counts$`#Missing days`[which(counts$`#Missing days`>0)])
tail(sort(counts$`#Missing days`[which(counts$`#Missing days`>0)]))
#Mancano nel 10% dei mesi almeno un giorno. Quando ne manca uno ne mancano in media 2.2. Un'idea è stimare quanti
#producano effetti in base agli altri quando ne mancano e.g. <5

sum((counts$total_miss>0))
hist(counts$total_miss[which(counts$total_miss>0)])
mean(counts$total_miss[which(counts$total_miss>0)])
median(counts$total_miss[which(counts$total_miss>0)])
max(counts$total_miss[which(counts$total_miss>0)])
tail(sort(counts$total_miss[which(counts$total_miss>0)]))
#In metà dei mesi del dataset c'è almeno un na, la media quando ce ne è uno è 30 al mese.

#Attenzione che questi due tipi di problemi vanno anche valutati insieme, anche in mesi contigui.
