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
mesi <- 3:8
#We have to consider only from 5 to 10 in the month
giorni_true <- c(31, 30, 31, 30, 31, 31)
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
          Na_giorni[i, 6*(j-1)+k] <- length(miss)
        }
        #Update su ore mancanti o Na
        if (sum(missing_hours))
        {
          Na_ore[i, 6*(j-1)+k] <- sum(missing_hours)
        }
      }
      else
      {
        mis_month <- mis_month + 1
        #Do something on Na_mese, i sensor, j index of year, k index month
        Na_mese[i, 6*(j-1)+k] <- 1
      }
    }
  }
}

mis_month
image(Na_mese)
#Non sono un problema, balziamo il mese e andiamo avanti

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
mesi <- 3:8
#We have to consider only from 5 to 10 in the month
giorni_true <- c(31, 30, 31, 30, 31, 31)
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
        counts <- rbind(counts, c(sum(count), sensors[i], years[j], mesi[k], length(miss), sum(total_miss>5), sum(total_miss>10)))
      }
      else
      {
        print(i)
      }
    }
  }
}

counts <- data.frame(counts)
colnames(counts) <- c("Count", "idSensore", "Year", "Month", "Missing days", "Days with more than 5 hours missed", "Days with more than 10 hours missed")

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

sum(counts$`Days with more than 5 hours missed`)
mean(counts$`Days with more than 5 hours missed`)
median(counts$`Days with more than 10 hours missed`)
hist(counts$`Days with more than 10 hours missed`)

sum(counts$`Days with more than 10 hours missed`+counts$`Missing days`>5)
#204 mesi verrebbero buttati a causa di Nas

#quanti mesi li mettiamo come Na?
sum(counts$`Days with more than 10 hours missed`+counts$`Missing days`>0)
#Su 3850 osservazioni totali -> 3646 mesi rimarrebbero
#Subito Na -> 1200 (33%)
#Più di un giorno diventa Na -> 918 Nas (25%)
#Più di due giorni -> 631 (17%)        #My choice, e se sono due li stimiamo con una Bernoulli
#Più di 3 -> 434 (11%)

#Definitivo:
#I mesi con più di 5 giorni mancanti vengono buttati, con meno di 5 giorni ma più di x (io direi 22) stimati dal modello
#a meno che non sia meno di due giorni in quel caso lo stimiamo (es.Bernoulli)

#Il giorno è "dichiarato mancante" se manca il giorno, oppure se mancano più di 10 ore di osservazioni.
#Con meno di 10 ore di informazioni invece stimiamo il livello di O3 con un medione e usiamo questo come dato, 
#nella pratica, consideriamo il giorno così com'è, infatti se la media è >180 anche uno dei due estremi è >180.

####Costruzione definitiva dataset####

disc <- 0
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
        if (length(miss)+sum(total_miss>10)>5)
        {
          disc <- disc + 1
        }
        else
        {
          if (length(miss)+sum(total_miss>10)>2)
          {
            counts <- rbind(counts, c(NA, sensors[i], years[j], mesi[k]))
          }
          else
          {
            if (length(miss)+sum(total_miss>10))
            {
              added <- rbinom(n = 1, size = length(miss)+sum(total_miss>10), prob = sum(count)/giorni_true[k])
              counts <- rbind(counts, c(sum(count)+added, sensors[i], years[j], mesi[k]))
            }
            else
            {
              counts <- rbind(counts, c(sum(count), sensors[i], years[j], mesi[k]))
            }
          }
        }
      }
      else
      {
        disc <- disc + 1
      }
    }
  }
}

counts <- data.frame(counts)
colnames(counts) <- c("Count", "idSensore", "Year", "Month")

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

matplot(mat, type='l')


