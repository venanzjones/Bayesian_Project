library(lubridate)
library(dplyr)
library(tidyr)

ozono <- read.csv("Dati_Iniziali/datasetO3.csv", header = TRUE)
stazioni <- read.csv("Dati_Iniziali/stazioni_O3.csv")
ozono$idOperatore <- NULL
stazioni.usate <- stazioni[which(stazioni$IdSensore %in% unique(ozono$idSensore)), ]
start.stop <- data.frame(stazioni.usate$DataStart, stazioni.usate$DataStop)

started.late <- start.stop[which(as.Date(start.stop$stazioni.usate.DataStart, format = "%d/%m/%Y") >
                                   as.Date("01/01/2010", format = "%d/%m/%Y")), ]

closed.early <- start.stop[which(as.Date(start.stop$stazioni.usate.DataStop, format = "%d/%m/%Y") >
                                   as.Date("01/01/2010", format = "%d/%m/%Y")), ]
ozono$Data <- mdy_hms(ozono$Data)
ozono$Year <- year(ozono$Data - 1)
ozono$Month <- month(ozono$Data - 1)
ozono$Day <- day(ozono$Data - 1)
ozono$Hour <- hour(ozono$Data - 1)

####Creazione dataset con massimo giornaliero e medione in caso manchino i dati####
Massimi <- NULL
sensors <- unique(ozono$idSensore)
years <- unique((ozono$Year))
mesi <- 4:10
giorni_true <- c(30, 31, 30, 31, 31, 30, 31)
lista_sensori <- list()
missed_month <- 0
for (i in 1:length(sensors))
{
  lista_sensori[[i]] <- ozono[which(ozono$idSensore == sensors[i]), ]
}
for (i in 1:length(lista_sensori))
{
  temp <- lista_sensori[[i]]
  for (j in 1:length(years))
  {
    temp_year <- temp[which(temp$Year==years[j]) ,]   #Un sensore, un anno
    for (k in 1:length(mesi))
    {
      temp_mese <- temp_year[which(temp_year$Month==mesi[k]) ,]   #Singolo mese
      giorni <- unique(temp_mese$Day)
      new_month <- rep(-1, giorni_true[k])
      if (length(giorni))
      {
        lista_giorni <- list()
        for (d in 1:length(giorni))
        {
          lista_giorni[[giorni[d]]] <- temp_mese[which(temp_mese$Day == giorni[d]), ]
        }
        na <- NULL
        missing_hours <- NULL
        total_miss <- NULL
        for (d in 1:length(giorni))
        {
          na[d] <- sum(is.na(lista_giorni[[giorni[d]]]$Valore))
          missing_hours[d] <- 24 - (dim(lista_giorni[[giorni[d]]])[1])
          total_miss[d] <- missing_hours[d] + na[d]
          if (sum(na.omit(lista_giorni[[giorni[d]]]$Valore)>180))
          {
            total_miss[d] <- 0
          }
        }
        miss <- setdiff(1:giorni_true[k], giorni[!(total_miss>8)])
        
        if (length(giorni[!(total_miss>8)]))
        {
          for (a in 1:length(giorni[!(total_miss>8)]))
          {
            new_month[giorni[!(total_miss>8)][a]] <- max(na.omit(lista_giorni[[giorni[!(total_miss>8)][a]]]$Valore))
            if (!sum(na.omit(lista_giorni[[giorni[!(total_miss>8)][a]]]$Valore)))
            {
              print(c(i, j, k))
            }
          }
          
          Massimi <- rbind(Massimi, cbind(new_month, 1:giorni_true[k], rep(sensors[i], giorni_true[k]), rep(years[j], giorni_true[k]), rep(mesi[k], giorni_true[k])))
        }
      } 
      else 
      {
        missed_month <- missed_month + 1
      }
    }
  }
}

Massimi <- data.frame(Massimi)
names(Massimi) <- c('max', 'Giorno', 'idSensore', 'Anno', 'Mese')
rm(ozono)

# estraggo il mese dal dataset massimi, me li salvo in una lista
# di mesi e la scorro, allo stesso tempo devo però salvarmi gli
# indici delle righe per risalire al dataset massimi e fare medione

## rpova con un mese
temp_month <- Massimi[which(Massimi$idSensore == 5707 & Massimi$Anno == 2010 & Massimi$Mese == 4),]
valori_popolati <- NULL

for(k in 1:dim(temp_month)[1]){
  # se i -1 sono >= 6 metto il mese con valore NA nel dataset finale
  if (sum(temp_month$max[which(temp_month$max == -1)] >= 6)){
    valori_popolati <- "NA"
  }
  
  if (temp_month[k, "max"] == -1){
    j = k+1
    while(temp_month[j, "max"] == -1){
      j <- j+1
    }
    valori_popolati[k:j-1] <- (temp_month[k-1,"max"]+temp_month[j, "max"])/2
  }
  else {
    valori_popolati[k] <- temp_month[k, "max"]
  }
}

temp_anno <-Massimi[which(Massimi$idSensore == 5707 & Massimi$Anno == 2010),]
  





if (new_month[1] == -1)  #Caso degenere
{
  
}






for (a in 1:giorni_true[k])
{
  if (new_month[a] == -1)
  {
    for (b in (a+1):giorni_true[k])
    {
      if(new_month[b] != -1)
        new_month[a:(b-1)] <- mean(new_month[a-1], new_month[b])
    }
    #Se arrivo qua devo andare al mese successivo
  }
}
####Prova con medione####

counts <- NULL
sensors <- unique(ozono$idSensore)
years <- unique((ozono$Year))
mesi <- 4:10
missed_month <- 0
# We have to consider only from 5 to 10 in the month
giorni_true <- c(30, 31, 30, 31, 31, 30, 31)
giorno_prima <- 0
lista_sensori <- list()
for (i in 1:length(sensors))
{
  lista_sensori[[i]] <- Massimi[which(Massimi$idSensore == sensors[i]), ]
}

for (i in 1:length(lista_sensori))
{
  temp <- lista_sensori[[i]]
  lista_anni <- list()
  for (j in 1:length(years))
  {
    lista_anni[[j]] <- temp[which(temp$Anno == years[j]), ]
  }
  
  for (j in 1:length(years))
  {
    temp_year <- lista_anni[[j]]
    lista_mesi <- list()
    for (k in 1:length(mesi))
    {
      lista_mesi[[k]] <- temp_year[which(temp_year$Mese == mesi[k]), ]
    }
    
    for (k in 1:length(mesi))
    {
      temp_mese <- lista_mesi[[k]]
      giorni <- unique(lista_mesi[[k]]$Giorno)
      if (length(giorni)) 
      {
        maxs <- NULL
        for (d in 1:length(giorni))
        {
          maxs[d] <- temp_mese[which(temp_mese$Giorno == giorni[d]), ]$max
        }
        if (sum(maxs==-1)<6)
        {
          #Codice per riempire i buchi
          row <- which(maxs==-1)
          #Sto impazzendo mi devo fermare
          #Codice per riempire i buchi
          
          counts <- rbind(counts, c(sum(maxs>180), sensors[i], years[j], mesi[k]))
        }
        else
        {
          counts <- rbind(counts, c(NA, sensors[i], years[j], mesi[k], length(miss)))
        }
      } 
      else 
      {
        counts <- rbind(counts, c(NA, sensors[i], years[j], mesi[k], length(miss)))
      }
    }
  }
}

counts <- data.frame(counts)
colnames(counts) <- c("Count", "idSensore", "Year", "Month")





## Coppe
mm_na <- NULL

# Filling the vector mm_na with whether a month is admissible or not
for (i in seq_along(sensors)) {
  temp_years <- unique(Massimi$Anno[which(Massimi$idSensore == sensors[i])])
  for (j in seq_along(temp_years)) {
    temp_mese <- unique(Massimi$Mese[which(Massimi$idSensore == sensors[i] & Massimi$Anno == temp_years[j])])
    for (k in seq_along(temp_mese)) {
      temp <- Massimi[which(Massimi$idSensore == sensors[i] & Massimi$Anno == temp_years[j] & Massimi$Mese == temp_mese[k]),]
      if (sum(temp$max == -1) < 6) {
        mm_na <- rbind(mm_na, c(1, sensors[i], temp_years[j], temp_mese[k]))
      } else {
        mm_na <- rbind(mm_na, c(0, sensors[i], temp_years[j], temp_mese[k]))
      }
    }
  }
}

mm_na <- data.frame(mm_na)
colnames(mm_na) <- c("Admissible", "idSensore", "Year", "Month")

# filling Massimi replacing -1 values with a linear
# interpolation of the 2 nearest admissible maximums

findFirstDay <- function(row, df) {
  while (row <= nrow(df)) {
    if (df[row, "max"] != -1) {
      return(row)
    }
    row <- row + 1
  }
  return(row)
}

findLastDay<- function(row, df) {
  while (row >= 1) {
    if (df[row, "max"] != -1) {
      return(row)
    }
    row <- row - 1
  }
  return(row)
}

maximum_df <- NULL
for (s in sensors) {
  temp_df_id <- Massimi[which(Massimi$idSensore == s), ]
  for (y in unique(temp_df_id$Anno)) {
    temp_df <- temp_df_id[which(temp_df_id$Anno == y), ]
    first_adm <- findFirstDay(1, temp_df)
    last_adm <- findLastDay(nrow(temp_df), temp_df)
    if (first_adm != 1 & first_adm != (nrow(temp_df) + 1)) {
      temp_df[1, "max"] <- temp_df[first_adm, "max"]
    }
    if (last_adm != nrow(temp_df) & last_adm != 0) {
      temp_df[nrow(temp_df), "max"] <- temp_df[last_adm, "max"]
    }
    if (first_adm == (nrow(temp_df) + 1) | last_adm == 0) {
      temp_df[1, "max"] <- -1
      temp_df[nrow(temp_df), "max"] <- -1
      print("error occurred")
    } else {
      for (row in seq_len(nrow(temp_df))) {
        if (temp_df[row, "max"] == -1) {
          last_adm <- findLastDay(row, temp_df)
          next_adm <- findFirstDay(row, temp_df)
          temp_df[row, "max"] <- (temp_df[next_adm, "max"] - temp_df[last_adm, "max"]) /
            (next_adm - last_adm) * (row - last_adm) + temp_df[last_adm, "max"]
        }
      }
    }
    maximum_df <- rbind(maximum_df, temp_df)
  }
}

maximum_df <- data.frame(maximum_df)
colnames(maximum_df) <- c("max", "Giorno", "idSensore", "Anno", "Mese")

# Placing Nas where a month is not admissible
for (i in seq_len(nrow(mm_na))) {
  if (mm_na[i, "Admissible"] == 0) {
    maximum_df[which(maximum_df$idSensore == mm_na[i, "idSensore"] &
                maximum_df$Anno == mm_na[i, "Year"] &
                maximum_df$Mese == mm_na[i, "Month"]), "max"] <- NA
  }
}

View(maximum_df)
