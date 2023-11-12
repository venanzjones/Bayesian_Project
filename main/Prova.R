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
