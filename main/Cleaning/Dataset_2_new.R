library(tidyr)
library(lubridate)
library(dplyr)

ozono <- read.csv("./Dati_iniziali/datasetO3.csv")
stazioni <- read.csv("./Dati_iniziali/stazioni_O3.csv")
ozono$idOperatore <- NULL

ozono$Data <- mdy_hms(ozono$Data)
ozono$Year <- year(ozono$Data - 1)
ozono$Month <- month(ozono$Data - 1)
ozono$Day <- day(ozono$Data - 1)
ozono$Hour <- hour(ozono$Data - 1)

### STEP 1: fill the dataset with NAs where there are non registered values
# 4748*51*24 = 5811552 osservazioni in tutto dal 1-1-2010 al 31-12-2022
ozono$timestamp <- make_datetime(ozono$Year, ozono$Month, ozono$Day, ozono$Hour)

# Generate a complete set of timestamps for all sensors
all_timestamps <- expand.grid(
  idSensore = unique(ozono$idSensore),
  timestamp = seq(min(ozono$timestamp), max(ozono$timestamp), by = "1 hour")
)

# Merge the complete set of timestamps with your data
complete_data <- left_join(all_timestamps, ozono, by = c("idSensore", "timestamp"))

complete_data <- complete_data %>%
  mutate(
    Year = year(timestamp),
    Month = month(timestamp),
    Day = day(timestamp),
    Hour = hour(timestamp)
  )

complete_data$Data <- NULL

### STEP 2: creare un dataset che misura la media oraria delle ultime
# 7 ore (+1 che è quella che sto considerando), aggiungendo anche una
# colonna che indica se in quello slot di ore ci sono <= 3 NA 
library(zoo)

complete_data <- complete_data %>%
  arrange(idSensore, timestamp) %>%
  group_by(idSensore) %>%
  mutate(
    MovingAvg = rollapply(Valore, width = 8, FUN = function(x) {
      if (sum(!is.na(x)) >= 4) {
        mean(x, na.rm = TRUE)
      } else {
        NA
      }
    }, by = 1, align = "right", fill = NA),
    Admissible = rollapply(!is.na(Valore), width = 8, FUN = function(x) sum(x) >= 4, by = 1, align = "right", fill = NA)
  )

## NOTA: dovrebbe essere corretto, capire solo se ci devo mettere 4 o 5 o 3
# (sono stanca)

### STEP 3: a questo punto seleziono solo i mesi che mi interessano
# e faccio il count delle moving averages > 120



sensors <- unique(ozono$idSensore)
years <- unique((ozono$Year))
mesi <- 4:10
giorni_true <- c(30, 31, 30, 31, 31, 30, 31)
lista_sensori <- list()
for (i in 1:length(sensors))
{
  lista_sensori[[i]] <- ozono[which(ozono$idSensore == sensors[i]), ]
}

for (i in 1:length(lista_sensori))
{
  temp <- lista_sensori[[i]]
  lista_anni <- list()
  for (j in 1:length(years))
  {
    lista_anni[[j]] <- temp[which(temp$Year == years[j]), ]
  }
  
  for (j in 1:length(years))
  {
    temp_year <- lista_anni[[j]]
    lista_mesi <- list()
    for (k in 1:length(mesi))
    {
      lista_mesi[[k]] <- temp_year[which(temp_year$Month == mesi[k]), ]
    }
    
    for (k in 1:length(mesi))
    {
      temp_mese <- lista_mesi[[k]]
      giorni <- unique(lista_mesi[[k]]$Day)
      lista_giorni <- list()
      
      if (length(giorni)) {
        miss <- setdiff(1:giorni_true[k], giorni)
        # Giorni interi mancanti, vanno riempiti di Nas
        
        for (d in 1:length(giorni))
        {
          lista_giorni[[d]] <- temp_mese[which(temp_mese$Day == giorni[d]), ]
        }
        for (d in 1:length(giorni))
        {
          obs <- rep(NA, giorni_true[k])
          temp_giorno <- lista_giorni[[d]]
          #Riempire il giorno, occhio al fatto che ci possono essere ore mancanti, 
          #ogni temp_giorno deve avere dim=24
          #Metterlo nel nuovo dataset
          #new[giorni[d]] <- Vettore di observations
        } 
        #Il mese deve essere in ordine, riempirlo con l'ordine 1:giorni_true[k]
        #Alcuni saranno in miss(tutti NAs) altri saranno in giorni
        #new[miss] <- Vettore di Na
      } else {
        #Riempire il mese di NAs
      }
      #Aggiungere il mese al nuovo dataset
    }
  }
}

counts <- data.frame(counts)
colnames(counts) <- c("Count", "idSensore", "Year", "Month", "Missing days", "Days with more than 3 hours missed", "Days with more than 5 hours missed", "Days with more than 8 hours missed", "Days with more than 10 hours missed")
