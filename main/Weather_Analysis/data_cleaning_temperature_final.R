library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)

ozono <- read.csv("./Dati_iniziali/datasetO3.csv")
stazioni <- read.csv("./Dati_iniziali/stazioni_O3.csv")
stazioni.usate <- stazioni[which(stazioni$IdSensore %in% unique(ozono$idSensore)), ]
rm(ozono)
Stazioni_chiuse = c(1374,558,583,670,690,669)
Id_chiusi = stazioni.usate$IdSensore[stazioni.usate$Idstazione %in% Stazioni_chiuse]

# Then I will loop i in 1...51 and exploit paste to collect each df
monthly_means_list <- list()

map_beaufort <- function(x) {
  nodi <- x / 1.852
  
  if (nodi == 0) {
    return(0)
  } else if (nodi <= 3) {
    return(1)
  } else if (nodi <= 6) {
    return(2)
  } else if (nodi <= 10) {
    return(3)
  } else if (nodi <= 16) {
    return(4)
  } else if (nodi <= 21) {
    return(5)
  } else if (nodi <= 27) {
    return(6)
  } else if (nodi <= 33) {
    return(7)
  } else {
    return("Wind speed is too high for Beaufort scale")
  }
}


for (i in 1:51) {
  staz <- read.csv(paste0("./Weather_Analysis/weather/staz", i, ".csv"), skip = 2, header = T)

  staz$time <- ymd(staz$time)
  staz$Year <- year(staz$time)
  staz$Month <- month(staz$time)
  staz$Day <- day(staz$time)
  staz$beaufort = sapply(staz$wind_speed_10m_max..km.h, map_beaufort)
  staz <- staz[which(staz$Month %in% 4:10), ]

  # Create a new data frame with monthly means
  monthly.means <- staz %>%
    group_by(Year, Month) %>%
    summarize(
      mean_temperature = mean(temperature_2m_mean...C.),
      mean_precipitation_sum = mean(precipitation_sum..mm.),
      mean_precipitation_hours = mean(precipitation_hours..h.),
      mean_windspeed_10m_max = mean(wind_speed_10m_max..km.h.),
      mean_radiation_sum = mean(shortwave_radiation_sum..MJ.m..),
      # count_highwind = sum(wind_speed_10m_max..km.h. > threshold)
      count_highwind = mean(beaufort)
    ) %>%
    ungroup()
  monthly_means_list[[i]] <- monthly.means
  
  # 
  staz <- staz %>%
    group_by(Year, Month) %>%
    mutate(
      consecutive_highwind_days = sequence(rle(beaufort > 3)$lengths) * (beaufort > 3)
    ) %>%
    ungroup()
  
  # Calculate the maximum number of consecutive days with Beaufort > 4 for each month
  monthly_max_consecutive_days <- staz %>%
    group_by(Year, Month) %>%
    summarize(max_consecutive_highwind_days = max(consecutive_highwind_days))
  
  # Merge the result with your existing monthly_means dataframe
  monthly_means_list[[i]] <- left_join(monthly_means_list[[i]], monthly_max_consecutive_days, by = c("Year", "Month"))
  
}

combined_df <- data.frame()

for (i in 1:51) {
  monthly_means_list[[i]]$Station <- i
  combined_df <- bind_rows(combined_df, monthly_means_list[[i]])
}

sapply(combined_df, function(y) sum(length(which(is.na(y)))))

# there are no NAs

for (i in 1:51) {
  combined_df[which(combined_df$Station == i), 10] <- stazioni.usate$IdSensore[i]
}


### Aggiungiamo la densità
density = read.csv("./Weather_Analysis/density.csv")
names(density) <- gsub("^X", "", names(density))

# Estrai l'anno dal dataframe "combined_df"
combined_df <- combined_df %>%
  mutate(Year = as.numeric(Year))

# Usa melt per trasformare le colonne di anno in righe in "density"
density_long <- melt(density, id.vars = c("IdSensore", "Comune"), variable.name = "Year", value.name = "Densità")

# Rimuovi il prefisso "X" dalla colonna "Year" e converti in numerico in density_long
density_long$Year <- as.numeric(gsub("X", "", as.character(density_long$Year)))

# Unisci i due dataframe
combined_df <- left_join(combined_df, density_long, by = c("Station" = "IdSensore", "Year"))

# Rimuovi la colonna 'Comune' da combined_df
combined_df <- combined_df %>%
  select(-Comune)


### Aggiungiamo la quota
stazioni_ausiliario <- stazioni.usate %>% select(IdSensore, Quota)

# Unire le colonne "quota" da stazioni_ausiliario a combined_df in base all'IdSensore
combined_df <- left_join(combined_df, stazioni_ausiliario, by = c("Station" = "IdSensore"))


### Elimino le stazioni chiuse
combined_df <- combined_df %>%
  filter(!Station %in% Id_chiusi)

### Aggiungo Type

type <- density[,c(1,16)]

type <- type %>%
  filter(!IdSensore %in% Id_chiusi)

combined_df <- left_join(combined_df, type, by = c("Station" = "IdSensore"))



write.csv(combined_df, "./Dati_iniziali/X.csv", row.names = FALSE)
