library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)

#Load stazioni chiuse
ozono <- read.csv("./Dati_iniziali/datasetO3.csv")
stazioni <- read.csv("./Dati_iniziali/stazioni_O3.csv")
stazioni.usate <- stazioni[which(stazioni$IdSensore %in% unique(ozono$idSensore)), ]
rm(ozono)
Stazioni_chiuse = c(1374,558,583,670,690,669)
Id_chiusi = stazioni.usate$IdSensore[stazioni.usate$Idstazione %in% Stazioni_chiuse]

### Aggiungiamo la densità
density = read.csv("./Dataset_construction/density.csv")
names(density) <- gsub("^X", "", names(density))

# Estrai l'anno dal dataframe "combined_df"
combined_df <- read.csv("./Datasets/wheather_covariates.csv")
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

combined_df <- combined_df[order(combined_df$Station),]
combined_df <- combined_df[, -1]

write.csv(combined_df, "./Datasets/covariates.csv", row.names = FALSE)
