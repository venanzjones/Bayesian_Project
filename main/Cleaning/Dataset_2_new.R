library(tidyr)
library(lubridate)
library(dplyr)
library(tidyverse)

ozono <- read.csv("./Dati_iniziali/datasetO3.csv")
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
ozono.completo <- left_join(all_timestamps, ozono, by = c("idSensore", "timestamp"))

ozono.completo <- ozono.completo %>%
  mutate(
    Year = year(timestamp),
    Month = month(timestamp),
    Day = day(timestamp),
    Hour = hour(timestamp)
  )

ozono.completo$Data <- NULL
rm(ozono)
rm(all_timestamps)
gc()

### STEP 2: creare un dataset che misura la media oraria delle ultime
# 7 ore (+1 che è quella che sto considerando)
# threshold: MovingAvg è NA se nei 8 valori che considero ci sono > 4 NA
library(zoo)

ozono.completo <- ozono.completo %>%
  arrange(idSensore, timestamp) %>%
  group_by(idSensore) %>%
  mutate(
    MovingAvg = rollapply(Valore, width = 8, FUN = function(x) {
      if (sum(!is.na(x)) >= 4) { # non NA >= 4 faccio la media
        mean(x, na.rm = TRUE)
      } else {
        NA
      }
    }, by = 1, align = "right", fill = NA),
    Admissible = rollapply(!is.na(Valore), width = 8, FUN = function(x) sum(x) >= 4, by = 1, align = "right", fill = NA)
  )

### STEP 3: a questo punto seleziono solo i mesi che mi interessano (4:10)
# e faccio il count delle moving averages > 120

# (30+31+30+31+31+30+31)*13*24*51 = 3405168 obs in filtered_data
ozono.filtered <- ozono.completo %>%
  filter(Month >= 4 & Month <= 10)

# ora stesso lavoro fatto per count_180
# massimi deve avere (30+31+30+31+31+30+31)*13*51 = 141882 obs
# massimi è costruito con lo stesso criterio di quello per 180
# prendo il massimo delle MovingAvg nella giornata se quella giornata
# ha > 16 MovingAvg non NA oppure, se ha >= 8 NA, se almeno una di 
# quelle registrate supera 120

massimi <- ozono.filtered %>%
  group_by(idSensore, Year, Month, Day) %>%
  summarize(
    max = ifelse(
      sum(!is.na(MovingAvg)) > 16,
      max(MovingAvg, na.rm = TRUE),
      ifelse(
        sum(is.na(MovingAvg)) >= 8 & any(MovingAvg[!is.na(MovingAvg)] >= 120),
        max(MovingAvg, na.rm = TRUE),
        -1
      )
    )
  ) %>%
  ungroup()

counts <- NULL
sensors <- unique(massimi$idSensore)
years <- unique(massimi$Year)
mesi <- 4:10
mm_na <- NULL

# Filling the vector mm_na with whether a month is admissible or not
# a month is admissible iff it has >= 6 non NA (ie -1) for max variable
for (i in seq_along(sensors)) {
  temp_years <- unique(massimi$Year[which(massimi$idSensore == sensors[i])])
  for (j in seq_along(temp_years)) {
    temp_mese <- unique(massimi$Month[which(massimi$idSensore == sensors[i] & massimi$Year == temp_years[j])])
    for (k in seq_along(temp_mese)) {
      temp <- massimi[which(massimi$idSensore == sensors[i] & massimi$Year == temp_years[j] & massimi$Month == temp_mese[k]),]
      if (sum(temp$max == -1) < 6) {
        mm_na <- rbind(mm_na, c(1, sensors[i], temp_years[j], temp_mese[k]))
      } else {
        mm_na <- rbind(mm_na, c(0, sensors[i], temp_years[j], temp_mese[k]))
      }
    }
  }
}

# mm_na deve avere 51*7*13 = 4641 obs
mm_na <- data.frame(mm_na)
colnames(mm_na) <- c("Admissible", "idSensore", "Year", "Month")

# filling massimi replacing -1 values with a linear
# interpolation of the 2 nearest admissible maximums
# usiamo lo stesso criterio di riempimento usato per count_180

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
  temp_df_id <- massimi[which(massimi$idSensore == s), ]
  for (y in unique(temp_df_id$Year)) {
    temp_df <- temp_df_id[which(temp_df_id$Year == y), ]
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
# colnames(maximum_df) <- c("max", "Day", "idSensore", "Year", "Month")

# Placing Nas where a month is not admissible
for (i in seq_len(nrow(mm_na))) {
  if (mm_na[i, "Admissible"] == 0) {
    maximum_df[which(maximum_df$idSensore == mm_na[i, "idSensore"] &
                       maximum_df$Year == mm_na[i, "Year"] &
                       maximum_df$Month == mm_na[i, "Month"]), "max"] <- NA
  }
}


count_120_df <- NULL
for (s in sensors) {
  temp_df_id <- maximum_df[which(maximum_df$idSensore == s), ]
  for (y in 2010:2022) {
    if (y %in% unique(temp_df_id$Year)) {
      temp_df <- temp_df_id[which(temp_df_id$Year == y), ]
      for (m in 4:10) {
        if (m %in% unique(temp_df$Month)) {
          temp_df_m <- temp_df[which(temp_df$Month == m), ]
          count_120_df <- rbind(count_120_df, c(sum(temp_df_m$max > 120), s, y, m))
        } else {
          count_120_df <- rbind(count_120_df, c(NA, s, y, m))
        }
      }
    } else {
      for (m in 4:10) {
        count_120_df <- rbind(count_120_df, c(NA, s, y, m))
      }
    }
  }
}

count_120_df <- data.frame(count_120_df)
colnames(count_120_df) <- c("Count_120", "idSensore", "Year", "Month")

write.csv(count_120_df, "./Datasets/Dataset_120", row.names = FALSE)

##Na analysis
sum(is.na(count_120_df$Count_120))
sum(is.na(count_120_df$Count_120))/nrow(count_120_df)

sen <- 1:length(sensors)
time <- 1:(length(years)*length(mesi))
nas <- matrix(rep(0, length(time)*length(sensors)), nrow = length(sensors), ncol = length(time))

nas <- NULL
for (i in sensors)
{
  nas <- rbind(nas, as.numeric(is.na(count_120_df$Count_120[count_120_df$idSensore==i])))
}
image(nas)

sum(nas[nrow(nas) ,]==1)/dim(nas)[2]
thre <- rep(0, length(sensors))
for (i in 1:length(sensors))
{
  thre[i] <- sum(nas[i ,]==1)/dim(nas)[2]
}
plot(thre)
abline(h=0.1)
#Togliere questi è troppo, togliere gli ultimi però sembra necessario. Questa è la mia proposta

Dataset_120 <- count_120_df[-which(count_120_df$idSensore %in% sensors[46:51]) ,]

sensors <- unique(Dataset_120$idSensore)
mat_plot <- matrix(rep(0, length(time)*length(sensors)), nrow = length(sensors), ncol = length(time))
for (i in sensors)
{
  mat_plot <- rbind(mat_plot, Dataset_120$Count_120[which(Dataset_120$idSensore==i)])
}

matplot(t(mat_plot), type='l')
k <- 7
n <- 13 
vertical_lines_x <- seq(k, n*k, by=k)
abline(v = vertical_lines_x, col = "black")

media <- rep(0, length(sensors))
varianza <- rep(0, length(sensors))
for (i in sensors)
{
  media <- c(media, mean(na.omit(Dataset_120$Count_120[which(Dataset_120$idSensore==i)])))
  varianza <- c(varianza, sd(na.omit(Dataset_120$Count_120[which(Dataset_120$idSensore==i)])))
}

xx <- seq(0, 15, by=0.1)
plot(media, varianza)
lines(xx, xx)
