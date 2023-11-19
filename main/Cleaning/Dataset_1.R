library(lubridate)
library(dplyr)
library(tidyr)

ozono <- read.csv("Dati_Iniziali/datasetO3.csv", header = TRUE)
stazioni <- read.csv("Dati_Iniziali/stazioni_O3.csv")
ozono$idOperatore <- NULL

ozono$Data <- mdy_hms(ozono$Data)
ozono$Year <- year(ozono$Data - 1)
ozono$Month <- month(ozono$Data - 1)
ozono$Day <- day(ozono$Data - 1)
ozono$Hour <- hour(ozono$Data - 1)

#### Creazione dataset con massimo giornaliero e medione in caso manchino i dati####
Massimi <- NULL
sensors <- unique(ozono$idSensore)
years <- unique((ozono$Year))
mesi <- 4:10
giorni_true <- c(30, 31, 30, 31, 31, 30, 31)
lista_sensori <- list()
missed_month <- 0
for (i in 1:length(sensors)) {
  lista_sensori[[i]] <- ozono[which(ozono$idSensore == sensors[i]), ]
}
for (i in 1:length(lista_sensori)) {
  temp <- lista_sensori[[i]]
  for (j in 1:length(years)) {
    temp_year <- temp[which(temp$Year == years[j]), ] # Un sensore, un anno
    for (k in 1:length(mesi)) {
      temp_mese <- temp_year[which(temp_year$Month == mesi[k]), ] # Singolo mese
      giorni <- unique(temp_mese$Day)
      new_month <- rep(-1, giorni_true[k])
      if (length(giorni)) {
        lista_giorni <- list()
        for (d in 1:length(giorni)) {
          lista_giorni[[giorni[d]]] <- temp_mese[which(temp_mese$Day == giorni[d]), ]
        }
        na <- NULL
        missing_hours <- NULL
        total_miss <- NULL
        for (d in 1:length(giorni)) {
          na[d] <- sum(is.na(lista_giorni[[giorni[d]]]$Valore))
          missing_hours[d] <- 24 - (dim(lista_giorni[[giorni[d]]])[1])
          total_miss[d] <- missing_hours[d] + na[d]
          if (sum(na.omit(lista_giorni[[giorni[d]]]$Valore) >= 180)) {
            total_miss[d] <- 0
          }
        }
        miss <- setdiff(1:giorni_true[k], giorni[!(total_miss > 5)])

        if (length(giorni[!(total_miss > 5)])) {
          for (a in 1:length(giorni[!(total_miss > 5)]))
          {
            new_month[giorni[!(total_miss > 5)][a]] <- max(na.omit(lista_giorni[[giorni[!(total_miss > 5)][a]]]$Valore))
            if (!sum(na.omit(lista_giorni[[giorni[!(total_miss > 5)][a]]]$Valore))) {
              print(c(i, j, k))
            }
          }

          Massimi <- rbind(Massimi, cbind(new_month, 1:giorni_true[k], rep(sensors[i], giorni_true[k]), rep(years[j], giorni_true[k]), rep(mesi[k], giorni_true[k])))
        }
      } else {
        missed_month <- missed_month + 1
      }
    }
  }
}

Massimi <- data.frame(Massimi)
names(Massimi) <- c("max", "Giorno", "idSensore", "Anno", "Mese")
rm(ozono)

#### Fill the gaps in Massimi####  -Run again from here

# Filling the vector mm_na with whether a month is admissible or not
mm_na <- NULL
for (s in unique(Massimi$idSensore)) {
  for (y in unique(Massimi$Anno[which(Massimi$idSensore == s)])) {
    for (m in unique(Massimi$Mese[which(Massimi$idSensore == s & Massimi$Anno == y)])) {
      if (sum(Massimi$max[which(Massimi$idSensore == s & Massimi$Anno == y & Massimi$Mese == m)] == -1) < 6) {
        mm_na <- rbind(mm_na, c(1, s, y, m))
      } else {
        mm_na <- rbind(mm_na, c(0, s, y, m))
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

findLastDay <- function(row, df) {
  while (row >= 1) {
    if (df[row, "max"] != -1) {
      return(row)
    }
    row <- row - 1
  }
  return(row)
}

maximum_df <- NULL
for (s in unique(Massimi$idSensore)) {
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
      print(paste("For sensor ", s, " the year ", y, " is not admissible"))
    } else {
      for (row in seq_len(nrow(temp_df))) {
        if (temp_df[row, "max"] == -1) {
          prev_adm <- findLastDay(row, temp_df)
          next_adm <- findFirstDay(row, temp_df)
          temp_df[row, "max"] <- (temp_df[next_adm, "max"] - temp_df[prev_adm, "max"]) /
            (next_adm - prev_adm) * (row - prev_adm) + temp_df[prev_adm, "max"]
        }
      }
    }
    maximum_df <- rbind(maximum_df, temp_df)
  }
}

maximum_df <- data.frame(maximum_df)

# Placing NA where a month is not admissible
for (i in seq_len(nrow(mm_na))) {
  if (mm_na[i, "Admissible"] == 0) {
    maximum_df[which(maximum_df$idSensore == mm_na[i, "idSensore"] &
      maximum_df$Anno == mm_na[i, "Year"] &
      maximum_df$Mese == mm_na[i, "Month"]), "max"] <- NA
  }
}

count_180_df <- NULL
for (s in unique(maximum_df$idSensore)) {
  temp_df_id <- maximum_df[which(maximum_df$idSensore == s), ]
  for (y in 2010:2022) {
    if (y %in% unique(temp_df_id$Anno)) {
      temp_df <- temp_df_id[which(temp_df_id$Anno == y), ]
      for (m in 4:10) {
        if (m %in% unique(temp_df$Mese)) {
          temp_df_m <- temp_df[which(temp_df$Mese == m), ]
          count_180_df <- rbind(count_180_df, c(sum(temp_df_m$max >= 180), s, y, m))
        } else {
          count_180_df <- rbind(count_180_df, c(NA, s, y, m))
        }
      }
    } else {
      for (m in 4:10) {
        count_180_df <- rbind(count_180_df, c(NA, s, y, m))
      }
    }
  }
}

count_180_df <- data.frame(count_180_df)
colnames(count_180_df) <- c("Count_180", "idSensore", "Year", "Month")

write.csv(count_180_df, "./Datasets/Dataset_180.csv", row.names = FALSE)

#### Plot the Nas of the full final dataset####
count_180_df <- read.csv("./Datasets/Dataset_180.csv")

sensors <- unique(count_120_df$idSensore)
years <- 2010:2022
mesi <- 4:10

sum(is.na(count_180_df$Count_180))
sum(is.na(count_180_df$Count_180)) / nrow(count_180_df)

sen <- seq_along(sensors)
time <- seq_len(length(years) * length(mesi))
nas <- matrix(rep(0, length(time) * length(sensors)), nrow = length(sensors), ncol = length(time))

nas <- NULL
for (i in sensors) {
  nas <- rbind(nas, as.numeric(is.na(count_180_df$Count_180[count_180_df$idSensore == i])))
}

sum(nas[nrow(nas), ] == 1) / dim(nas)[2]
thre <- rep(0, length(sensors))
for (i in sen) {
  thre[i] <- sum(nas[i, ] == 1) / dim(nas)[2]
}
plot(thre)
abline(h = 0.1)
# Togliere questi è troppo, togliere gli ultimi però sembra necessario. Questa è la mia proposta

Dataset_180 <- count_180_df[-which(count_180_df$idSensore %in% sensors[46:51]), ]

sensors <- unique(Dataset_180$idSensore)
mat_plot <- matrix(rep(0, length(time) * length(sensors)), nrow = length(sensors), ncol = length(time))
for (i in sensors) {
  mat_plot <- rbind(mat_plot, Dataset_180$Count_180[which(Dataset_180$idSensore == i)])
}

matplot(t(mat_plot), type = "l")
k <- 7
n <- 13
vertical_lines_x <- seq(k, n * k, by = k)
abline(v = vertical_lines_x, col = "black")

media <- rep(0, length(sensors))
varianza <- rep(0, length(sensors))
for (i in sensors) {
  media <- c(media, mean(na.omit(Dataset_180$Count_180[which(Dataset_180$idSensore == i)])))
  varianza <- c(varianza, sd(na.omit(Dataset_180$Count_180[which(Dataset_180$idSensore == i)])))
}

xx <- seq(0, 4, by = 0.1)
plot(media, varianza, xlim = c(0, 4), ylim = c(0, 4))
lines(xx, xx)
