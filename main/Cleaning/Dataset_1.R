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
ozono_completed <- left_join(all_timestamps, ozono, by = c("idSensore", "timestamp"))

ozono_completed <- ozono_completed %>%
  mutate(
    Year = year(timestamp),
    Month = month(timestamp),
    Day = day(timestamp),
    Hour = hour(timestamp)
  )

ozono_completed$Data <- NULL
rm(ozono)
rm(all_timestamps)
gc()

### STEP 2: a questo punto seleziono solo i mesi che mi interessano (4:10)
# e faccio il count delle moving averages > 180

# (30+31+30+31+31+30+31)*13*24*51 = 3405168 obs in filtered_data
ozono_filtered <- ozono_completed %>%
  filter(Month >= 4 & Month <= 10)

# ora stesso lavoro fatto per count_180
# massimi deve avere (30+31+30+31+31+30+31)*13*51 = 141882 obs
# massimi è costruito con lo stesso criterio di quello per 180
# prendo il massimo delle Valore nella giornata se quella giornata
# ha > 16 Valore non NA oppure, se ha >= 8 NA, se almeno una di
# quelle registrate supera 180

massimi <- ozono_filtered %>%
  group_by(idSensore, Year, Month, Day) %>%
  summarize(
    max = ifelse(
      sum(is.na(Valore)) < 6,
      max(Valore, na.rm = TRUE),
      ifelse(
        any(Valore[!is.na(Valore)] >= 180),
        max(Valore, na.rm = TRUE),
        -1
      )
    )
  ) %>%
  ungroup()

# Filling the vector mm_na with whether a month is admissible or not
# a month is admissible iff it has >= 6 non NA (ie -1) for max variable
mm_na <- NULL
for (s in unique(massimi$idSensore)) {
  for (y in unique(massimi$Year[which(massimi$idSensore == s)])) {
    for (m in unique(massimi$Month[which(massimi$idSensore == s & massimi$Year == y)])) {
      if (sum(massimi$max[which(massimi$idSensore == s & massimi$Year == y & massimi$Month == m)] == -1) < 6) {
        mm_na <- rbind(mm_na, c(1, s, y, m))
      } else {
        mm_na <- rbind(mm_na, c(0, s, y, m))
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
for (s in unique(massimi$idSensore)) {
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

# Placing Nas where a month is not admissible
for (i in seq_len(nrow(mm_na))) {
  if (mm_na[i, "Admissible"] == 0) {
    maximum_df[which(maximum_df$idSensore == mm_na[i, "idSensore"] &
      maximum_df$Year == mm_na[i, "Year"] &
      maximum_df$Month == mm_na[i, "Month"]), "max"] <- NA
  }
}

count_180_df <- NULL
for (s in unique(maximum_df$idSensore)) {
  temp_df_id <- maximum_df[which(maximum_df$idSensore == s), ]
  for (y in 2010:2022) {
    if (y %in% unique(temp_df_id$Year)) {
      temp_df <- temp_df_id[which(temp_df_id$Year == y), ]
      for (m in 4:10) {
        if (m %in% unique(temp_df$Month)) {
          temp_df_m <- temp_df[which(temp_df$Month == m), ]
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

sensors <- unique(count_180_df$idSensore)
years <- 2010:2022
mesi <- 4:10

sum(is.na(count_180_df$Count_180))
sum(is.na(count_180_df$Count_180)) / nrow(count_180_df)

sen <- 1:length(sensors)
time <- 1:(length(years) * length(mesi))

nas <- NULL
for (i in sensors) {
  nas <- rbind(nas, as.numeric(is.na(count_180_df$Count_180[count_180_df$idSensore == i])))
}

sum(nas[nrow(nas), ] == 1) / dim(nas)[2]
thre <- rep(0, length(sensors))
for (i in 1:length(sensors)) {
  thre[i] <- sum(nas[i, ] == 1) / dim(nas)[2]
}
plot(thre)
abline(h = 0.1)
# Togliere questi è troppo, togliere gli ultimi però sembra necessario. Questa è la mia proposta

Dataset_180 <- count_180_df[-which(count_180_df$idSensore %in% sensors[46:51]), ]
sensors <- unique(Dataset_180$idSensore)

# Plot of the responses over 2010-2022

y120$Count_180 = NULL

wrapped180 = y180 %>%
  pivot_wider(names_from = IdSensore, values_from = Count_180, values_fill = 0)

matplot(wrapped180[-c(1,2)], type = 'l', ylab = "Day count", ylim = c(0,30),
        xaxt = "n",main = "Days with max > 180")

ticks <- seq(4.5,88.5,by = 7)
labels <- c("2010","2011","2012","2013","2014","2015","2016","2017",
            "2018","2019","2020","2021","2022")
axis(1, at = c(ticks), labels = labels)
abline(v = seq(8,86,7), lty = 4)

media <- NULL
varianza <- NULL
for (i in sensors) {
  media <- c(media, mean(na.omit(Dataset_180$Count_180[which(Dataset_180$idSensore == i)])))
  varianza <- c(varianza, sd(na.omit(Dataset_180$Count_180[which(Dataset_180$idSensore == i)])))
}

xx <- seq(0, 4, by = 0.1)
plot(media, varianza, xlim = c(0, 4), ylim = c(0, 4))
lines(xx, xx)

zero <- NULL
for (i in sensors) {
  zero <- rbind(zero, as.numeric((count_180_df$Count_180[count_180_df$idSensore == i]))==0)
}
image(zero)
sum(na.omit(Dataset_180$Count_180) == 0)
sum(na.omit(Dataset_180$Count_180) == 0) / nrow(Dataset_180)
