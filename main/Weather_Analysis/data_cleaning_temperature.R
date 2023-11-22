library(dplyr)
library(lubridate)
library(tidyr)
ozono <- read.csv("./Dati_iniziali/datasetO3.csv")
stazioni <- read.csv("./Dati_iniziali/stazioni_O3.csv")
stazioni.usate <- stazioni[which(stazioni$IdSensore %in% unique(ozono$idSensore)), ]
rm(ozono)

# then I will loop i in 1...51 and exploit paste to collect each df
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
  # threshold <- quantile(staz$wind_speed_10m_max..km.h., 0.75)
  # threshold = 18

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
}

combined_df <- data.frame()

# change to 51 later
for (i in 1:51) {
  monthly_means_list[[i]]$Station <- i
  combined_df <- bind_rows(combined_df, monthly_means_list[[i]])
}

sapply(combined_df, function(y) sum(length(which(is.na(y)))))

# there are no NAs

for (i in 1:51) {
  combined_df[which(combined_df$Station == i), 9] <- stazioni.usate$IdSensore[i]
}


write.csv(combined_df, "./Weather_Analysis/weather_data.csv", row.names = FALSE)
