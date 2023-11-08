
setwd("C:/Users/39339/OneDrive/Desktop/I semestre/Bayesian_Project/main")

library(dplyr)
library(lubridate)


# then I will loop i im 1...51 and expolit paste to collect each df
monthly_means_list <- list()

for (i in 1:51) { # change to 51 later
  

staz = read.csv(paste0("weather/staz", i, ".csv"), skip = 2, header = T)
staz$time <- ymd(staz$time)
staz$Year <- year(staz$time)    
staz$Month <- month(staz$time)
staz$Day <- day(staz$time)  

staz <- staz[which(staz$Month %in% 5:10),]
# Create a new data frame with monthly means

threshold = quantile(staz$windspeed_10m_max..km.h., .75)

monthly.means <- staz %>%
  group_by(Year, Month) %>%
  summarize(
    mean_temperature = mean(temperature_2m_mean...C.),
    mean_precipitation_sum = mean(precipitation_sum..mm.),
    mean_precipitation_hours = mean(precipitation_hours..h.),
    wind_count = sum(length(which(windspeed_10m_max..km.h. > threshold)))
  ) %>%
  ungroup()
print(threshold)
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
  combined_df[which(combined_df$Station == i),7] <- stazioni.usate$IdSensore[i]
}

write.csv(combined_df, "weather_data.csv", row.names=FALSE)


