library(dplyr)
library(lubridate)
library(tidyr)
library(reshape2)

combined_df <- read.csv("./Datasets/covariates.csv")
count_180 <- read.csv("./Datasets/Dataset_180.csv")
count_120 <- read.csv("./Datasets/Dataset_120.csv")

count_180 <- count_180[-which(count_180$idSensore %in% unique(count_180$idSensore)[46:51]), ]

colnames(count_180) <- c("Count_180", "Station", "Year", "Month")
colnames(count_120) <- c("Count_120", "Station", "Year", "Month")

# merging the dataframes by station year and month
combined_df <- left_join(combined_df, count_120, by = c("Station", "Year", "Month"))
combined_df <- left_join(combined_df, count_180, by = c("Station", "Year", "Month"))

write.csv(combined_df, "./Datasets/combined_df.csv", row.names = FALSE)

# scale all the numerical data in the dataset
combined_df$mean_temperature <- scale(combined_df$mean_temperature, scale = TRUE)
combined_df$mean_precipitation_sum <- scale(combined_df$mean_precipitation_sum, scale = TRUE)
combined_df$mean_precipitation_hours <- scale(combined_df$mean_precipitation_hours, scale = TRUE)
combined_df$mean_windspeed_10m_max <- scale(combined_df$mean_windspeed_10m_max, scale = TRUE)
combined_df$mean_radiation_sum <- scale(combined_df$mean_radiation_sum, scale = TRUE)
combined_df$count_highwind <- scale(combined_df$count_highwind, scale = TRUE)
combined_df$max_consecutive_highwind_days <- scale(combined_df$max_consecutive_highwind_days, scale = TRUE)

write.csv(combined_df, "./Datasets/combined_df_scaled.csv", row.names = FALSE)
