# loading all the required packages

setwd("C:/Users/39339/OneDrive/Desktop/I semestre/Bayesian_Project/main")

library(lubridate)
library(rstan)
library(dplyr)
library(tidyr)
library(zoo)

# read data

ozono = read.csv("C:/Users/39339/OneDrive/Desktop/I semestre/dati_ozono/datasetO3.csv")
stazioni = read.csv("C:/Users/39339/OneDrive/Desktop/I semestre/dati_ozono/stazioni_O3.csv")

# rm idOperatore since they re = 1

ozono$idOperatore = NULL


# occhio alle stazioni "chiuse temporaneamente"

# recap:  8 chiuse prima
#         1 aperte dopo
# ne abbiamo 51 totali, forse un po' too much toglierle tutte?

# guardiamo le giornate di utilizzo imho (?) 


#### DATA PROCESSING ####


# here i change format using lubridate and create Y/M/D/H vars to help future loops

ozono$Data <- mdy_hms(ozono$Data)
ozono$Year <- year(ozono$Data-1)    
ozono$Month <- month(ozono$Data-1)
ozono$Day <- day(ozono$Data-1)    
ozono$Hour <- hour(ozono$Data-1)  

ozone.may.oct = ozono[which(ozono$Month %in% 5:10),]

N = 8

ozone.may.oct$Data  = NULL
ozone.may.oct$Stato = NULL

threshold = 120

temp = ozone.may.oct

temp <- temp %>%
  group_by(idSensore) %>%
  mutate(RollingAvg = rollmean(Valore, k = 8, fill = 0, align = "left"))

sapply(temp, function(y) sum(length(which(is.na(y)))))

temp[which(temp$Hour>17),]$RollingAvg = 0
temp$RollingAvg = na.fill(temp$RollingAvg, 0)


temp <- temp %>%
  mutate(AboveThreshold = ifelse(RollingAvg > threshold, 1, 0))

result <- temp %>%
  group_by(idSensore, Year, Month, Day) %>%
  summarize(DaysWithAvgAboveThreshold = any(AboveThreshold == 1)) %>%
  group_by(idSensore, Year, Month) %>%
  summarize(Count = sum(DaysWithAvgAboveThreshold))

df <- result %>%
  pivot_wider(names_from = idSensore, values_from = Count, values_fill = 0)

# Each color is a station

matplot(df[,-c(1,2)], type = 'l')

# df$Data <- as.POSIXct(paste(df$Year, df$Month, '01', sep = '-'), format = "%Y-%m-%d")
# 
# plot(df[,c(3,54)])















