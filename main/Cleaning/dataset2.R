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

## prime 10
# 45.62736  ,9.026401
# 45.69795  ,9.406191
# 45.30950  ,9.585790
# 45.86375  ,9.399950
# 45.46242  ,8.880210
# 45.14254 ,10.043836
# 45.69043  ,9.484261
# 44.99955  ,9.008437
# 46.13228  ,9.566180
# 45.55104  ,9.162614

# bea 11-20
# 45.69104  ,9.643651
# 45.15047 , 9.930596
# 45.04008  ,8.914145
# 45.47900  ,9.235491
# 45.30278  ,9.495274
# 45.54852  ,8.847322
# 45.81504  ,9.066971
# 45.23349  ,9.666250
# 45.58026  ,9.273573
# 45.51304 ,10.191942

# bea 21-35
# 45.24954 ,10.299119
# 45.83690  ,8.803926
# 45.36631  ,9.703946
# 45.06966  ,8.868889
# 45.01688 ,11.076095
# 45.19468  ,9.164638
# 45.64963 ,10.205090
# 46.13814  ,9.384687
# 45.87460 ,10.177357
# 45.58289  ,8.842165
# 45.11350  ,8.874065
# 45.62056  ,9.611738
# 45.84221  ,9.351658
# 45.46375 ,10.480772
# 45.48363  ,9.327360


# ultime 15
# 44.92469 ,10.517502
# 45.80857  ,9.221779
# 45.66176  ,9.156644
# 45.16057 ,10.795564
# 45.49823  ,9.556232
# 46.46952 ,10.375433
# 45.41277 ,10.683357
# 45.61924  ,8.756970
# 45.46335  ,9.195325
# 46.01583  ,9.286409
# 45.73084  ,9.125734
# 45.91279  ,9.497538
# 45.60123  ,9.275073
# 45.28196 , 8.988563
# 46.16785,  9.879210
# 45.27849, 10.006202










