# loading all the required packages


library(lubridate)
library(rstan)
library(dplyr)
library(tidyr)

# read data

ozono = read.csv("./Dati_iniziali/datasetO3.csv")
stazioni = read.csv("./Dati_iniziali/stazioni_O3.csv")

# rm idOperatore since they re = 1

ozono$idOperatore = NULL


# occhio alle stazioni "chiuse temporaneamente"

stazioni.usate <- stazioni[which(stazioni$IdSensore %in% unique(ozono$idSensore)),]
start.stop <- data.frame(stazioni.usate$DataStart, stazioni.usate$DataStop)

started.late <- start.stop[which(as.Date(start.stop$stazioni.usate.DataStart, format = "%d/%m/%Y") > 
                                   as.Date("01/01/2010", format = "%d/%m/%Y")),] 

closed.early <- start.stop[which(as.Date(start.stop$stazioni.usate.DataStop, format = "%d/%m/%Y") > 
                                   as.Date("01/01/2010", format = "%d/%m/%Y")),] 

# guardiamo le giornate di utilizzo imho (?) 

#### DATA PROCESSING ####

# here i change format using lubridate and create Y/M/D/H vars to help future loops

ozono$Data <- mdy_hms(ozono$Data)
ozono$Year <- year(ozono$Data)    
ozono$Month <- month(ozono$Data)
ozono$Day <- day(ozono$Data)    
ozono$Hour <- hour(ozono$Data)  

sapply(ozono, function(y) sum(length(which(is.na(y)))))

may.oct.15 = ozono[which(ozono$Month %in% 5:10 & ozono$Year %in% 2015),]

new_dataset <- may.oct.15 %>%
  pivot_wider(names_from = idSensore, values_from = Valore, values_fill = 0)

new_dataset[,c(2,3,4,5,6)] = NULL

data_long <- new_dataset %>%
  gather(station, value, -Data)

# Create a line plot with different colors for each station
library(ggplot2)

ggplot(data_long, aes(x = Data, y = value, color = station)) +
  geom_line() +
  labs(title = "Time Series for Each Station",
       x = "Date",
       y = "Value") +
  theme_minimal()


















































