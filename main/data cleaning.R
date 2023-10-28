# loading all the required packages

setwd("C:/Users/39339/OneDrive/Desktop/I semestre/Bayesian_Project/main")

library(lubridate)
library(rstan)
library(dplyr)

# read data

ozono = read.csv("C:/Users/39339/OneDrive/Desktop/I semestre/dati_ozono/datasetO3.csv")
stazioni = read.csv("C:/Users/39339/OneDrive/Desktop/I semestre/dati_ozono/stazioni_O3.csv")

# rm idOperatore since they re = 1

ozono$idOperatore = NULL

# check NAs

na_count <-sapply(ozono, function(y) sum(length(which(is.na(y)))))
na_count

100 * na_count[3] / length(ozono[,3])

# 2.376734 % di NAs 
# da vedere meglio comunque

# converting dates to dmy hms format (-> to deal with am/pm) 

# ozono$Data = lubridate::dmy_hms(ozono$Data)

# per qualche ragione strana crea NAs, da vedere il perchè

# serve preprocessing delle date, magari creando nuove features: ora, giorno, day, 
# giorno della settimana, mese, anno => uso dplyr + lurbidate per parsare al meglio il formato 
# e passare da AM/PM a UTC

# in teoria cosi dovrebbe andare, idk why crea tanti NAs

temp = data.frame(ozono)

temp$Data <- dmy_hms(temp$Data) 

temp$Year <- year(temp$Data)    
temp$Month <- month(temp$Data)
temp$Day <- day(temp$Data)    
temp$Hour <- hour(temp$Data)  

# intanto meglio dare un occhio alle stazioni "chiuse temporaneamente"

stazioni.usate <- stazioni[which(stazioni$IdSensore %in% unique(ozono$idSensore)),]
start.stop <- data.frame(stazioni.usate$DataStart, stazioni.usate$DataStop)

started.late <- start.stop[which(as.Date(start.stop$stazioni.usate.DataStart, format = "%d/%m/%Y") > 
                   as.Date("01/01/2010", format = "%d/%m/%Y")),] 

closed.early <- start.stop[which(as.Date(start.stop$stazioni.usate.DataStop, format = "%d/%m/%Y") > 
                                                   as.Date("01/01/2010", format = "%d/%m/%Y")),] 


# recap:  8 chiuse prima
#         1 aperte dopo
# ne abbiamo 51 totali, forse un po' too much toglierle tutte?

# guardiamo le giornate di utilizzo imho 





















































