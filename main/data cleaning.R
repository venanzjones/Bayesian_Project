# loading all the required packages

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
# giorno della settimana, mese, anno 

# intanto meglio dare un occhio alle stazioni "chiuse temporaneamente"












































































