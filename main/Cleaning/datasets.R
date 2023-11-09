library(lubridate)
library(rstan)
library(dplyr)
library(tidyr)

rm(list = ls())
ozono <- read.csv("./Dati_iniziali/datasetO3.csv")
stazioni <- read.csv("./Dati_iniziali/stazioni_O3.csv")
ozono$Data <- mdy_hms(ozono$Data)
ozono$Year <- year(ozono$Data - 1)
ozono$Month <- month(ozono$Data - 1)
ozono$Day <- day(ozono$Data - 1)
ozono$Hour <- hour(ozono$Data - 1)

sensors <- unique(ozono$idSensore)
years <- unique((ozono$Year))

# We consider only months 4 through 10
# since they have the most relevant ammont of observations accross all stations
mesi <- 4:10
giorni_true <- c(30, 31, 30, 31, 31, 30, 31)


lista_sensori <- list()
for (i in seq_along(sensors))
{
    lista_sensori[[i]] <- ozono[which(ozono$idSensore == sensors[i]), ]
}

## Creating a dataset with:
# - Count:
# - idSensore:
# - Year:
# - Month:

counts_1 <- NULL
counts_2 <- NULL
N <- 8
set.seed(1)
for (sens in lista_sensori) {
    lista_anni <- list()
    for (j in seq_along(years)) {
        lista_anni[[j]] <- sens[which(sens$Year == years[j]), ]
    }

    for (y in lista_anni) {
        lista_mesi <- list()
        for (k in seq_along(mesi)) {
            lista_mesi[[k]] <- y[which(y$Month == mesi[k]), ]
        }

        for (m in seq_along(mesi)) {
            temp_mese <- lista_mesi[[k]]
            giorni <- unique(lista_mesi[[k]]$Day)
            lista_giorni <- list()

            if (length(giorni)) {
                miss <- setdiff(1:giorni_true[k], giorni)   # Giorni interi mancanti

                for (d in seq_along(giorni)) {
                    lista_giorni[[d]] <- temp_mese[which(temp_mese$Day == giorni[d]), ]
                }

                count_1 <- NULL
                count_2 <- NULL
                na <- NULL
                missing_hours <- NULL
                total_miss <- NULL
                added <- NULL
                for (d in seq_along(giorni)) {
                    na[d] <- sum(is.na(lista_giorni[[d]]$Valore))
                    missing_hours[d] <- 24 - (dim(lista_giorni[[d]])[1])
                    count_1[d] <- (sum(na.omit(lista_giorni[[d]]$Valore) > 180) > 0)
                    total_miss[d] <- missing_hours[d] + na[d]

                    means <- NULL
                    count_120 <- NULL
                    for (h in 0:23 - N) {
                        sub_list <- lista_giorni[[d]][which(lista_giorni[[d]]$Hour >= h & lista_giorni[[d]]$Hour <= h + N), ]
                        count_120[h + 1] <- sum(na.omit(sub_list$Valore > 120))
                        if (c(count_120[h + 1], 0)[1] >= 4 || total_miss[d] < 8) {
                            means[h + 1] <- mean(sub_list$Valore, na.rm = TRUE)
                        }
                    }

                    count_2[d] <- ((sum(na.omit(means) > 120)) > 0)
                }

                if (length(miss) + sum((total_miss > 8) & (count_1 == 0)) > 5) {
                    counts_1 <- rbind(
                        counts_1,
                        c(NA, sens$idSensore[1], y$Year[1], mesi[k])
                    )
                } else {
                    added <- rbinom(
                        n = 1, size = length(miss) + sum((total_miss > 8) & (count_1 == 0)),
                        prob = sum(count_1) / (giorni_true[k] - (length(miss) + sum((total_miss > 8) & (count_1 == 0))))
                    )
                    counts_1 <- rbind(
                        counts_1,
                        c(sum(count_1) + added, sens$idSensore[1], y$Year[1], mesi[k])
                    )
                }
                if (length(miss) + sum((total_miss > 8) & (count_2 == 0)) > 5) {
                    counts_2 <- rbind(
                        counts_2,
                        c(NA, sens$idSensore[1], y$Year[1], mesi[k])
                    )
                } else {
                    added <- rbinom(
                        n = 1, size = length(miss) + sum((total_miss > 8) & (count_2 == 0)),
                        prob = sum(count_2) / (giorni_true[k] - (length(miss) + sum((total_miss > 8) & (count_2 == 0))))
                    )
                    counts_2 <- rbind(
                        counts_2,
                        c(sum(count_2) + added, sens$idSensore[1], y$Year[1], mesi[k])
                    )
                }
            } else {
                counts_1 <- rbind(
                    counts_1,
                    c(NA, sens$idSensore[1], y$Year[1], mesi[k])
                )
                counts_2 <- rbind(
                    counts_2,
                    c(NA, sens$idSensore[1], y$Year[1], mesi[k])
                )
            }
        }
    }
}

counts_1 <- data.frame(counts_1)
counts_3 <- cbind(counts_1$Count, counts_2)
counts_3 <- data.frame(counts_3)
counts_2 <- data.frame(counts_2)
colnames(counts_1) <- c("Count", "idSensore", "Year", "Month")
colnames(counts_2) <- c("Count", "idSensore", "Year", "Month")
colnames(counts_3) <- c("Count_180", "Count_mean120", "idSensore", "Year", "Month")

sum(is.na(counts_1$Count)) / length(counts_1$Count)
sum(is.na(counts_2$Count)) / length(counts_2$Count)

write.csv(counts_1, "./Datasets/Dataset_1")
write.csv(counts_2, "./Datasets/Dataset_2")
write.csv(counts_3, "./Datasets/Dataset_full")

# Manca da creare un dataset per fare una sorta di matplot, righe sensori colonne time

## Plots

rm(list = ls())
ozono <- read.csv("./Dati_iniziali/datasetO3.csv")
stazioni <- read.csv("./Dati_iniziali/stazioni_O3.csv")
ozono$Data <- mdy_hms(ozono$Data)
ozono$Year <- year(ozono$Data - 1)
ozono$Month <- month(ozono$Data - 1)
ozono$Day <- day(ozono$Data - 1)
ozono$Hour <- hour(ozono$Data - 1)

sensors <- unique(ozono$idSensore)
years <- unique((ozono$Year))
mesi <- 4:10

counts <- read.csv("./Datasets/Dataset_1")
#counts <- read.csv("./Datasets/Dataset_2")

time <- NULL
for (i in years)
{
    for (j in mesi)
    {
        time <- c(time, paste(i, j))
    }
}
mat <- matrix(rep(0, length(sensors) * length(time)), nrow = length(sensors), ncol = length(time))
for (s in seq_along(sensors))
{
    for (i in seq_along(years))
    {
        for (j in seq_along(mesi))
        {
            if (length(which(counts$idSensore == sensors[s] & counts$Year == years[i] & counts$Month == mesi[j]))) {
                mat[s, (i - 1) * length(mesi) + j] <- counts$Count[which(counts$idSensore == sensors[s] & counts$Year == years[i] & counts$Month == mesi[j])]
            } else {
                mat[s, (i - 1) * length(mesi) + j] <- NA
            }
        }
    }
}

matplot(t(mat), type = "l")
